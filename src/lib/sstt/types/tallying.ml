open Core
open Sstt_utils

let solve_rectype v ty =
  Ty.of_eqs [v, ty] |> List.hd |> snd
let solve_recfield v f =
  let module VHT = Hashtbl.Make(Var) in
  let nodes = VHT.create 10 in
  let f = f |> Ty.F.map_nodes (fun n ->
      let v = Var.mk "" in
      VHT.add nodes v n ; Ty.mk_var v
    )
  in
  let eqs = VHT.to_seq nodes |> List.of_seq |> List.map (fun (v',ty') ->
    v', Subst.apply (Subst.singleton2 v (Row.all_fields f)) ty'
  ) in
  let s = Ty.of_eqs eqs |> Subst.of_list1 in
  f |> Ty.F.map_nodes (fun n -> Subst.apply s n)

(* =============== Tallying algorithm =============== *)

type constr = Ty.t * Ty.t

module type VarSettings = sig
  val delta : MixVarSet.t
end
  
module Make(VS:VarSettings) = struct
  module type B = sig
    type t
    val is_mono : t -> bool
    val cap : t -> t -> t
    val cup : t -> t -> t
    val neg : t -> t
    val leq : t -> t -> bool
    val compare : t -> t -> int
  end
  module type V = sig
    type t
    module Set : Set.S with type elt=t
    val compare : t -> t -> int
    val delta : Set.t
  end

  module C(V:V)(B:B) = struct
    type t = B.t * V.t * B.t (* s ≤ α ≤ t *)
    module V = V
    module B = B

    (* C1 subsumes C2 if it has the same variable
       and gives better bounds (larger lower bound and smaller upper bound)
    *)
    let subsumes (t1, v1, t1') (t2, v2, t2') =
      V.compare v1 v2 = 0 &&
      B.leq t2 t1 && B.leq t1' t2'

    let compare (t1,v1,t1') (t2,v2,t2')=
      V.compare v1 v2 |> ccmp
        B.compare t1 t2 |> ccmp
        B.compare t1' t2'

    let unsat (s, _, t) =
      B.is_mono s && B.is_mono t && not (B.leq s t)
  end

  exception Unsat
  module CS(V:V)(B:B) = struct
    module V = V
    module B = B
    module C = C(V)(B)
    type t = [] | (::) of C.t * t

    let assert_sat c =
      if C.unsat c
      then raise_notrace Unsat
    let any = []
    let is_any t = (t = [])
    let singleton e = assert_sat e; [e]
    let merge (s, v, t) (s', _, t') =
      let ss = B.cup s s' in
      let tt = B.cap t t' in
      let merged = ss, v, tt in
      assert_sat merged ; merged

    let rec add ((_, v, _) as c) l =
      match l with
        [] -> [ c ]
      | ((_, v', _) as c') :: ll ->
        let n = V.compare v v' in
        if n < 0 then c::l
        else if n = 0 then (merge c c')::ll
        else c' :: add c ll

    let rec cap l1 l2 =
      match l1, l2 with
      | [],  _ -> l2
      | _, []  -> l1
      | ((_,v1, _) as c1)::ll1, ((_, v2, _) as c2)::ll2 ->
        let n = V.compare v1 v2 in
        if n < 0 then c1 :: cap ll1 l2
        else if n > 0 then c2 :: cap l1 ll2
        else (merge c1 c2)::cap ll1 ll2

    (* A constraint set l1 subsumes a constraint set l2 if
       forall constraint c2 in m2, there exists
       c1 in t1 such that c1 subsumes c2
    *)
    let rec subsumes l1 l2 =
      match l1, l2 with
      | _, [] -> true
      | [], _ -> false
      | ((_,v1, _) as c1)::ll1, ((_, v2, _) as c2)::ll2 ->
        let n = V.compare v1 v2 in
        if n < 0 then subsumes ll1 l2
        else if n > 0 then false
        else C.subsumes c1 c2 && subsumes ll1 ll2

    let rec compare l1 l2 =
      match l1, l2 with
        [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: ll1, c2 :: ll2 ->
        let c = C.compare c1 c2 in
        if c <> 0 then c else compare ll1 ll2

    let rec to_list_map f = function
        [] -> List.[]
      | e :: ll -> (f e)::to_list_map f ll
  end
  module TyB = struct
    include Ty
    let is_mono t = MixVarSet.subset (Ty.all_vars t) VS.delta
  end
  module TV = struct
    type t = Var.t
    let compare = Var.compare
    let delta = MixVarSet.proj1 VS.delta
    module Set = VarSet
  end
  module VCS = CS(TV)(TyB)
  module FTyB = struct
    include Ty.F
    let pack f = Row.all_fields f |> Row.to_record_atom |> Descr.mk_record |> Ty.mk_descr
    let is_mono f = MixVarSet.subset (pack f |> Ty.all_vars) VS.delta
    let leq f1 f2 = Ty.leq (pack f1) (pack f2)
  end
  module RV = struct
    type t = RowVar.t
    let compare = RowVar.compare
    let delta = MixVarSet.proj2 VS.delta
    module Set = RowVarSet
  end
  module FCS = CS(RV)(FTyB)

  module CS' = struct
    type t = VCS.t * FCS.t

    let any = (VCS.any, FCS.any)
    let is_any (vt,ft) = VCS.is_any vt && FCS.is_any ft
    let singleton e = (VCS.singleton e, FCS.any)
    let singleton' e = (VCS.any, FCS.singleton e)

    (* let add (vt,ft) e = (VCS.add vt e, ft)
    let add' (vt,ft) e = (vt, FCS.add ft e) *)

    let cap (vt1, ft1) (vt2, ft2) = (VCS.cap vt1 vt2, FCS.cap ft1 ft2)

    let subsumes (vt1, ft1) (vt2, ft2) =
      VCS.subsumes vt1 vt2 && FCS.subsumes ft1 ft2

    let compare (vt1, ft1) (vt2, ft2) =
      VCS.compare vt1 vt2 |> ccmp FCS.compare ft1 ft2
  end

  module CSS = struct
    (* Constraint sets are ordered list of non subsumable elements.
       They represent union of constraints, so we maintain the invariant
       that we don't want to add a constraint set that subsumes an already
       existing one.
    *)
    type t = CS'.t list
    let empty : t = []
    let is_empty = function [] -> true | _ -> false
    let any : t = [CS'.any]
    let is_any = function [t] when CS'.is_any t -> true | _ -> false
    let singleton (e:CS'.t) = [e]
    let single e = try singleton (CS'.singleton e) with Unsat -> empty
    let single' e = try singleton (CS'.singleton' e) with Unsat -> empty
    let rec insert_aux c l =
      match l with
        [] -> [c]
      | c' :: ll ->
        let n = CS'.compare c c' in
        if n < 0 then c::l
        else if n = 0 then l
        else c' :: insert_aux c ll
    let add c l =
      if List.exists (CS'.subsumes c) l then l
      else List.filter (fun c' -> CS'.subsumes c' c |> not) l |> insert_aux c

    let cup t1 t2 = List.fold_left (fun acc cs -> add cs acc) t1 t2
    let cap t1 t2 =
      (cartesian_product t1 t2)
      |> List.fold_left (fun acc (cs1,cs2) -> try add (CS'.cap cs1 cs2) acc with Unsat -> acc) empty

    let cup_lazy t1 t2 =
      if is_any t1 then any
      else cup t1 (t2 ())
    let cap_lazy t1 t2 =
      if is_empty t1 then empty
      else cap t1 (t2 ())

    let map_disj f t = List.fold_left (fun acc e -> cup_lazy acc (fun () -> f e))  empty t
    let map_conj f t = List.fold_left (fun acc e -> cap_lazy acc (fun () -> f e)) any t
    let to_list (l:t) = l
  end

  module type P = sig
    module V : V
    type descr
    type t
    val of_line : V.t list * V.t list * descr -> t
    val empty : t
    val any : t
    val neg : t -> t
  end
  module Toplevel(P:P) = struct

    let pos_var v e = (P.empty, v, P.neg (P.of_line e))

    let neg_var v e = (P.of_line e, v, P.any)

    (* Extract a constraint for the smallest polymorphic (not in delta) top-level variable of a summand *)
    let extract_smallest (pvs, nvs, d) =
      let rec find_min_var acc o_min l =
        match l, o_min with
        | [], None -> None
        | [], Some v -> Some (v, acc)
        | v :: ll, _ when P.V.Set.mem v P.V.delta -> find_min_var (v::acc) o_min ll
        | v :: ll, None -> find_min_var acc (Some v) ll
        | v :: ll, Some v_min ->
          if P.V.compare v v_min < 0 then
            find_min_var (v_min::acc) (Some v) ll
          else find_min_var (v :: acc) o_min ll
      in
      match find_min_var [] None pvs, find_min_var [] None nvs with
        None, None -> None
      | Some (v, rem_pos), None -> Some (pos_var v (rem_pos, nvs, d))
      | None, Some (v, rem_neg) -> Some (neg_var v (pvs, rem_neg, d))
      | Some (vp, rem_pos), Some (vn, rem_neg) ->
        if P.V.compare vp vn < 0 then
          Some (pos_var vp (rem_pos, nvs, d))
        else
          Some (neg_var vn (pvs, rem_neg, d))
  end
  module VP = struct
    include Ty
    module V = TV
    type descr = Descr.t
    let of_line line = VDescr.of_dnf [line] |> Ty.of_def
  end
  module VToplevel = Toplevel(VP)
  module FP = struct
    include Ty.F
    module V = RV
    type descr = Ty.O.t
    let of_line line = Ty.F.of_dnf [line]
  end

  module FToplevel = Toplevel(FP)
  module VDHash = Hashtbl.Make(VDescr)
  module FTyHash = Hashtbl.Make(Ty.F)
  
  let norm_tuple_gen ~diff ~disjoint ~norm ps ns =
    (* Same algorithm as for subtyping tuples.
       We define it outside norm below so that its type can be
       generalized and we can apply it to different ~any/~conj/...
    *)
    let rec psi acc ss ts () =
      let cstr = ss |> CSS.map_disj norm in
      CSS.cup_lazy cstr (fun () ->
        match ts with
          [] -> CSS.empty
        | tt :: ts ->
          if List.exists2 disjoint ss tt then psi acc ss ts ()
          else fold_distribute_comb (fun acc ss ->
              CSS.cap_lazy acc (psi acc ss ts)) diff acc ss tt
      )
    in psi CSS.any ps ns ()
  let norm, norm_field =
    let memo = VDHash.create 16 in
    let rec norm_ty t =
      let vd = Ty.def t in
      match VDHash.find_opt memo vd  with
      | Some cstr -> cstr
      | None ->
        VDHash.add memo vd CSS.any;
        let res =
          if Ty.is_empty t then CSS.any
          else if TyB.is_mono t then CSS.empty
          else vd |> VDescr.dnf |> CSS.map_conj norm_summand
        in
        VDHash.remove memo vd ; res
    and norm_summand summand =
      match VToplevel.extract_smallest summand with
      | None ->
        let (_,_,d) = summand in
        norm_descr d
      | Some cs -> CSS.single cs
    and norm_descr d =
      let (cs, others) = d |> Descr.components in
      if others then CSS.empty
      else cs |> CSS.map_conj norm_comp
    and norm_comp c =
      let open Descr in
      match c with
      | Enums c -> norm_enums c
      | Arrows c -> norm_arrows c
      | Intervals c -> norm_intervals c
      | Tags c -> norm_tags c
      | Tuples c -> norm_tuples c
      | Records c -> norm_records c
    and norm_enums d =
      match Enums.destruct d with
      | true, [] -> CSS.any
      | _, _ -> CSS.empty
    and norm_intervals d =
      match Intervals.destruct d with
      | [] -> CSS.any
      | _ -> CSS.empty
    and norm_tags tag =
      let (cs, others) = tag |> Tags.components in
      if others then CSS.empty
      else cs |> CSS.map_conj norm_tagcomp
    and norm_tagcomp c =
      let tag = TagComp.tag c in
      c |> TagComp.dnf |> CSS.map_conj (norm_tag tag)      
    and norm_arrows arr =
      arr |> Arrows.dnf |> CSS.map_conj norm_arrow
    and norm_tuples tup =
      let (comps, others) = tup |> Tuples.components in
      if others then CSS.empty
      else comps |> CSS.map_conj norm_tuplecomp
    and norm_tuplecomp tup =
      let n = TupleComp.len tup in
      tup |> TupleComp.dnf |> CSS.map_conj (norm_tuple n)
    and norm_records r =
      r |> Records.dnf |> CSS.map_conj norm_record
    and norm_arrow (ps, ns) =
      let rec psi t1 t2 ps () =
        let cstr = CSS.cup_lazy (norm_ty t1) (fun () -> norm_ty t2) in
        let cstr_rec () = match ps with
            [] -> CSS.empty
          | (s1, s2) :: ps ->
            if Ty.disjoint t1 s1 || Ty.leq t2 s2 then psi t1 t2 ps ()
            else CSS.cap_lazy
              (psi (Ty.diff t1 s1) t2 ps ())
              (psi t1 (Ty.cap t2 s2) ps)
        in
        CSS.cup_lazy cstr cstr_rec
      in
      let norm_single_neg_arrow ps (t1, t2) =
        let cstr_domain = Ty.diff t1 (List.map fst ps |> Ty.disj) |> norm_ty in
        if CSS.is_empty cstr_domain then CSS.empty
        else
          let cstr_struct () =
            if List.is_empty ps then CSS.any else psi t1 (Ty.neg t2) ps () in
          CSS.cap_lazy cstr_domain cstr_struct
      in
      CSS.map_disj (norm_single_neg_arrow ps) ns
    and norm_tuple n (ps,ns) =
      let ps = mapn (fun () -> List.init n (fun _ -> Ty.any)) Ty.conj ps in
      norm_tuple_gen ~diff:Ty.diff ~disjoint:Ty.disjoint ~norm:norm_ty ps ns
    and norm_tag tag line =
      let tys = TagComp.line_emptiness_checks tag line in
      CSS.map_disj norm_ty tys
    and norm_record (ps, ns) =
      let (tl,p), ns = Records.dnf_line_to_types (ps, ns) in
      CSS.cup_lazy (norm_field tl)
        (fun () -> norm_record_tests (tl,p) [] ns)
    and norm_record_tests (tl,p) ns ns' =
      match ns' with
      | [] -> norm_record_bindings p ns
      | (tl',bs')::ns' ->
        CSS.cup_lazy (norm_record_tests (tl,p) ns ns') (fun () ->
          CSS.cap_lazy (Ty.F.cap tl (Ty.F.neg tl') |> norm_field)
            (fun () -> norm_record_tests (tl,p) (bs'::ns) ns')
        )
    and norm_record_bindings p ns =
      let disjoint s1 s2 =
        let o = Ty.F.cap s1 s2 |> Ty.F.get_descr in
        Ty.O.is_required o && Ty.O.get o |> Ty.is_empty
      in
      norm_tuple_gen ~diff:Ty.F.diff ~disjoint ~norm:norm_field p ns
    and norm_field (f:Ty.F.t) =
      f |> Ty.F.dnf |> CSS.map_conj norm_field_summand
    and norm_field_summand summand =
      match FToplevel.extract_smallest summand with
      | None ->
        let (_,_,oty) = summand in
        norm_oty oty
      | Some cs -> CSS.single' cs
    and norm_oty (n,o) =
      if o then CSS.empty else norm_ty n
    in
    norm_ty, norm_field

  let propagate cs =
    let memo_ty = VDHash.create 8 in
    let memo_f = FTyHash.create 8 in
    let rec aux (prev,prev') ((cs,cs') : CS'.t) =
      let retry_with css =
        let css' () = CS'.cap (prev,prev') (cs,cs') |> CSS.singleton in
        let css = CSS.cap_lazy css css' in
        css |> CSS.to_list |> CSS.map_disj (aux CS'.any)
      in
      match cs,cs' with
      | VCS.[], FCS.[] -> (prev,prev') |> CSS.singleton
      | ((t',_, t) as constr) :: tl, cs' ->
        let ty = Ty.diff t' t in
        if VDHash.mem memo_ty (Ty.def ty) then
          aux (VCS.add constr prev, prev') (tl,cs')
        else
          let () = VDHash.add memo_ty (Ty.def ty) () in
          let res = norm ty |> retry_with in
          VDHash.remove memo_ty (Ty.def ty); res
      | VCS.[], ((f',_, f) as constr) :: tl ->
        let f = Ty.F.diff f' f in
        if FTyHash.mem memo_f f then
          aux (prev, FCS.add constr prev') (cs,tl)
        else
          let () = FTyHash.add memo_f f () in
          let res = norm_field f |> retry_with in
          FTyHash.remove memo_f f; res
    in
    aux CS'.any cs

  let solve (cs, cs' : VCS.t * FCS.t) =
    let renaming = ref Subst.identity in
    let to_eq (ty1, v, ty2) =
      let v' = Var.mk (Var.name v) in
      renaming := Subst.add1 v' (Ty.mk_var v) !renaming ;
      (v, Ty.cap (Ty.cup ty1 (Ty.mk_var v')) ty2)
    in
    let to_eq' (f1, v, f2) =
      let v' = RowVar.mk (RowVar.name v) in
      renaming := Subst.add2 v' (Row.id_for v) !renaming ;
      (v, Ty.F.cap (Ty.F.cup f1 (Ty.F.mk_var v')) f2)
    in
    let rec unify eqs1 eqs2 =
      match eqs1, eqs2 with
      | [], [] -> Subst.identity
      | (v,ty)::eqs1, eqs2 ->
        let ty' = solve_rectype v ty in
        let s = Subst.singleton1 v ty' in
        let eqs1' = eqs1 |> List.map (fun (v,eq) -> (v, Subst.apply s eq)) in
        let eqs2' = eqs2 |> List.map (fun (v,eq) -> (v, Row.tail (Subst.apply_to_row s (Row.all_fields eq)))) in
        let res = unify eqs1' eqs2' in
        Subst.add1 v (Subst.apply res ty') res
      | [], (v,f)::eqs2 ->
        let f' = solve_recfield v f |> Row.all_fields in
        let s = Subst.singleton2 v f' in
        let eqs1' = eqs1 |> List.map (fun (v,eq) -> (v, Subst.apply s eq)) in
        let eqs2' = eqs2 |> List.map (fun (v,eq) -> (v, Row.tail (Subst.apply_to_row s (Row.all_fields eq)))) in
        let res = unify eqs1' eqs2' in
        Subst.add2 v (Subst.apply_to_row res f') res
    in
    let eqs1 = VCS.to_list_map to_eq cs in
    let eqs2 = FCS.to_list_map to_eq' cs' in
    unify eqs1 eqs2
    |> Subst.map1 (Subst.apply !renaming)
    |> Subst.map2 (Subst.apply_to_row !renaming)

  let tally cs =
    let ncss = cs |> CSS.map_conj (fun (s,t) -> norm (Ty.diff s t)) in
    let mcss = ncss |> CSS.to_list |> CSS.map_disj propagate in
    mcss |> CSS.to_list |> List.map solve
end

(* =============== Operations on row and field variables =============== *)

let labels_of_ty t =
  let labels = ref LabelSet.empty in
  let _ = Ty.nodes t |> List.iter (fun n ->
      Ty.def n |> VDescr.map (fun d ->
        let _ = d |> Descr.get_records |> Records.map (fun r ->
            labels := LabelSet.union !labels (Records.Atom.dom r) ; r
        ) in d
      ) |> ignore
    ) in !labels
let labels_of_tys tys = tys
  |> List.map labels_of_ty
  |> List.fold_left LabelSet.union LabelSet.empty
let rvs_of_tys tys = tys
  |> List.map Ty.row_vars
  |> List.fold_left RowVarSet.union RowVarSet.empty
module RVH = Hashtbl.Make(RowVar)
type field_ctx = Subst.t * Subst.t
let get_field_ctx' labels rvs =
  (* Substitute row variables with "field variables" *)
  let labels = LabelSet.elements labels in
  let original_rv = RVH.create 10 in
  let s, rs = rvs |> RowVarSet.elements |> List.map (fun rv ->
    let bindings = labels |> List.map (fun lbl ->
        let rv' = RowVar.mk (RowVar.name rv) in
        RVH.add original_rv rv' rv ;
        lbl, rv'
      ) in
    (rv, Row.mk (List.map (fun (lbl, rv') -> lbl, Ty.F.mk_var rv') bindings) (Ty.F.mk_var rv)),
    (List.map (fun (_, rv') -> rv', Row.id_for rv) bindings)
  ) |> List.split in
  Subst.of_list2 s, List.concat rs |> Subst.of_list2
let get_field_ctx delta tys =
  let rvs = RowVarSet.diff (rvs_of_tys tys) delta in
  get_field_ctx' (labels_of_tys tys) rvs
let decorrelate_fields (s,_) ty = Subst.apply s ty
let recombine_fields (_,rs) ty = Subst.apply rs ty
let recombine_fields' (s,rs) sol =
  Subst.compose sol s |> Subst.remove_many2 (Subst.intro2 s) |> Subst.compose_restr rs
let fvars_associated_with (s,_) rv = Subst.find2 s rv |> Row.row_vars_toplevel
let fvar_associated_with (s,_) (rv,lbl) =
  Subst.find2 s rv |> Row.find lbl |> Ty.F.get_vars |> RowVarSet.elements |> List.hd
let rvar_associated_with (_,rs) rv =
  match Subst.find2 rs rv |> Row.bindings with
  | [lbl,f] -> Some (Ty.F.get_vars f |> RowVarSet.elements |> List.hd, lbl)
  | _ -> None

(* =============== Exported functions =============== *)

let tally_fields delta cs =
  let module Tallying = Make(struct let delta = delta end) in
  Tallying.tally cs

let tally delta cs =
  let frc = cs |> List.concat_map (fun (t1,t2) -> [t1;t2]) |> get_field_ctx (MixVarSet.proj2 delta) in
  cs |> List.map (fun (t1,t2) -> decorrelate_fields frc t1, decorrelate_fields frc t2)
  |> tally_fields delta |> List.map (recombine_fields' frc)

let decompose delta s1 s2 =
  let union_many = List.fold_left MixVarSet.union MixVarSet.empty in
  let vars = union_many
    [Subst.domain s1 ; Subst.intro s1 ; Subst.domain s2 ; Subst.intro s2 ] in
  let fresh, fresh_inv = Subst.refresh (MixVarSet.diff vars delta) in
  let fresh_vars = Subst.intro fresh in
  let s2 = Subst.compose fresh s2 in
  let cs = MixVarSet.elements1 vars |> List.concat_map (fun v ->
      let t1, t2 = Subst.find1 s1 v, Subst.find1 s2 v in
      [ t1, t2 ; t2, t1 ]
    )
  in
  let cs' = MixVarSet.elements2 vars |> List.concat_map (fun v ->
      let r1, r2 = Subst.find2 s1 v, Subst.find2 s2 v in
      Row.equiv_constraints r1 r2
    )
  in
  tally (MixVarSet.union delta fresh_vars) (cs@cs')
  |> List.map (fun s -> Subst.compose fresh_inv s |> Subst.restrict vars)
