open Core
open Sstt_utils

module type VarOrder = sig
  val compare : Var.t -> Var.t -> int
end

type constr = Ty.t * Ty.t


module Make(VO:VarOrder) = struct
  module Var = struct
    include Var
    (* prevent from using default comparison*)
    let equal (_:t) (_:t) = () [@@ocaml.warning "-32"]
    let compare (_:t) (_:t) = () [@@ocaml.warning "-32"]
  end

  module Constr = struct
    type t = Ty.t * Var.t * Ty.t (* s ≤ α ≤ t *)

    (* C1 subsumes C2 if it has the same variable
       and gives better bounds (larger lower bound and smaller upper bound)
    *)
    let subsumes (t1, v1, t1') (t2, v2, t2') =
      VO.compare v1 v2 = 0 &&
      Ty.leq t2 t1 && Ty.leq t1' t2'

    let compare (t1,v1,t1') (t2,v2,t2')=
      VO.compare v1 v2 |> ccmp
        Ty.compare t1 t2 |> ccmp
        Ty.compare t1' t2'
  end

  (* As in CDuce, we follow POPL'15 but keep constraint merged:
     - A Constraint Set C, is a (sorted list of triples) (s, α, t)
     - Adding a new constraint for an existing variable merges them.
     - If such a constraint is trivially unsatifiable (because s and
       t are both monomorphic and s ≤ t does not hold), then
       a failure is returned
  *)
  module CS = struct

    type t = [] | (::) of Constr.t * t

    exception Unsat

    let assert_sat delta s t =
      if VarSet.subset (Ty.vars s) delta &&
         VarSet.subset (Ty.vars t) delta &&
         not (Ty.leq s t)
      then raise_notrace Unsat
    let any = []
    let singleton delta ((s, _, t) as e) = assert_sat delta s t; [e]
    let merge delta (s, v, t) (s', _, t') =
      let ss = Ty.cup s s' in
      let tt = Ty.cap t t' in
      assert_sat delta ss tt;
      (ss, v, tt)

    let rec add delta ((_, v, _) as c) l =
      match l with
        [] -> [ c ]
      | ((_, v', _) as c') :: ll ->
        let n = VO.compare v v' in
        if n < 0 then c::l
        else if n = 0 then (merge delta c c')::ll
        else c' :: add delta c ll

    let rec cap delta l1 l2 =
      match l1, l2 with
      | [],  _ -> l2
      | _, []  -> l1
      | ((_,v1, _) as c1)::ll1, ((_, v2, _) as c2)::ll2 ->
        let n = VO.compare v1 v2 in
        if n < 0 then c1 :: cap delta ll1 l2
        else if n > 0 then c2 :: cap delta l1 ll2
        else (merge delta c1 c2)::cap delta ll1 ll2

    (* A constraint set l1 subsumes a constraint set l2 if
       forall constraint c2 in m2, there exists
       c1 in t1 such that c1 subsumes c2
    *)
    let rec subsumes l1 l2 =
      match l1, l2 with
      | _, [] -> true
      | [], _ -> false
      | ((_,v1, _) as c1)::ll1, ((_, v2, _) as c2)::ll2 ->
        let n = VO.compare v1 v2 in
        if n < 0 then subsumes ll1 l2
        else if n > 0 then false
        else Constr.subsumes c1 c2 && subsumes ll1 ll2

    let rec compare l1 l2 =
      match l1, l2 with
        [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: ll1, c2 :: ll2 ->
        let c = Constr.compare c1 c2 in
        if c <> 0 then c else compare ll1 ll2

    let rec to_list_map f = function
        [] -> List.[]
      | e :: ll -> (f e)::to_list_map f ll
  end

  module CSS = struct
    (* Constraint sets are ordered list of non subsumable elements.
       They represent union of constraints, so we maintain the invariant
       that we don't want to add a constraint set that subsumes an already
       existing one.
    *)
    type t = CS.t list
    let empty : t = []
    let is_empty = function [] -> true | _ -> false
    let any : t = [CS.any]
    let singleton e = [e]
    let single delta e = try singleton (CS.singleton delta e) with CS.Unsat -> empty
    let rec insert_aux c l =
      match l with
        [] -> [c]
      | c' :: ll ->
        if CS.subsumes c c' then raise Exit
        else
          let n = CS.compare c c' in
          if n < 0 then (if List.exists (CS.subsumes c) ll then raise Exit else c::l)
          else if n = 0 then l
          else c' :: insert_aux c ll

    let add c l = try insert_aux c l with Exit -> l

    let cup t1 t2 = List.fold_left (fun acc cs -> add cs acc) t1 t2
    let cap delta t1 t2 =
      (cartesian_product t1 t2)
      |> List.fold_left (fun acc (cs1,cs2) -> try add (CS.cap delta cs1 cs2) acc with CS.Unsat -> acc) empty

    let cap_lazy delta t1 t2 =
      if is_empty t1 then empty
      else cap delta t1 (t2 ())

    let disj t = List.fold_left cup empty t
    let map_conj delta f t = List.fold_left (fun acc e -> cap delta (f e) acc) any t
    let to_list l = l
  end

  module Toplevel = struct
    (* Extract the smallest polymorphic (not in delta) top-level variable of a type *)
    let to_ty e = [ e ] |> VDescr.of_dnf |> Ty.of_def

    let pos_var v e = (Ty.empty, v, Ty.neg (to_ty e))

    let neg_var v e = (to_ty e, v, Ty.any)

    let extract_smallest delta (pvs, nvs, d) =
      let rec find_min_var acc o_min l =
        match l, o_min with
        | [], None -> None
        | [], Some v -> Some (v, acc)
        | v :: ll, _ when VarSet.mem v delta -> find_min_var (v::acc) o_min ll
        | v :: ll, None -> find_min_var acc (Some v) ll
        | v :: ll, Some v_min ->
          if VO.compare v v_min < 0 then
            find_min_var (v_min::acc) (Some v) ll
          else find_min_var (v :: acc) o_min ll
      in
      match find_min_var [] None pvs, find_min_var [] None nvs with
        None, None -> None
      | Some (v, rem_pos), None -> Some (pos_var v (rem_pos, nvs, d))
      | None, Some (v, rem_neg) -> Some (neg_var v (pvs, rem_neg, d))
      | Some (vp, rem_pos), Some (vn, rem_neg) ->
        if VO.compare vp vn < 0 then
          Some (pos_var vp (rem_pos, nvs, d))
        else
          Some (neg_var vn (pvs, rem_neg, d))

  end

  module VDHash = Hashtbl.Make (VDescr)
  let norm_tuple_gen ~any ~conj ~diff ~norm delta n (ps, ns, _) =
    (* Same algorithm as for subtyping tuples.
       We define it outside norm below so that its type can be
       generalized and we can apply it to different ~any/~conj/...
    *)
    let ps = mapn (fun () -> List.init n (fun _ -> any)) conj ps in
    let rec psi acc ss ts () =
      List.map norm ss |> CSS.disj
      |> CSS.cup (
        match ts with
          [] -> CSS.empty
        | tt :: ts ->
          fold_distribute_comb (fun acc ss ->
              CSS.cap_lazy delta acc (psi acc ss ts)) diff acc ss tt
      )
    in psi CSS.any ps ns ()
  let norm memo delta t =
    let rec norm_ty t =
      let vd = Ty.def t in
      match VDHash.find_opt memo vd  with
      | Some cstr -> cstr
      | None ->
        VDHash.add memo vd CSS.any;
        let res =
          if VarSet.subset (Ty.vars t) delta then
            (* Optimisation: performing tallying on an expression with
               no polymorphic type variable should be as fast as subtyping. *)
            begin if Ty.is_empty t then CSS.any else CSS.empty end
          else
            let summands = vd |> VDescr.dnf |> VDescr.Dnf.simplify in
            summands |> CSS.map_conj delta norm_summand
        in
        VDHash.replace memo vd res; res
    and norm_summand summand =
      match Toplevel.extract_smallest delta summand with
      | None ->
        let (_,_,d) = summand in
        norm_descr d
      | Some cs -> CSS.single delta cs
    and norm_descr d  =
      Descr.components d |> CSS.map_conj delta norm_comp
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
      else cs |>
           CSS.map_conj delta (fun c -> TagComp.as_atom c |> snd |> norm_ty)
    and norm_arrows arr =
      arr |> Arrows.dnf |> Arrows.Dnf.simplify
      |> CSS.map_conj delta norm_arrow
    and norm_tuples tup =
      let (comps, others) = tup |> Tuples.components in
      if others then CSS.empty
      else comps |> CSS.map_conj delta norm_tuplecomp
    and norm_tuplecomp tup =
      let n = TupleComp.len tup in
      tup |> TupleComp.dnf |> TupleComp.Dnf.simplify
      |> CSS.map_conj delta (norm_tuple n)
    and norm_records r =
      r |> Records.dnf |> Records.Dnf.simplify
      |> CSS.map_conj delta norm_record
    and norm_arrow (ps, ns, _ ) =
      let rec psi t1 t2 ps () =
        if Ty.is_empty t1 || Ty.is_empty t2 then CSS.any else
          let cstr_1 = norm_ty t1 in
          let cstr_2 = norm_ty t2 in
          let cstr_rec = match ps with
              [] -> CSS.empty
            | (s1, s2) :: ps ->
              CSS.cap_lazy delta
                (psi (Ty.diff t1 s1) t2 ps ())
                (psi t1 (Ty.cap t2 s2) ps)
          in
          CSS.cup cstr_1 (CSS.cup cstr_2 cstr_rec)
      in
      let norm_single_neg_arrow ps (t1, t2) =
        let cstr_domain = Ty.diff t1 (List.map fst ps |> Ty.disj) |> norm_ty in
        if CSS.is_empty cstr_domain then CSS.empty
        else
          let cstr_struct () =
            if List.is_empty ps then CSS.any else psi t1 (Ty.neg t2) ps () in
          CSS.cap_lazy delta cstr_domain cstr_struct
      in
      List.map (norm_single_neg_arrow ps) ns |> CSS.disj
    and norm_tuple n line = norm_tuple_gen ~any:Ty.any ~conj:Ty.conj
        ~diff:Ty.diff ~norm:norm_ty delta n line
    and norm_record (ps, ns, b) =
      let line, n = Records.dnf_line_to_tuple (ps, ns, b) in
      norm_tuple_gen ~any:Ty.O.any ~conj:Ty.O.conj
        ~diff:Ty.O.diff ~norm:norm_oty delta n line
    and norm_oty (n,o) =
      if o then CSS.empty else norm_ty n
    in
    norm_ty t

  let merge memo delta cs =
    (* Step1 from the paper is useless, since the ConstrSet maintains
       merged constraints
    *)
    let memo_ty = VDHash.create 8 in
    let rec step2 prev (cs : CS.t) =
      match cs with
      | CS.[] -> prev |> CSS.singleton
      | ((t',_, t) as constr) :: cs' ->
        let ty = Ty.diff t' t in
        if VDHash.mem memo_ty (Ty.def ty) then
          step2 (CS.add delta constr prev) cs'
        else
          let () = VDHash.add memo_ty (Ty.def ty) () in
          let css = norm memo delta ty in
          let css' () = cs |> CS.cap delta prev |> CSS.singleton in
          let css = CSS.cap_lazy delta css css' in
          let res = css |> CSS.to_list |> List.map (step2 CS.any) |> CSS.disj in
          VDHash.remove memo_ty (Ty.def ty); res
    in
    step2 CS.any cs

  let solve cs =
    let renaming = ref Subst.identity in
    let to_eq (ty1, v, ty2) =
      let v' = Var.mk (Var.name v) in
      renaming := Subst.add v' (Ty.mk_var v) !renaming ;
      (v, Ty.cap (Ty.cup ty1 (Ty.mk_var v')) ty2)
    in
    let rec unify eqs =
      match eqs with
      | [] -> Subst.identity
      | (v,ty)::eqs ->
        let (_,ty') = Ty.of_eqs [v, ty] |> List.hd in
        let s = Subst.singleton v ty' in
        let eqs' = eqs |> List.map (fun (v,eq) -> (v, Subst.apply s eq)) in
        let res = unify eqs' in
        Subst.add v (Subst.apply res ty') res
    in
    cs |> CS.to_list_map to_eq |> unify |> Subst.map (Subst.apply !renaming)

  let tally delta cs =
    let delta = VarSet.of_list delta in
    let memo = VDHash.create 16 in
    let ncss = cs
               |> CSS.map_conj delta (fun (s,t) -> norm memo delta (Ty.diff s t)) in
    let mcss = ncss
               |> CSS.to_list |> List.map (merge memo delta) |> CSS.disj in
    mcss |> CSS.to_list |> List.map solve
end

module Tallying = Make(Var)

let tally delta =
  Tallying.tally (VarSet.elements delta)
let tally_with_order cmp delta =
  let module Tallying = Make(struct let compare = cmp end) in
  Tallying.tally (VarSet.elements delta)
let tally_with_priority preserve =
  let cnt = ref 0 in
  let pmap = List.fold_left
      (fun acc v -> cnt := !cnt + 1 ; VarMap.add v !cnt acc)
      VarMap.empty preserve
  in
  let cmp v1 v2 =
    match VarMap.find_opt v1 pmap, VarMap.find_opt v2 pmap with
    | None, None -> Var.compare v1 v2
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some i1, Some i2 -> compare i2 i1
  in
  tally_with_order cmp
