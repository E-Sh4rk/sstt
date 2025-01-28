open Sstt_core
open Sstt_utils

type constr = Ty.t * Ty.t
type norm_constr = Left of Var.t * Ty.t | Right of Ty.t * Var.t
type merged_constr = Ty.t * Var.t * Ty.t

module type Constr = sig
  type t
  val leq : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module NormConstr = struct
  type t = norm_constr
  let leq t1 t2 =
    match t1, t2 with
    | Left (v1,t1), Left (v2,t2) when Var.equal v1 v2 -> Ty.leq t1 t2
    | Right (t1,v1), Right (t2,v2) when Var.equal v1 v2 -> Ty.leq t2 t1
    | _, _ -> false
  let compare c1 c2 =
    let var = function Left (v,_) -> v | Right (_,v) -> v in
    let ty = function Left (_,ty) -> ty | Right (ty,_) -> ty in
    let cmp_constructor c1 c2 =
      match c1, c2 with
      | Left _, Left _ | Right _, Right _ -> 0
      | Left _, Right _ -> -1
      | Right _, Left _ -> 1
    in
    Var.compare (var c1) (var c2) |> ccmp
    cmp_constructor c1 c2 |> ccmp
    Ty.compare (ty c1) (ty c2)
  let pp fmt t =
    match t with
    | Left (v,ty) -> Format.fprintf fmt "%a <= %a" Var.pp v Printer.print_ty' ty
    | Right (ty,v) -> Format.fprintf fmt "%a <= %a" Printer.print_ty' ty Var.pp v
end

module MergedConstr = struct
  type t = merged_constr
  let leq (t1,v1,t1') (t2,v2,t2') =
    Var.equal v1 v2 &&
      Ty.leq t2 t1 && Ty.leq t1' t2'
  let compare (t1,v1,t1') (t2,v2,t2')=
    Var.compare v1 v2 |> ccmp
    Ty.compare t1 t2 |> ccmp
    Ty.compare t1' t2'
  let pp fmt (ty,v,ty') =
    Format.fprintf fmt "%a <= %a <= %a"
      Printer.print_ty' ty Var.pp v Printer.print_ty' ty'
end

module ConstrSet(C:Constr) = struct
  module CS = Set.Make(C)
  type t = CS.t
  let any : t = CS.empty
  let singleton = CS.singleton
  let normalize t =
    let cs = CS.to_list t in
    let cs = cs |> filter_among_others (fun c cs ->
      cs |> List.exists (fun c' -> C.leq c' c) |> not
      ) in
    CS.of_list cs
  let to_list = CS.to_list
  let of_list lst = CS.of_list lst |> normalize
  let add e t = CS.add e t |> normalize
  let cap t1 t2 = CS.union t1 t2 |> normalize
  let leq t1 t2 = t2 |> CS.for_all (fun cs2 ->
    t1 |> CS.exists (fun cs1 -> C.leq cs1 cs2)
    )
  let compare = CS.compare
  (* let pp fmt t = Format.fprintf fmt "%a" (print_seq C.pp " ; ") (CS.to_list t) *)
end

module ConstrSets(C:Constr) = struct
  module CS = ConstrSet(C)
  module CSS = Set.Make(CS)
  type t = CSS.t
  let empty : t = CSS.empty
  let any : t = CSS.singleton CS.any
  let singleton = CSS.singleton
  let normalize t =
    let css = CSS.to_list t in
    let css = css |> filter_among_others (fun cs css ->
      css |> List.exists (fun cs' -> CS.leq cs cs') |> not
      ) in
    CSS.of_list css
  let cup t1 t2 = CSS.union t1 t2 |> normalize
  let cap t1 t2 =
    let t1, t2 = CSS.elements t1, CSS.elements t2 in
    carthesian_product t1 t2 |> List.map (fun (cs1,cs2) -> CS.cap cs1 cs2)
    |> CSS.of_list |> normalize
  let disj t = List.fold_left cup empty t |> normalize
  let conj t = List.fold_left cap any t |> normalize
  let to_list = CSS.to_list
  (* let pp fmt t =
    Format.fprintf fmt "%a" (print_seq_cut CS.pp) (CSS.to_list t) *)
end

module NCSS = ConstrSets(NormConstr)
module NCS = NCSS.CS
module MCSS = ConstrSets(MergedConstr)
module MCS = MCSS.CS

module Summand = struct
  type t = VarSet.t * VarSet.t * Descr.t

  let of_line (pvs, nvs, d) : t =
    (VarSet.of_list pvs, VarSet.of_list nvs, d)

  let to_ty (pvs, nvs, d) =
    [(pvs |> VarSet.to_list, nvs |> VarSet.to_list, d)] |> VDescr.of_dnf |> Ty.of_def

  let vars (pvs, nvs, _) = VarSet.union pvs nvs

  let single v (pvs, nvs, d) =
    if VarSet.mem v pvs then
      let t = (VarSet.remove v pvs, nvs, d) |> to_ty in
      Left (v, Ty.neg t) |> NCS.singleton
    else if VarSet.mem v nvs then
      let t = (pvs, VarSet.remove v nvs, d) |> to_ty in
      Right (t, v) |> NCS.singleton
    else
      assert false
end

module VDSet = Set.Make(VDescr)

let norm delta t =
  let rec aux m t =
    let vd = Ty.def t in
    if VDSet.mem vd m then NCSS.any
    else
      let m = VDSet.add vd m in
      let summands = vd |> VDescr.dnf |> VDescr.Dnf.simplify |> List.map Summand.of_line in
      summands |> List.map (aux_summand m) |> NCSS.conj
  and aux_summand m summand =
    match VarSet.diff (Summand.vars summand) delta |> VarSet.elements with
    | [] ->
      let (_,_,d) = summand in
      aux_descr m d
    | v::_ -> Summand.single v summand |> NCSS.singleton
  and aux_descr m d =
    Descr.components d |> List.map (aux_comp m) |> NCSS.conj
  and aux_comp m c =
    let open Descr in
    match c with
    | Atoms c -> aux_atoms m c
    | Arrows c -> aux_arrows m c
    | Intervals c -> aux_intervals m c
    | Tags c -> aux_tags m c
    | Tuples c -> aux_tuples m c
    | Records c -> aux_records m c
  and aux_atoms _ d =
    match Atoms.destruct d with
    | true, [] -> NCSS.any
    | _, _ -> NCSS.empty
  and aux_intervals _ d =
    match Intervals.destruct d with
    | [] -> NCSS.any
    | _ -> NCSS.empty
  and aux_tags m tag =
    let (cs, others) = tag |> Tags.components in
    if others then NCSS.empty
    else cs |>
      List.map (fun c -> TagComp.as_atom c |> snd |> aux m)
      |> NCSS.conj
  and aux_arrows m arr =
    arr |> Arrows.dnf |> Arrows.Dnf.simplify
    |> List.map (aux_arrow m) |> NCSS.conj
  and aux_tuples m tup =
    let (comps, others) = tup |> Tuples.components in
    if others then NCSS.empty
    else comps |> List.map (aux_tuplecomp m) |> NCSS.conj
  and aux_tuplecomp m tup =
    let n = TupleComp.len tup in
    tup |> TupleComp.dnf |> TupleComp.Dnf.simplify
    |> List.map (aux_tuple m n) |> NCSS.conj
  and aux_records m r =
    r |> Records.dnf |> Records.Dnf.simplify
    |> List.map (aux_record m) |> NCSS.conj
  and aux_arrow m (ps, ns, _) =
    let pt, _ = List.split ps in
    let dom = Ty.disj pt in
    let aux_n (nt,nt') =
      let css1 = Ty.cap nt (Ty.neg dom) |> aux m in
      let aux_p (p1, p2) =
        if p2 = [] then NCSS.any else
          let pt1, _ = List.split p1 in
          let dom1 = Ty.disj pt1 in
          let _, pt2' = List.split p2 in
          let codom2 = Ty.conj pt2' in
          let css1 = Ty.cap nt (Ty.neg dom1) |> aux m in
          let css2 = Ty.cap codom2 (Ty.neg nt') |> aux m in
          NCSS.cup css1 css2
      in
      let css2 = subsets ps |> List.map aux_p |> NCSS.conj in
      NCSS.cap css1 css2
    in
    ns |> List.map aux_n |> NCSS.disj
  and aux_tuple m n (ps, ns, _) =
    let ps = mapn (fun () -> List.init n (fun _ -> Ty.any)) Ty.conj ps in
    let aux_n nss =
      let csss = nss |> List.mapi (fun i ns ->
        let pcomp = List.nth ps i in
        let ncomp = ns |> List.map (fun ns -> List.nth ns i) |> Ty.disj |> Ty.neg in
        Ty.cap pcomp ncomp |> aux m
      ) in
      NCSS.disj csss
    in
    ns |> partitions n |> List.map aux_n |> NCSS.conj
  and aux_record m (ps, ns, _) =
    let open Records in
    let open Atom in
    let dom = List.fold_left
      (fun acc a -> LabelSet.union acc (dom a))
      LabelSet.empty (ps@ns) |> LabelSet.to_list in
    let ps, ns =
      ps |> List.map (to_tuple_with_default dom),
      ns |> List.map (to_tuple_with_default dom) in
    let n = List.length dom + 1 in
  (* We reuse the same algorithm as for tuples *)
    let ps = mapn (fun () -> List.init n (fun _ -> OTy.any ())) OTy.conj ps in
    let aux_n nss =
      let csss = nss |> List.mapi (fun i ns ->
        let pcomp = List.nth ps i in
        let ncomp = ns |> List.map (fun ns -> List.nth ns i) |> OTy.disj |> OTy.neg in
        OTy.cap pcomp ncomp |> aux_oty m
      ) in
      NCSS.disj csss
    in
    ns |> partitions n |> List.map aux_n |> NCSS.conj
  and aux_oty m (n,o) =
    if o then NCSS.empty else aux m n
  in
  aux (VDSet.empty) t

(* TODO: Normalize using psi, in the same way as for subtyping *)

let merge delta cs =
  let rec merge m cs =
    cs |> NCS.to_list |> step1 |> step2 m []
  and step1 cs =
    match cs with
    | [] -> []
    | (Left (v,t))::(Left (v',t'))::cs when Var.equal v v' ->
      (Left (v, Ty.cap t t'))::cs |> step1
    | (Right (t,v))::(Right (t',v'))::cs when Var.equal v v' ->
      (Right (Ty.cup t t', v))::cs |> step1
    | hd::tl -> hd::(step1 tl)
  and step2 m prev cs =
    match cs with
    | [] -> NCS.of_list prev |> NCSS.singleton
    | (Left (v,t))::(Right (t',v'))::cs when Var.equal v v' ->
      let ty = Ty.diff t' t in
      if VDSet.mem (Ty.def ty) m then
        step2 m ((Right (t',v'))::(Left (v,t))::prev) cs
      else
        let m = VDSet.add (Ty.def ty) m in
        let css = norm delta ty in
        let css' = (Left (v,t))::(Right (t',v'))::cs@prev |> NCS.of_list |> NCSS.singleton in
        let css = NCSS.cap css css' in
        css |> NCSS.to_list |> List.map (merge m) |> NCSS.disj
    | hd::cs -> step2 m (hd::prev) cs
  in
  let regroup cs =
    let rec aux cs =
      match cs with
      | [] -> MCS.any
      | (Left (v,t))::(Right (t',v'))::cs when Var.equal v v' ->
          MCS.add (t', v, t) (aux cs)
      | (Left (v,t))::cs -> MCS.add (Ty.empty, v, t) (aux cs)
      | (Right (t,v))::cs -> MCS.add (t, v, Ty.any) (aux cs)
    in
    NCS.to_list cs |> aux |> MCSS.singleton
  in
  merge (VDSet.empty) cs |> NCSS.to_list |> List.map regroup |> MCSS.disj

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
  cs |> MCS.to_list |> List.map to_eq |> unify |> Subst.map (Subst.apply !renaming)

let tally delta cs =
  let ncss = cs
    |> List.map (fun (s,t) -> norm delta (Ty.diff s t)) |> NCSS.conj in
  let mcss = ncss
    |> NCSS.to_list |> List.map (merge delta) |> MCSS.disj in
  mcss |> MCSS.to_list |> List.map solve
