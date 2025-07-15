open Core
open Sstt_utils

module type VarOrder = sig
  val compare : Var.t -> Var.t -> int
end

type constr = Ty.t * Ty.t

module Make(VO:VarOrder) = struct
  module Var = struct
    include Var
    let compare = VO.compare
  end
  module VarSet = Set.Make(Var)
  module VarMap = Map.Make(Var)

  module Constr = struct
    type t = Ty.t * Var.t * Ty.t (* s ≤ α ≤ t *)

    (* C1 subsumes C2 they are on the same variable
       and gives better bounds (larger lower bound and smaller upper bound)
    *)
    let subsumes (t1, v1, t1') (t2, v2, t2') =
      Var.equal v1 v2 &&
      Ty.leq t2 t1 && Ty.leq t1' t2'

    let compare (t1,v1,t1') (t2,v2,t2')=
      Var.compare v1 v2 |> ccmp
        Ty.compare t1 t2 |> ccmp
        Ty.compare t1' t2'
  end

  (* As in CDuce, we follow POPL'15 but keep constraint merged:
     - A Constraint Set C, is a (sorted list of triples) (s, α, t)
     - Adding a new constraint for an existing variable merges them.
  *)
  module ConstrSet = struct
    type t = [] | (::) of Constr.t * t
    let any = []
    let singleton e = [e]
    let merge (s, v, t) (s', _, t') = (Ty.cup s s', v, Ty.cap t t')

    let rec add ((_, v, _) as c) l =
      match l with
        [] -> [ c ]
      | ((_, v', _) as c') :: ll ->
        let n = Var.compare v v' in
        if n < 0 then c::l
        else if n = 0 then (merge c c')::ll
        else c' :: add c ll

    let rec cap l1 l2 =
      match l1, l2 with
      | [],  _ -> l2
      | _, []  -> l1
      | ((_,v1, _) as c1)::ll1, ((_, v2, _) as c2)::ll2 ->
        let n = Var.compare v1 v2 in
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
        let n = Var.compare v1 v2 in
        if n < 0 then subsumes ll1 l2
        else if n > 0 then false
        else Constr.subsumes c1 c2 && subsumes ll1 ll2

    let rec compare l1 l2 =
      match l1, l2 with
        [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: ll1, c2 :: ll2 ->
        Constr.compare c1 c2 |> ccmp
          compare ll1 ll2

    let rec to_list_map f = function
        [] -> List.[]
      | e :: ll -> (f e)::to_list_map f ll
  end

  module ConstrSets = struct
    module CS = ConstrSet
    (* Constraint sets are ordered list of non subsumable elements.
       They represent union of constraints, so we maintain the invariant
       that we don't want to add a constraint set that subsumes an already
       existing one.
    *)
    type t = CS.t list
    let empty : t = []
    let any : t = [CS.any]
    let singleton e = [e]
    let single e = singleton (CS.singleton e)
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
    let cap t1 t2 =
      (cartesian_product t1 t2)
      |> List.fold_left (fun acc (cs1,cs2) -> add (CS.cap cs1 cs2) acc) empty

    let disj t = List.fold_left cup empty t
    let conj t = List.fold_left cap any t
    let to_list l = l

  end

  module MCSS = ConstrSets
  module MCS = MCSS.CS

  module Summand = struct
    type t = VarSet.t * VarSet.t * Descr.t

    let of_line (pvs, nvs, d) : t =
      (VarSet.of_list pvs, VarSet.of_list nvs, d)


    let to_ty (pvs, nvs, d) =
      [(pvs |> VarSet.to_list, nvs |> VarSet.to_list, d)] |> VDescr.of_dnf |> Ty.of_def

    let pos_var v (pvs, nvs, d) =
      let t = (VarSet.remove v pvs, nvs, d) |> to_ty in
      (Ty.empty, v, Ty.neg t)

    let neg_var v (pvs, nvs, d) =
      let t = (pvs, VarSet.remove v nvs, d) |> to_ty in
      (t, v, Ty.any)

    let smallest_toplevel delta ((pvs, nvs, _) as s) =
      match VarSet.(
          min_elt_opt (diff pvs delta),
          min_elt_opt (diff nvs delta))
      with
      | None, None -> None
      | Some v, None -> Some (pos_var v s)
      | None, Some v -> Some (neg_var v s)
      | Some v1, Some v2 ->
        if Var.compare v1 v2 <= 0 then Some (pos_var v1 s)
        else Some (neg_var v2 s)
  end

  module VDSet = Set.Make(VDescr)

  let norm delta_ty delta t =
    let rec aux m t =
      if Base.VarSet.subset (Ty.vars t) delta_ty then
        (* Optimisation: performing tallying on an expression with
           no polymorphic type variable should be as fast as subtyping. *)
        begin if Ty.is_empty t then MCSS.any else MCSS.empty end
      else
        let vd = Ty.def t in
        if VDSet.mem vd m then MCSS.any
        else
          let m = VDSet.add vd m in
          let summands = vd |> VDescr.dnf |> VDescr.Dnf.simplify |> List.map Summand.of_line in
          summands |> List.map (aux_summand m) |> MCSS.conj
    and aux_summand m summand =
      match Summand.smallest_toplevel delta summand with
      | None ->
        let (_,_,d) = summand in
        aux_descr m d
      | Some cs -> MCSS.single cs
    and aux_descr m d  =
      Descr.components d |> List.map (aux_comp m) |> MCSS.conj
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
      | true, [] -> MCSS.any
      | _, _ -> MCSS.empty
    and aux_intervals _ d =
      match Intervals.destruct d with
      | [] -> MCSS.any
      | _ -> MCSS.empty
    and aux_tags m tag =
      let (cs, others) = tag |> Tags.components in
      if others then MCSS.empty
      else cs |>
           List.map (fun c -> TagComp.as_atom c |> snd |> aux m)
           |> MCSS.conj
    and aux_arrows m arr =
      arr |> Arrows.dnf |> Arrows.Dnf.simplify
      |> List.map (aux_arrow m) |> MCSS.conj
    and aux_tuples m tup =
      let (comps, others) = tup |> Tuples.components in
      if others then MCSS.empty
      else comps |> List.map (aux_tuplecomp m) |> MCSS.conj
    and aux_tuplecomp m tup =
      let n = TupleComp.len tup in
      tup |> TupleComp.dnf |> TupleComp.Dnf.simplify
      |> List.map (aux_tuple m n) |> MCSS.conj
    and aux_records m r =
      r |> Records.dnf |> Records.Dnf.simplify
      |> List.map (aux_record m) |> MCSS.conj
    and aux_arrow m (ps, ns, _) =
      let pt, _ = List.split ps in
      let dom = Ty.disj pt in
      let aux_n (nt,nt') =
        let css1 = Ty.cap nt (Ty.neg dom) |> aux m in
        let aux_p (p1, p2) =
          if p2 = [] then MCSS.any else
            let pt1, _ = List.split p1 in
            let dom1 = Ty.disj pt1 in
            let _, pt2' = List.split p2 in
            let codom2 = Ty.conj pt2' in
            let css1 = Ty.cap nt (Ty.neg dom1) |> aux m in
            let css2 = Ty.cap codom2 (Ty.neg nt') |> aux m in
            MCSS.cup css1 css2
        in
        let css2 = subsets ps |> List.map aux_p |> MCSS.conj in
        MCSS.cap css1 css2
      in
      ns |> List.map aux_n |> MCSS.disj
    and aux_tuple m n (ps, ns, _) =
      let ps = mapn (fun () -> List.init n (fun _ -> Ty.any)) Ty.conj ps in
      let aux_n nss =
        let csss = nss |> List.mapi (fun i ns ->
            let pcomp = List.nth ps i in
            let ncomp = ns |> List.map (fun ns -> List.nth ns i) |> Ty.disj |> Ty.neg in
            Ty.cap pcomp ncomp |> aux m
          ) in
        MCSS.disj csss
      in
      ns |> partitions n |> List.map aux_n |> MCSS.conj
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
      let ps = mapn (fun () -> List.init n (fun _ -> Ty.O.any)) Ty.O.conj ps in
      let aux_n nss =
        let csss = nss |> List.mapi (fun i ns ->
            let pcomp = List.nth ps i in
            let ncomp = ns |> List.map (fun ns -> List.nth ns i) |> Ty.O.disj |> Ty.O.neg in
            Ty.O.cap pcomp ncomp |> aux_oty m
          ) in
        MCSS.disj csss
      in
      ns |> partitions n |> List.map aux_n |> MCSS.conj
    and aux_oty m (n,o) =
      if o then MCSS.empty else aux m n
    in
    aux (VDSet.empty) t

  (* TODO: Normalize using psi, in the same way as for subtyping *)

  let merge delta_ty delta cs =
    (* Step1 from the paper is useless, since the ConstrSet maintains
       merged constraints
    *)
    let rec step2 m prev (cs : MCS.t) =
      match cs with
      | ConstrSet.[] -> prev |> MCSS.singleton
      | ((t',_, t) as constr) :: cs' ->
        let ty = Ty.diff t' t in
        if VDSet.mem (Ty.def ty) m then
          step2 m (MCS.add constr prev) cs'
        else
          let m = VDSet.add (Ty.def ty) m in
          let css = norm delta_ty delta ty in
          let css' = cs |> MCS.cap prev |> MCSS.singleton in
          let css = MCSS.cap css css' in
          css |> MCSS.to_list |> List.map (step2 m MCS.any) |> MCSS.disj
    in
    step2 (VDSet.empty) MCS.any cs

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
    cs |> MCS.to_list_map to_eq |> unify |> Subst.map (Subst.apply !renaming)

  let tally delta cs =
    let delta_ty = Base.VarSet.of_list delta in
    let delta = VarSet.of_list delta in
    let ncss = cs
               |> List.map (fun (s,t) -> norm delta_ty delta (Ty.diff s t)) |> MCSS.conj in
    let mcss = ncss
               |> MCSS.to_list |> List.map (merge delta_ty delta) |> MCSS.disj in
    mcss |> MCSS.to_list |> List.map solve
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
