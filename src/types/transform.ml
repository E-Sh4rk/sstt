open Sstt_core
open Sstt_utils

module VDMap = Map.Make(VDescr)

type ctx = {
  mutable cache : Var.t VDMap.t ;
  mutable eqs : (Var.t * Ty.t) list
}

let transform f t =
  let ctx = {
    cache = VDMap.empty ;
    eqs = []
  } in
  let rec aux t =
    let vd = Ty.def t in
    match VDMap.find_opt vd ctx.cache with
    | Some v -> v
    | None ->
      let v = Var.mk "" in
      ctx.cache <- VDMap.add vd v ctx.cache ;
      let vd = f vd |> VDescr.map_nodes aux_ty in
      ctx.eqs <- (v, Ty.of_def vd)::ctx.eqs ;
      v
  and aux_ty t = aux t |> Ty.mk_var in
  let v = aux t in
  let res = Ty.of_eqs ctx.eqs |> VarMap.of_list in
  VarMap.find v res

(* Type simplification *)

let regroup_arrows conjuncts =
  let merge_conjuncts (l,r) (l',r') =
      if Ty.equiv l l'
      then Some (l, Ty.cap r r')
      else if Ty.equiv r r'
      then Some (Ty.cup l l', r)
      else None
  in
  merge_when_possible merge_conjuncts conjuncts
let regroup_arrows (ps,ns,b) =
  (regroup_arrows ps, ns, b)

let regroup_records conjuncts =
  let dom =
    conjuncts |> List.map Records.Atom.dom |>
    List.fold_left LabelSet.union LabelSet.empty |>
    LabelSet.to_list in
  let tuples = conjuncts |> List.map (Records.Atom.to_tuple dom) in
  try
    let tuple = mapn (fun () -> raise Exit) Records.Atom.OTy.conj tuples in
    let bindings = List.combine dom tuple |> LabelMap.of_list in
    let opened = List.for_all (fun a -> a.Records.Atom.opened) conjuncts in
    [{ Records.Atom.bindings ; opened }]
  with Exit -> []
let regroup_records (ps,ns,b) =
  (regroup_records ps, ns, b)

let regroup_tuples conjuncts =
  try [mapn (fun () -> raise Exit) Ty.conj conjuncts]
  with Exit -> []
let regroup_tuples (ps,ns,b) =
  (regroup_tuples ps, ns, b)

let simpl_arrows a =
  Arrows.dnf a |> Arrows.Dnf.simplify |> List.map regroup_arrows |> Arrows.of_dnf
let simpl_records r =
  Records.dnf r |> Records.Dnf.simplify |> List.map regroup_records |> Records.of_dnf
let simpl_tuples p =
  TupleComp.dnf p |> TupleComp.Dnf.simplify |> List.map regroup_tuples
  |> TupleComp.of_dnf (TupleComp.len p)
let simpl_tuples t = Tuples.map simpl_tuples t
let simpl_tagcomp p =
  try Op.TagComp.as_atom p |> TagComp.mk
  with Op.EmptyAtom -> TagComp.empty (TagComp.tag p)
let simpl_tags t = Tags.map simpl_tagcomp t

let simpl_descr d =
  let open Descr in
  d |> components |> List.map (function
    | Intervals i -> Intervals i
    | Atoms a -> Atoms a
    | Tags t -> Tags (simpl_tags t)
    | Arrows a -> Arrows (simpl_arrows a)
    | Tuples t -> Tuples (simpl_tuples t)
    | Records r -> Records (simpl_records r)
  ) |>  Descr.of_components

let simpl_vdescr = VDescr.map simpl_descr

let simplify = transform simpl_vdescr
