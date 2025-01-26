open Sstt_core
open Sstt_utils

module VDMap = Map.Make(VDescr)

type ctx = {
  mutable cache : Var.t VDMap.t ;
  mutable eqs : (Var.t * Ty.t) list
}

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
    let tuple =
      mapn (fun () -> raise Exit) Records.Atom.OTy.conj tuples |> List.tl in
    let bindings = List.combine dom tuple |> LabelMap.of_list in
    let opened = List.for_all (fun a -> a.Records.Atom.opened) conjuncts in
    [{ Records.Atom.bindings ; opened }]
  with Exit -> []

let regroup_records (ps,ns,b) =
  (regroup_records ps, ns, b)

let regroup_products conjuncts =
  try [mapn (fun () -> raise Exit) Ty.conj conjuncts]
  with Exit -> []

let regroup_products (ps,ns,b) =
  (regroup_products ps, ns, b)

let simpl t =
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
      let vd = simpl_vdescr vd in
      ctx.eqs <- (v, Ty.of_def vd)::ctx.eqs ;
      v
  and simpl_ty t = aux t |> Ty.mk_var
  and simpl_vdescr vd =
    VDescr.map simpl_descr vd |> VDescr.map_nodes simpl_ty
  and simpl_descr d =
    let open Descr in
    d |> components |> List.map (function
      | Intervals i -> Intervals i
      | Atoms a -> Atoms a
      | Arrows a -> Arrows (simpl_arrows a)
      | Tuples t -> Tuples (simpl_tuples t)
      | Records r -> Records (simpl_records r)
    ) |>  Descr.of_components
  and simpl_arrows a =
    Arrows.dnf a |> Arrows.Dnf.simplify |> List.map regroup_arrows |> Arrows.of_dnf
  and simpl_records r =
    Records.dnf r |> Records.Dnf.simplify |> List.map regroup_records |> Records.of_dnf
  and simpl_tuples t = Tuples.map simpl_products t
  and simpl_products p =
    Products.dnf p |> Products.Dnf.simplify |> List.map regroup_products
    |> Products.of_dnf (Products.len p)
  in
  let v = aux t in
  let res = Ty.of_eqs ctx.eqs |> VarMap.of_list in
  VarMap.find v res
