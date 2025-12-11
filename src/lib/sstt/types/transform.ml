open Core
open Sstt_utils

module VDHash = Hashtbl.Make(VDescr)

type ctx = {
  cache : Var.t VDHash.t ;
  mutable eqs : (Var.t * Ty.t) list
}

let transform f t =
  let ctx = {
    cache = VDHash.create 8 ;
    eqs = []
  } in
  let rec aux t =
    let vd = Ty.def t in
    match VDHash.find_opt ctx.cache vd with
    | Some v -> v
    | None ->
      let v = Var.mk "" in
      VDHash.add ctx.cache vd v ;
      let vd = f vd |> VDescr.map_nodes aux_ty in
      ctx.eqs <- (v, Ty.of_def vd)::ctx.eqs ;
      v
  and aux_ty t = aux t |> Ty.mk_var in
  let v = aux t in
  let res = Ty.of_eqs ctx.eqs in
  res 
  |> List.find_map (fun (v', t) -> if Var.equal v v' then Some t else None)
  |> Option.get

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
let regroup_arrows (ps,ns) =
  (regroup_arrows ps, ns)

let regroup_pos_line ~any ~conj n conjuncts =
  mapn (fun () -> List.init n (fun _ -> any)) conj conjuncts
let regroup_neg_line ~diff ~leq p ns =
  let merge (p,ns) n =
    try
      let are_smaller tys1 tys2 = List.for_all2 leq tys1 tys2 in
      let rec aux tys1 tys2 =
        match tys1, tys2 with
        | [], [] -> []
        | ty1::tys1, ty2::tys2 when leq ty1 ty2 -> ty1::(aux tys1 tys2)
        | ty1::tys1, ty2::tys2 when are_smaller tys1 tys2 -> (diff ty1 ty2)::tys1
        | _::_, _::_ -> raise Exit
        | _, _ -> assert false
      in
      (aux p n, ns)
    with Exit -> (p, n::ns)
  in
  let p, ns = List.fold_left merge (p,[]) ns in
  [p], List.rev ns

let regroup_tuples n (ps,ns) =
  let p = regroup_pos_line ~any:Ty.any ~conj:Ty.conj n ps in
  regroup_neg_line ~diff:Ty.diff ~leq:Ty.leq p ns
let regroup_records (ps,ns) =
  let open Records.Atom in
  let opened, labels = ref true, ref LabelSet.empty in
  ps |> List.iter (fun r ->
    labels := LabelSet.union !labels (dom r) ;
    opened := !opened && r.opened) ;
  ns |> List.iter (fun r -> labels := LabelSet.union !labels (dom r)) ;
  let labels, opened = LabelSet.elements !labels, !opened in
  let is_empty tyo = Ty.O.is_required tyo && Ty.O.get tyo |> Ty.is_empty in
  let leq tyo1 tyo2 = Ty.O.diff tyo1 tyo2 |> is_empty in
  let ns1, ns2 = List.partition (fun r -> not opened || r.opened) ns in
  let ps, ns1 = List.map (to_tuple labels) ps, List.map (to_tuple labels) ns1 in
  let p = regroup_pos_line ~any:Ty.O.any ~conj:Ty.O.conj (List.length labels) ps in
  let ps, ns1 = regroup_neg_line ~diff:Ty.O.diff ~leq p ns1 in
  let of_tuple tys =
    let bindings = List.combine labels tys |> LabelMap.of_list in
    { bindings ; opened }
  in
  let ps, ns1 = List.map of_tuple ps, List.map of_tuple ns1 in
  ps, ns1@ns2

let simpl_arrows a =
  Arrows.dnf a |> List.map regroup_arrows |> Arrows.of_dnf
let simpl_records r =
  Records.dnf r |> List.map regroup_records |> Records.of_dnf
let simpl_tuples p =
  let n = TupleComp.len p in
  TupleComp.dnf p |> List.map (regroup_tuples n) |> TupleComp.of_dnf n
let simpl_tuples t =
  let b, comps = Tuples.destruct t in
  let comps = List.map simpl_tuples comps in
  Tuples.construct (b, comps)
let simpl_tags c =
  if Op.TagComp.is_identity c then
    Op.TagComp.as_atom c |> TagComp.mk
  else
    c
let simpl_tags t =
    let b, comps = Tags.destruct t in
    let comps = List.map simpl_tags comps in
    Tags.construct (b,comps)

let simpl_descr d =
  let open Descr in
  let b, comps = destruct d in
  let comps = comps |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (simpl_tags t)
      | Arrows a -> Arrows (simpl_arrows a)
      | Tuples t -> Tuples (simpl_tuples t)
      | Records r -> Records (simpl_records r)
    ) in
    construct (b, comps)

let simpl_vdescr = VDescr.map simpl_descr

let simplify = transform simpl_vdescr
