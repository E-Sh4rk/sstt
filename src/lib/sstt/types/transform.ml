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

let filter_dnf ~normalize to_ty dnf =
  let decorate_atom a = to_ty a |> normalize, a in
  let decorate_line' (ps,ns) =
    let pty, nty = List.map fst ps, List.map fst ns in
    let ty = Ty.cap (Ty.conj pty) (Ty.disj nty |> Ty.neg) in
    ty, (ps,ns)
  in
  let decorate_line (ps,ns) =
    let ps = List.map decorate_atom ps in
    let ns = List.map decorate_atom ns in
    decorate_line' (ps,ns)
  in
  let undecorate_line (_, (ps,ns)) =
    List.map snd ps, List.map snd ns
  in
  let dnf = List.map decorate_line dnf in
  let ty = List.map fst dnf |> Ty.disj in
  (* Remove useless clauses *)
  let dnf = dnf |> map_among_others (fun (_, (cp, cn)) c_others ->
      let ty_others = List.map fst c_others |> Ty.disj in
      let ty_p, ty_n = List.map fst cp |> Ty.conj, List.map fst cn |> Ty.disj |> Ty.neg in
      let cp = cp |> filter_among_others (fun _ cp_others ->
          Ty.leq (Ty.cup (Ty.cap (List.map fst cp_others |> Ty.conj) ty_n) ty_others) ty |> not
        ) in
      let cn = cn |> filter_among_others (fun _ cn_others ->
          Ty.leq (Ty.cup (Ty.cap ty_p (List.map fst cn_others |> Ty.disj |> Ty.neg)) ty_others) ty |> not
        ) in
      decorate_line' (cp, cn)
    )
  in
  (* Remove useless summands (must be done AFTER clauses simplification) *)
  let dnf = dnf |> filter_among_others (fun (ty_c,_) c_others ->
      let ty_others = List.map fst c_others |> Ty.disj in
      Ty.leq ty_c ty_others |> not
    ) in
  List.map undecorate_line dnf

let filter_dnf ~normalize to_ty dnf =
  match normalize with
  | None -> dnf
  | Some normalize -> filter_dnf ~normalize to_ty dnf

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
  let tail, labels = ref Ty.F.any, ref LabelSet.empty in
  ps |> List.iter (fun r ->
    labels := LabelSet.union !labels (dom r) ; tail := Ty.F.cap !tail r.tail) ;
  ns |> List.iter (fun r -> labels := LabelSet.union !labels (dom r)) ;
  let labels, tail = !labels, !tail in
  let is_empty s =
    let o = Ty.F.get_descr s in
    Ty.O.is_required o && Ty.O.get o |> Ty.is_empty
  in
  let leq s1 s2 = is_empty (Ty.F.diff s1 s2) in
  let ns1, ns2 = List.partition (fun r -> leq tail r.tail) ns in
  let ps, ns1 = List.map (to_tuple labels) ps, List.map (to_tuple labels) ns1 in
  let p = regroup_pos_line ~any:Ty.F.any ~conj:Ty.F.conj (LabelSet.cardinal labels) ps in
  let ps, ns1 = regroup_neg_line ~diff:Ty.F.diff ~leq p ns1 in
  let of_tuple tys =
    let bindings = LabelMap.combine labels tys in
    { bindings ; tail }
  in
  let ps, ns1 = List.map of_tuple ps, List.map of_tuple ns1 in
  ps, ns1@ns2

let simpl_arrows ~normalize a =
  let to_ty a = Descr.mk_arrow a |> Ty.mk_descr in
  Arrows.dnf a |> List.map regroup_arrows |> filter_dnf ~normalize to_ty |> Arrows.of_dnf
let simpl_records ~normalize r =
  let to_ty a = Descr.mk_record a |> Ty.mk_descr in
  Records.dnf r |> List.map regroup_records |> filter_dnf ~normalize to_ty |> Records.of_dnf
let simpl_tuples ~normalize p =
  let n = TupleComp.len p in
  let to_ty a = Descr.mk_tuple a |> Ty.mk_descr in
  TupleComp.dnf p |> List.map (regroup_tuples n) |> filter_dnf ~normalize to_ty |> TupleComp.of_dnf n
let simpl_tuples ~normalize t =
  let b, comps = Tuples.destruct t in
  let comps = List.map (simpl_tuples ~normalize) comps in
  Tuples.construct (b, comps)
let simpl_tags ~normalize c =
  let tag = TagComp.tag c in
  let to_ty a = Descr.mk_tag a |> Ty.mk_descr in
  if Op.TagComp.is_identity c then
    Op.TagComp.as_atom c |> TagComp.mk
  else if Op.TagComp.preserves_cap c then
    Op.TagComp.as_union c |> List.map (fun a -> [a],[])
    |> filter_dnf ~normalize to_ty |> TagComp.of_dnf tag
  else
    TagComp.dnf c |> filter_dnf ~normalize to_ty |> TagComp.of_dnf tag

let simpl_tags ~normalize t =
    let b, comps = Tags.destruct t in
    let comps = List.map (simpl_tags ~normalize) comps in
    Tags.construct (b,comps)

let simpl_descr ~normalize d =
  let open Descr in
  let b, comps = destruct d in
  let comps = comps |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (simpl_tags ~normalize t)
      | Arrows a -> Arrows (simpl_arrows ~normalize a)
      | Tuples t -> Tuples (simpl_tuples ~normalize t)
      | Records r -> Records (simpl_records ~normalize r)
    ) in
    construct (b, comps)

let simpl_vdescr ~normalize = VDescr.map (simpl_descr ~normalize)

let simplify ?normalize = transform (simpl_vdescr ~normalize)
