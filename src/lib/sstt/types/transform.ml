open Core
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
    let tuple = mapn (fun () -> raise Exit) Ty.O.conj tuples in
    let bindings = List.combine dom tuple |> LabelMap.of_list in
    let opened = List.for_all (fun a -> a.Records.Atom.opened) conjuncts in
    [{ Records.Atom.bindings ; opened }]
  with Exit -> []
let regroup_records (ps,ns,b) =
  let open Records.Atom in
  (* Convert negative atoms to positive ones when possible *)
  let ps',ns = ns |> List.partition_map (fun r ->
    match LabelMap.to_list r.bindings with
    | [lbl,oty] when r.opened ->
      Either.Left ({ r with bindings=LabelMap.singleton lbl (Ty.O.neg oty) })
    | _ -> Either.Right r
  ) in
  (* Regroup positive conjuncts *)
  (regroup_records (ps'@ps), ns, b)
let merge_record_lines (ps1,ns1,b1) (ps2,ns2,b2) =
  let open Records.Atom in
  match ps1, ps2, ns1, ns2 with
  | [p1], [p2], [], [] when b1=b2 && p1.opened=p2.opened ->
    begin match LabelMap.to_list p1.bindings, LabelMap.to_list p2.bindings with
    | [lbl1,oty1], [lbl2,oty2] when Label.equal lbl1 lbl2 ->
      Some ([{ p1 with bindings=LabelMap.singleton lbl1 (Ty.O.cup oty1 oty2) }],[],b1)
    | _, _ -> None
    end
  | _, _, _, _ -> None

let regroup_tuples conjuncts =
  try [mapn (fun () -> raise Exit) Ty.conj conjuncts]
  with Exit -> []
let regroup_tuples (ps,ns,b) =
  (* Convert negative atoms to positive ones when possible *)
  let ps',ns = ns |> List.partition_map (fun lst ->
    match lst with
    | [ty] -> Either.Left [Ty.neg ty]
    | _ -> Either.Right lst
  ) in
  (* Regroup positive conjuncts *)
  (regroup_tuples (ps'@ps), ns, b)
let merge_tuple_lines (ps1,ns1,b1) (ps2,ns2,b2) =
  match ps1, ps2, ns1, ns2 with
  | [[p1]], [[p2]], [], [] when b1=b2 ->
    Some ([[Ty.cup p1 p2]], [], b1)
  | _, _, _, _ -> None

let simpl_arrows a =
  Arrows.dnf a |> Arrows.Dnf.simplify |> List.map regroup_arrows |> Arrows.of_dnf
let simpl_records r =
  Records.dnf r |> Records.Dnf.simplify |> List.map regroup_records
  |> merge_when_possible merge_record_lines |> Records.of_dnf
let simpl_tuples p =
  let dnf = TupleComp.dnf p |> TupleComp.Dnf.simplify in
  let n = TupleComp.len p in
  dnf |> List.map regroup_tuples
  |> merge_when_possible merge_tuple_lines |> TupleComp.of_dnf n
let simpl_tuples t = Tuples.map simpl_tuples t
let simpl_tags t = Tags.map (fun c -> TagComp.as_atom c |> TagComp.mk) t

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
