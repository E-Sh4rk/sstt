open Core

(* TODO: keep rows simplified by removing redundant bindings? *)

type t = Records.Atom.t
let all_fields f = { Records.Atom.bindings=LabelMap.empty ; tail=f }
let id_for v = all_fields (Ty.F.mk_var v)
let mk bindings tail = { Records.Atom.bindings=LabelMap.of_list bindings ; tail }

let to_record_atom = Fun.id
let tail r = r.Records.Atom.tail
let bindings r = r.Records.Atom.bindings |> LabelMap.bindings
let dom = Records.Atom.dom
let find = Records.Atom.find

let pack f = Descr.mk_record (all_fields f) |> Ty.mk_descr
let equiv t1 t2 =
  let open Records.Atom in
  let dom = LabelSet.union (dom t1) (dom t2) |> LabelSet.elements in
  let t1, t2 = to_tuple_with_tail dom t1, to_tuple_with_tail dom t2 in
  List.for_all2 (fun f1 f2 -> Ty.equiv (pack f1) (pack f2)) t1 t2

let equiv_constraints t1 t2 =
  let open Records.Atom in
  let field lbl f =
    { bindings=LabelMap.singleton lbl f ; tail=Ty.F.any }
    |> Descr.mk_record |> Ty.mk_descr
  in
  let tail_except ls f =
    { bindings=List.map (fun l -> l,Ty.F.any) ls |> LabelMap.of_list ; tail=f }
    |> Descr.mk_record |> Ty.mk_descr
  in
  let dom = LabelSet.union (dom t1) (dom t2) |> LabelSet.elements in
  let cs = dom |> List.concat_map (fun l ->
      let t1, t2 = find l t1 |> field l, find l t2 |> field l in
      [(t1,t2) ; (t2,t1)]
    )
  in
  let t1, t2 = tail_except dom t1.tail, tail_except dom t2.tail in
  (t1,t2)::(t2,t1)::cs

let substitute (s,rs) r =
  let open Records.Atom in
  let r = map_nodes (fun ty -> Ty.substitute (s,rs) ty) r in
  substitute rs r

let vars t =
  let vs = ref VarSet.empty in
  let _ = Records.Atom.map_nodes (fun n -> vs := VarSet.union !vs (Ty.vars n) ; n) t in
  !vs
let row_vars t =
  let vs = ref (Records.Atom.vars_toplevel t) in
  let _ = Records.Atom.map_nodes (fun n -> vs := RowVarSet.union !vs (Ty.row_vars n) ; n) t in
  !vs

let compare = Records.Atom.compare
let equal = Records.Atom.equal
let hash = Records.Atom.hash
