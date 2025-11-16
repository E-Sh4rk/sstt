open Core

type t = Records.Atom.t
let id_for v = { Records.Atom.bindings=LabelMap.empty ; tail=Ty.F.mk_var v }

let pack_f f = Descr.mk_record { bindings=LabelMap.empty ; tail=f } |> Ty.mk_descr
let leq t1 t2 =
  let open Records.Atom in
  let dom = LabelSet.union (dom t1) (dom t2) |> LabelSet.elements in
  let t1, t2 = to_tuple_with_tail dom t1, to_tuple_with_tail dom t2 in
  List.for_all2 (fun f1 f2 -> Ty.leq (pack_f f1) (pack_f f2)) t1 t2
let equiv t1 t2 = leq t1 t2 && leq t2 t1

let substitute (s,rs) r =
  let open Records.Atom in
  let r = map_nodes (fun ty -> Ty.substitute (s,rs) ty) r in
  substitute rs r

let dom = Records.Atom.dom
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
