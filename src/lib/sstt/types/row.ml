open Core

type t = Records.Atom.t

let to_record_atom = Fun.id
let tail r = r.Records.Atom.tail
let bindings r = r.Records.Atom.bindings |> LabelMap.bindings
let dom = Records.Atom.dom
let find = Records.Atom.find

let all_fields f = { Records.Atom.bindings=LabelMap.empty ; tail=f }
let id_for v = all_fields (Ty.F.mk_var v)
let any, empty = all_fields Ty.F.any, all_fields Ty.F.empty

let pack f = Descr.mk_record (all_fields f) |> Ty.mk_descr
let norm r =
  let open Records.Atom in
  let tl = r.tail |> pack in
  let bindings = r.bindings |> LabelMap.filter (fun _ f -> Ty.equiv (pack f) tl |> not) in
  { r with bindings }

let mk bindings tail = { Records.Atom.bindings=LabelMap.of_list bindings ; tail } |> norm

let equiv t1 t2 =
  let open Records.Atom in
  let dom = LabelSet.union (dom t1) (dom t2) in
  let t1, t2 = t1.tail::(to_tuple dom t1), t2.tail::(to_tuple dom t2) in
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

let substitute s r =
  let open Records.Atom in
  let r = map_nodes (fun ty -> Ty.substitute s ty) r in
  substitute (MixVarMap.proj2 s) r |> norm

let vars t =
  let vs = ref VarSet.empty in
  let _ = Records.Atom.map_nodes (fun n -> vs := VarSet.union !vs (Ty.vars n) ; n) t in
  !vs
let row_vars t =
  let vs = ref (Records.Atom.vars_toplevel t) in
  let _ = Records.Atom.map_nodes (fun n -> vs := RowVarSet.union !vs (Ty.row_vars n) ; n) t in
  !vs
let all_vars t = MixVarSet.of_set (vars t) (row_vars t)
let row_vars_toplevel t = Records.Atom.vars_toplevel t
let map_nodes f t = Records.Atom.map_nodes f t

let compare = Records.Atom.compare
let equal = Records.Atom.equal
let hash = Records.Atom.hash
