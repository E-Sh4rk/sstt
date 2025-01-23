open Core

type t = Ty.t VarMap.t
let identity = VarMap.empty

let not_id v ty = Ty.equiv ty (Ty.mk_var v) |> not
let norm s = VarMap.filter not_id s
let mk lst = lst |> VarMap.of_list |> norm
let renaming ?names vs =
  let new_name =
    match names with None -> Var.name | Some f -> f
  in
  let (bindings, bindings') = vs |> VarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> Var.mk in
      (v, Ty.mk_var v'), (v', Ty.mk_var v)
    ) |> List.split in
  VarMap.of_list bindings, VarMap.of_list bindings'

let singleton v ty = mk [v, ty]
let bindings = VarMap.bindings
let add v ty s =
  if not_id v ty then VarMap.add v ty s else s
let remove v s = VarMap.remove v s
let map f s = VarMap.map f s |> norm
let filter = VarMap.filter

let domain t = bindings t |> List.map fst |> VarSet.of_list
let find t v =
  match VarMap.find_opt v t with
  | None -> Ty.mk_var v
  | Some t -> t

let compose s2 s1 =
  let dom1 = domain s1 in
  let bindings1 = bindings s1 |> List.map (fun (v,t) -> (v, Ty.substitute s2 t)) in
  let bindings2 =
    bindings s2 |> List.filter (fun (v, _) -> VarSet.mem v dom1 |> not) in
  bindings1@bindings2 |> mk

let equiv s1 s2 = VarMap.equal Ty.equiv s1 s2
let is_identity s = VarMap.is_empty s

let apply s ty = Ty.substitute s ty
