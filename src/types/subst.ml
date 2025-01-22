open Core

type t = Ty.t VarMap.t
let identity = VarMap.empty

let not_id v ty = Ty.equiv ty (Ty.mk_var v) |> not
let norm s = VarMap.filter not_id s
let mk lst = lst |> VarMap.of_list |> norm
let is_identity s = VarMap.is_empty s

let singleton v ty = mk [v, ty]
let bindings = VarMap.bindings
let add v ty s =
  if not_id v ty then VarMap.add v ty s else s
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

let apply s ty = Ty.substitute s ty
