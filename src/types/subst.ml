open Core

type t = Ty.t VarMap.t
let empty = VarMap.empty
let singleton = VarMap.singleton
let of_list = VarMap.of_list
let bindings = VarMap.bindings
let add = VarMap.add
let map = VarMap.map
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
  bindings1@bindings2 |> of_list
