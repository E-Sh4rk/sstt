open Core

type t = Ty.subst
let identity = VarMap.empty, RowVarMap.empty

let not_id v ty = Ty.equiv ty (Ty.mk_var v) |> not
let not_id_row v r = Row.equiv r (Row.id_for v) |> not
let norm s = VarMap.filter not_id s
let norm_row rs = RowVarMap.filter not_id_row rs
let of_list' lst1 lst2 =
  lst1 |> VarMap.of_list |> norm,
  lst2 |> RowVarMap.of_list |> norm_row
let of_list lst = of_list' lst []
let of_list_row lst = of_list' [] lst
let refresh ?names vs =
  let new_name = match names with None -> Var.name | Some f -> f in
  let (bindings, bindings') = vs |> VarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> Var.mk in
      (v, Ty.mk_var v'), (v', Ty.mk_var v)
    ) |> List.split in
  (VarMap.of_list bindings, RowVarMap.empty),
  (VarMap.of_list bindings', RowVarMap.empty)
let refresh_row ?names vs =
  let new_name = match names with None -> RowVar.name | Some f -> f in
  let (rbindings, rbindings') = vs |> RowVarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> RowVar.mk in
      (v, Row.id_for v'), (v', Row.id_for v)
    ) |> List.split in
  (VarMap.empty, RowVarMap.of_list rbindings),
  (VarMap.empty, RowVarMap.of_list rbindings')

let singleton v ty = VarMap.singleton v ty |> norm, RowVarMap.empty
let singleton_row v r = VarMap.empty, RowVarMap.singleton v r |> norm_row
let bindings (s,_) = VarMap.bindings s
let bindings_row (_,rs) = RowVarMap.bindings rs
let add v ty (s,rs) =
  (if not_id v ty then VarMap.add v ty s else s), rs
let add_row v r (s,rs) =
  s, (if not_id_row v r then RowVarMap.add v r rs else rs)
let remove v (s,rs) = VarMap.remove v s, rs
let remove_row v (s,rs) = s, RowVarMap.remove v rs
let map f (s,rs) = VarMap.map f s |> norm, rs
let map_row f (s,rs) = s, RowVarMap.map f rs |> norm_row
let filter f (s,rs) = VarMap.filter f s, rs
let filter_row f (s,rs) = s, RowVarMap.filter f rs
let restrict vs t = filter (fun v _ -> VarSet.mem v vs) t
let restrict_row vs t = filter_row (fun v _ -> RowVarSet.mem v vs) t

let domain t = bindings t |> List.map fst |> VarSet.of_list
let domain_row t = bindings_row t |> List.map fst |> RowVarSet.of_list
let intro t =
  let vs1 = bindings t |> List.map (fun (v,t) -> VarSet.remove v (Ty.vars t))
  |> List.fold_left VarSet.union VarSet.empty in
  let vs2 = bindings_row t |> List.map (fun (_,r) -> Row.vars r)
  |> List.fold_left VarSet.union VarSet.empty in
  VarSet.union vs1 vs2
let intro_row t =
  let vs1 = bindings_row t |> List.map (fun (v,r) -> RowVarSet.remove v (Row.row_vars r))
  |> List.fold_left RowVarSet.union RowVarSet.empty in
  let vs2 = bindings t |> List.map (fun (_,t) -> Ty.row_vars t)
  |> List.fold_left RowVarSet.union RowVarSet.empty in
  RowVarSet.union vs1 vs2
let find (s,_) v =
  match VarMap.find_opt v s with
  | None -> Ty.mk_var v
  | Some t -> t
let find_row (_,rs) v =
  match RowVarMap.find_opt v rs with
  | None -> Row.id_for v
  | Some r -> r

let compose t2 t1 =
  let dom1, rdom1 = domain t1, domain_row t1 in
  let bindings1 = bindings t1
    |> List.map (fun (v,t) -> (v, Ty.substitute t2 t)) in
  let bindings2 = bindings t2
    |> List.filter (fun (v, _) -> VarSet.mem v dom1 |> not) in
  let rbindings1 = bindings_row t1
    |> List.map (fun (v,r) -> (v, Row.substitute t2 r)) in
  let rbindings2 = bindings_row t2
    |> List.filter (fun (v, _) -> RowVarSet.mem v rdom1 |> not) in
  of_list' (bindings1@bindings2) (rbindings1@rbindings2)

let combine (s1,rs1) (s2,rs2) =
  let union _ = raise (Invalid_argument "Domains are not disjoint") in
  VarMap.union union s1 s2, RowVarMap.union union rs1 rs2

let equiv (s1,rs1) (s2,rs2) =
  VarMap.equal Ty.equiv s1 s2 &&
  RowVarMap.equal Row.equiv rs1 rs2
let is_identity (s,rs) = VarMap.is_empty s && RowVarMap.is_empty rs

let apply s ty = Ty.substitute s ty
