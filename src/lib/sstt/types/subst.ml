open Core

type t = (Ty.t, Row.t) MixVarMap.t
let identity = MixVarMap.empty

let not_id v ty = Ty.equiv ty (Ty.mk_var v) |> not
let not_id_row v r = Row.equiv r (Row.id_for v) |> not
let norm s = VarMap.filter not_id s
let norm_row rs = RowVarMap.filter not_id_row rs
let of_list' lst1 lst2 =
  MixVarMap.of_map
    (lst1 |> VarMap.of_list |> norm)
    (lst2 |> RowVarMap.of_list |> norm_row)
let of_list lst = of_list' lst []
let of_list_row lst = of_list' [] lst
let to_core_subst s = MixVarMap.map2 Row.to_record_atom s

let combine s1 s2 =
  let union _ = raise (Invalid_argument "Domains are not disjoint") in
  MixVarMap.union union union s1 s2

let refresh ?names vs =
  let new_name = match names with None -> Var.name | Some f -> f in
  let (bindings, bindings') = vs |> VarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> Var.mk in
      (v, Ty.mk_var v'), (v', Ty.mk_var v)
    ) |> List.split in
  MixVarMap.of_list1 bindings, MixVarMap.of_list1 bindings'
let refresh_row ?names vs =
  let new_name = match names with None -> RowVar.name | Some f -> f in
  let (rbindings, rbindings') = vs |> RowVarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> RowVar.mk in
      (v, Row.id_for v'), (v', Row.id_for v)
    ) |> List.split in
  MixVarMap.of_list2 rbindings, MixVarMap.of_list2 rbindings'
let refresh' ?names ?names_row vs =
  let s, rs = refresh ?names (MixVarSet.proj1 vs) in
  let s_row, rs_row = refresh_row ?names:names_row (MixVarSet.proj2 vs) in
  combine s s_row, combine rs rs_row

let singleton v ty = MixVarMap.of_map1 (VarMap.singleton v ty |> norm)
let singleton_row v r = MixVarMap.of_map2 (RowVarMap.singleton v r |> norm_row)
let bindings s = MixVarMap.bindings1 s
let bindings_row s = MixVarMap.bindings2 s
let add v ty s = if not_id v ty then MixVarMap.add1 v ty s else s
let add_row v r s = if not_id_row v r then MixVarMap.add2 v r s else s
let remove v s = MixVarMap.remove1 v s
let remove_row v s = MixVarMap.remove2 v s
let map f s = MixVarMap.of_map
  (MixVarMap.proj1 s |> VarMap.map f |> norm) (MixVarMap.proj2 s)
let map_row f s = MixVarMap.of_map
  (MixVarMap.proj1 s) (MixVarMap.proj2 s |> RowVarMap.map f |> norm_row)
let filter f s = MixVarMap.filter1 f s
let filter_row f s = MixVarMap.filter2 f s
let restrict vs t = filter (fun v _ -> VarSet.mem v vs) t
let restrict_row vs t = filter_row (fun v _ -> RowVarSet.mem v vs) t
let restrict' vs t =
  filter (fun v _ -> MixVarSet.mem1 v vs) t
  |> filter_row (fun v _ -> MixVarSet.mem2 v vs)

let domain t = bindings t |> List.map fst |> VarSet.of_list
let domain_row t = bindings_row t |> List.map fst |> RowVarSet.of_list
let domain' t = MixVarSet.of_set (domain t) (domain_row t)
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
let intro' t = MixVarSet.of_set (intro t) (intro_row t)
let find s v =
  match MixVarMap.find_opt1 v s with
  | None -> Ty.mk_var v
  | Some t -> t
let find_row s v =
  match MixVarMap.find_opt2 v s with
  | None -> Row.id_for v
  | Some r -> r

let compose t2 t1 =
  let s2 = to_core_subst t2 in
  let dom1, rdom1 = domain t1, domain_row t1 in
  let bindings1 = bindings t1
    |> List.map (fun (v,t) -> (v, Ty.substitute s2 t)) in
  let bindings2 = bindings t2
    |> List.filter (fun (v, _) -> VarSet.mem v dom1 |> not) in
  let rbindings1 = bindings_row t1
    |> List.map (fun (v,r) -> (v, Row.substitute s2 r)) in
  let rbindings2 = bindings_row t2
    |> List.filter (fun (v, _) -> RowVarSet.mem v rdom1 |> not) in
  of_list' (bindings1@bindings2) (rbindings1@rbindings2)

let equiv s1 s2 = MixVarMap.equal Ty.equiv Row.equiv s1 s2
let is_identity s = MixVarMap.is_empty s

let apply s ty = Ty.substitute (to_core_subst s) ty
let apply_to_row s r = Row.substitute (to_core_subst s) r
