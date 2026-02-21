open Core

type t = (Ty.t, Row.t) MixVarMap.t
let identity = MixVarMap.empty

let not_id1 v ty = Ty.equiv ty (Ty.mk_var v) |> not
let not_id2 v r = Row.equiv r (Row.id_for v) |> not
let norm1 s = VarMap.filter not_id1 s
let norm2 rs = RowVarMap.filter not_id2 rs
let of_list lst1 lst2 =
  MixVarMap.of_map
    (lst1 |> VarMap.of_list |> norm1)
    (lst2 |> RowVarMap.of_list |> norm2)
let of_list1 lst = of_list lst []
let of_list2 lst = of_list [] lst
let to_core_subst s = MixVarMap.map2 Row.to_record_atom s

let combine s1 s2 =
  let union _ = raise (Invalid_argument "Domains are not disjoint") in
  MixVarMap.union union union s1 s2

let refresh1 ?names vs =
  let new_name = match names with None -> Var.name | Some f -> f in
  let (bindings, bindings') = vs |> VarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> Var.mk in
      (v, Ty.mk_var v'), (v', Ty.mk_var v)
    ) |> List.split in
  MixVarMap.of_list1 bindings, MixVarMap.of_list1 bindings'
let refresh2 ?names vs =
  let new_name = match names with None -> RowVar.name | Some f -> f in
  let (rbindings, rbindings') = vs |> RowVarSet.elements |> List.map
    (fun v ->
      let v' = new_name v |> RowVar.mk in
      (v, Row.id_for v'), (v', Row.id_for v)
    ) |> List.split in
  MixVarMap.of_list2 rbindings, MixVarMap.of_list2 rbindings'
let refresh ?names1 ?names2 vs =
  let s, rs = refresh1 ?names:names1 (MixVarSet.proj1 vs) in
  let s_row, rs_row = refresh2 ?names:names2 (MixVarSet.proj2 vs) in
  combine s s_row, combine rs rs_row

let singleton1 v ty = MixVarMap.of_map1 (VarMap.singleton v ty |> norm1)
let singleton2 v r = MixVarMap.of_map2 (RowVarMap.singleton v r |> norm2)
let bindings1 s = MixVarMap.bindings1 s
let bindings2 s = MixVarMap.bindings2 s
let add1 v ty s = if not_id1 v ty then MixVarMap.add1 v ty s else s
let add2 v r s = if not_id2 v r then MixVarMap.add2 v r s else s
let remove1 v s = MixVarMap.remove1 v s
let remove2 v s = MixVarMap.remove2 v s
let map1 f s = MixVarMap.of_map
  (MixVarMap.proj1 s |> VarMap.map f |> norm1) (MixVarMap.proj2 s)
let map2 f s = MixVarMap.of_map
  (MixVarMap.proj1 s) (MixVarMap.proj2 s |> RowVarMap.map f |> norm2)
let filter1 f s = MixVarMap.filter1 f s
let filter2 f s = MixVarMap.filter2 f s
let restrict1 vs t = filter1 (fun v _ -> VarSet.mem v vs) t
let restrict2 vs t = filter2 (fun v _ -> RowVarSet.mem v vs) t
let restrict vs t =
  filter1 (fun v _ -> MixVarSet.mem1 v vs) t
  |> filter2(fun v _ -> MixVarSet.mem2 v vs)
let remove_many1 vs t = filter1 (fun v _ -> VarSet.mem v vs |> not) t
let remove_many2 vs t = filter2 (fun v _ -> RowVarSet.mem v vs |> not) t
let remove_many vs t =
  filter1 (fun v _ -> MixVarSet.mem1 v vs |> not) t
  |> filter2(fun v _ -> MixVarSet.mem2 v vs |> not)

let domain1 t = bindings1 t |> List.map fst |> VarSet.of_list
let domain2 t = bindings2 t |> List.map fst |> RowVarSet.of_list
let domain t = MixVarSet.of_set (domain1 t) (domain2 t)
let intro1 t =
  let vs1 = bindings1 t |> List.map (fun (v,t) -> VarSet.remove v (Ty.vars t))
  |> List.fold_left VarSet.union VarSet.empty in
  let vs2 = bindings2 t |> List.map (fun (_,r) -> Row.vars r)
  |> List.fold_left VarSet.union VarSet.empty in
  VarSet.union vs1 vs2
let intro2 t =
  let vs1 = bindings2 t |> List.map (fun (v,r) -> RowVarSet.remove v (Row.row_vars r))
  |> List.fold_left RowVarSet.union RowVarSet.empty in
  let vs2 = bindings1 t |> List.map (fun (_,t) -> Ty.row_vars t)
  |> List.fold_left RowVarSet.union RowVarSet.empty in
  RowVarSet.union vs1 vs2
let intro t = MixVarSet.of_set (intro1 t) (intro2 t)
let find1 s v =
  match MixVarMap.find_opt1 v s with
  | None -> Ty.mk_var v
  | Some t -> t
let find2 s v =
  match MixVarMap.find_opt2 v s with
  | None -> Row.id_for v
  | Some r -> r

let compose t2 t1 =
  let s2 = to_core_subst t2 in
  let dom1, rdom1 = domain1 t1, domain2 t1 in
  let b1 = bindings1 t1
    |> List.map (fun (v,t) -> (v, Ty.substitute s2 t)) in
  let b2 = bindings1 t2
    |> List.filter (fun (v, _) -> VarSet.mem v dom1 |> not) in
  let rb1 = bindings2 t1
    |> List.map (fun (v,r) -> (v, Row.substitute s2 r)) in
  let rb2 = bindings2 t2
    |> List.filter (fun (v, _) -> RowVarSet.mem v rdom1 |> not) in
  of_list (b1@b2) (rb1@rb2)

let compose_restr t2 t1 =
  let s2 = to_core_subst t2 in
  t1 |> map1 (Ty.substitute s2) |> map2 (Row.substitute s2)

let equiv s1 s2 = MixVarMap.equal Ty.equiv Row.equiv s1 s2
let is_identity s = MixVarMap.is_empty s

let apply s ty = Ty.substitute (to_core_subst s) ty
let apply_to_row s r = Row.substitute (to_core_subst s) r
