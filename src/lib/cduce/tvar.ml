
module CD = Cduce_types

module TVH = Hashtbl.Make(CD.Var)

module TVar = struct
  type t = CD.Var.t

  type vardata = {
    poly: bool;
    infer: bool ;
    dname: string
  }

  let data = TVH.create 100

  let is_unregistered t =
    TVH.mem data t |> not

  let is_poly t = (TVH.find data t).poly
  let is_mono t = is_poly t |> not
  let can_infer t = (TVH.find data t).infer

  let equal = CD.Var.equal
  let compare = CD.Var.compare
  let name = CD.Var.name
  let display_name t = (TVH.find data t).dname

  let unique_mono_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let unique_poly_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let unique_unregistered_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let mk_mono ?(infer=true) name =
    let id = unique_mono_id () in
    let norm_name = (string_of_int id)^"M" in
    let name = match name with None -> norm_name | Some str -> str in
    let var = CD.Var.mk norm_name in
    TVH.add data var {poly=false; infer; dname=name} ;
    var
  let mk_poly name =
    let id = unique_poly_id () in
    let norm_name = (string_of_int id)^"P" in
    let name = match name with None -> norm_name | Some str -> str in
    let var = CD.Var.mk norm_name in
    TVH.add data var {poly=true; infer=true; dname=name} ;
    var
  let mk_fresh t =
    let dname = display_name t in
    if is_mono t then mk_mono (Some dname) else mk_poly (Some dname)
  let mk_unregistered () =
    let id = unique_unregistered_id () in
    let norm_name = (string_of_int id)^"U" in
    CD.Var.mk norm_name

  let typ = CD.Types.var

  let pp = CD.Var.print
end

module TVarSet = struct
  type t = CD.Var.Set.t
  let empty = CD.Var.Set.empty
  let construct = CD.Var.Set.from_list
  let is_empty = CD.Var.Set.is_empty
  let mem = CD.Var.Set.mem
  let filter = CD.Var.Set.filter
  let union = CD.Var.Set.cup
  let union_many = List.fold_left union empty
  let add = CD.Var.Set.add
  let inter = CD.Var.Set.cap
  let inter_many = List.fold_left inter empty
  let diff = CD.Var.Set.diff
  let rm = CD.Var.Set.remove
  let equal = CD.Var.Set.equal
  let destruct = CD.Var.Set.get
  let pp fmt t =
    destruct t |> Format.fprintf fmt "%a@." (Utils.pp_list TVar.pp)
end

let vars = CD.Types.Subst.vars
let check_var = CD.Types.Subst.check_var

module Subst = struct
  type t = CD.Types.Subst.t
  let is_id (v,t) =
    match CD.Types.Subst.check_var t with
    | `Pos v' when CD.Var.equal v v' -> true
    | _ -> false
  let normalize = CD.Var.Map.filter (fun v t -> is_id (v,t) |> not)
  let construct lst =
    lst |> CD.Types.Subst.from_list |> normalize
  let identity = CD.Types.Subst.from_list []
  let destruct = CD.Var.Map.get
  let is_identity t = destruct t |> List.for_all is_id
  let apply = CD.Types.Subst.apply
  let dom s = CD.Var.Map.domain s
  let mem s v = CD.Var.Set.mem (dom s) v
  let rm = CD.Var.Map.remove
  let find s v = CD.Var.Map.assoc v s
  let find' t v = if mem t v then find t v else TVar.typ v
  let equiv s1 s2 =
    let s1 = normalize s1 in
    let s2 = normalize s2 in
    CD.Var.Map.equal Base.equiv s1 s2

  let apply_to_subst s2 s1 =
    destruct s1 |>
      List.map (fun (v,t) -> (v, apply s2 t))
  let compose s2 s1 =
      let only_s2 = destruct s2 |>
          List.filter (fun (v, _) -> mem s1 v |> not) in
      construct ((apply_to_subst s2 s1)@only_s2)
  let compose_restr s2 s1 = apply_to_subst s2 s1 |> construct
  let combine s1 s2 =
      assert (TVarSet.inter (dom s1) (dom s2) |> TVarSet.is_empty) ;
      let s1 = destruct s1 in
      let s2 = destruct s2 in
      s1@s2 |> construct
  let restrict s vars =
      let vars = TVarSet.inter (dom s) vars in
      vars |> TVarSet.destruct |> List.map (fun v -> (v, find s v)) |> construct
  let remove s vars =
      let vars = TVarSet.diff (dom s) vars in
      restrict s vars
  let split s vars =
      (restrict s vars, remove s vars)
  let vars s =
      destruct s |> List.map (fun (v, t) -> TVarSet.rm v (vars t))
      |> TVarSet.union_many
  let is_renaming t =
    destruct t |>
    List.for_all (fun (_,t) ->
      match check_var t with
      | `Pos _ -> true
      | _ -> false)
  (* let inverse_renaming t =
    destruct t |>
    List.map (fun (v,t) ->
      match check_var t with
      | `Pos v' -> (v', TVar.typ v)
      | _ -> assert false) |>
    construct *)

  let short_names vs =
    let r = CD.Var.full_renaming vs in
    CD.Var.Map.map TVar.typ r

(* let pp_entry fmt (v,t) =
    Format.fprintf fmt "%a ===> %a" pp_var v pp_typ t
  let pp fmt t =
    Format.fprintf fmt "%a@." (Utils.pp_long_list pp_entry) (destruct t) *)
  let pp = CD.Types.Subst.print
end

let vars_mono t =
  TVarSet.filter TVar.is_mono (vars t)
let vars_poly t =
  TVarSet.filter TVar.is_poly (vars t)
let vars_infer t =
  TVarSet.filter TVar.can_infer (vars t)
let top_vars = CD.Types.Subst.top_vars
let vars_with_polarity t = CD.Types.Subst.var_polarities t |> CD.Var.Map.get
let is_mono_typ t = vars_poly t |> TVarSet.is_empty
let is_novar_typ t = vars t |> TVarSet.is_empty

let refresh vars =
  let f v = (v, TVar.mk_fresh v |> TVar.typ) in
  vars |> TVarSet.destruct |> List.map f |> Subst.construct

let generalize vars =
  let f v = (v, TVar.mk_poly None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_mono |>
    TVarSet.destruct |> List.map f |> Subst.construct
let monomorphize vars =
  let f v = (v, TVar.mk_mono None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_poly |>
    TVarSet.destruct |> List.map f |> Subst.construct

let generalize_unregistered vars =
  let f v = (v, TVar.mk_poly None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_unregistered |>
    TVarSet.destruct |> List.map f |> Subst.construct
(* let monomorphize_unregistered vars =
  let f v = (v, TVar.mk_mono None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_unregistered |>
    TVarSet.destruct |> List.map f |> Subst.construct *)

let pp_typ_short fmt t =
  let t = Subst.apply (Subst.short_names (vars t)) t in
  Base.pp_typ fmt t
let string_of_type_short t =
  Format.asprintf "%a" pp_typ_short t

(* Operations on types *)
module Iter = Base.Iter
module type Kind = Base.Kind

module Raw = struct
  (* Tallying *)
  let clean_type ~pos ~neg vars t =
    CD.Types.Subst.clean_type ~pos ~neg vars t
  let rectype = CD.Types.Subst.solve_rectype

  let [@warning "-32"] print_tallying_instance delta constr =
    Format.printf "Constraints:@." ;
    let allvars = ref TVarSet.empty in
    constr |> List.iter (fun (l,r) ->
        allvars := TVarSet.union (!allvars) (vars l) ;
        allvars := TVarSet.union (!allvars) (vars r) ;
        Format.printf "(%a, %a)@." Base.pp_typ l Base.pp_typ r ;
    );
    Format.printf "With delta=%a, and var order=%a@."
        (Utils.pp_list TVar.pp) (TVarSet.destruct delta)
        (Utils.pp_list TVar.pp) (TVarSet.destruct !allvars)

  let [@warning "-32"] check_tallying_solution constr res =
    let error = ref false in
    let res =
        res |> List.filter_map (fun s ->
        if (constr |> List.for_all (fun (l,r) ->
                Base.subtype (Subst.apply s l) (Subst.apply s r)
            ))
        then Some s else begin
            error := true ;
            (* Format.printf "INVALID SOLUTION REMOVED: %a@." Subst.pp s ; *)
            None
        end
    )
    in
    if !error then begin
        Format.printf "===== WARNING: Cduce tallying issue.@. ====="
    end ; res

  let tallying d cs =
      CD.Types.Tallying.tallying d cs
      (* |> (check_tallying_solution cs) *)

  let test_tallying d cs =
    let res = CD.Types.Tallying.test_tallying d cs in
    (* let res' = tallying d cs <> [] in *)
    (* assert (res = res') ; *)
    res
end

let clean_type ~pos ~neg t =
  Raw.clean_type ~pos ~neg (vars_mono t) t

let clean_type_subst ~pos ~neg t =
  vars_with_polarity t |>
  List.filter_map (fun (v,p) ->
      if TVar.is_mono v then None
      else match p with
      | `Pos -> Some (v, pos)
      | `Neg -> Some (v, neg)
      | `Both -> None
  ) |> Subst.construct

let test_tallying constr =
  let mono = constr |>
    List.map (fun (a,b) -> [vars_mono a ; vars_mono b]) |>
    List.flatten in
  let mono = TVarSet.union_many mono in
  Raw.test_tallying mono constr

let tallying constr =
  let vars = constr |>
    List.map (fun (a,b) -> [vars a ; vars b]) |>
    List.flatten |> TVarSet.union_many in
  let mono = vars |> TVarSet.filter TVar.is_mono in
  (* let poly = vars |> TVarSet.filter TVar.is_poly in *)
  Raw.tallying mono constr
  |> List.map (fun s ->
    let reg_subst = generalize_unregistered (Subst.vars s) in
    (* let ref_subst = refresh poly in *)
    Subst.compose_restr reg_subst s
    (* |> Subst.compose ref_subst *)
  )

let tallying_infer constr =
  let infer = constr |>
    List.map (fun (a,b) -> [vars_infer a ; vars_infer b]) |>
    List.flatten |> TVarSet.union_many in
  let gen = generalize infer in
  let constr = constr |>
    List.map (fun (a,b) ->
      let r1 = refresh (vars_poly a) in
      let r2 = refresh (vars_poly b) in
      let a = Subst.apply r1 a in
      let b = Subst.apply r2 b in
      (Subst.apply gen a, Subst.apply gen b))
  in
  tallying constr |> List.map (fun s ->
    let s = Subst.compose_restr s gen in
    let mono_subst = monomorphize (Subst.vars s) in
    Subst.compose_restr mono_subst s
  )

let factorize (pvs, nvs) t =
  let open Iter in
  let treat_kind m t =
    let module K = (val m : Kind) in
    let conj lst = match lst with
    | [] -> K.Dnf.any
    | a::lst -> List.fold_left K.Dnf.cap a lst
    in
    let disj lst = match lst with
    | [] -> K.Dnf.empty
    | a::lst -> List.fold_left K.Dnf.cup a lst
    in
    let rebuild_partial lst =
      lst |> List.map (fun ((pvs, nvs), mono) ->
        let pvs = TVarSet.destruct pvs in
        let nvs = TVarSet.destruct nvs in
        let t = K.Dnf.mono mono in
        let pvs = pvs |> List.map K.Dnf.var |> conj in
        let nvs = nvs |> List.map K.Dnf.var |> List.map K.Dnf.neg |> conj in
        conj [pvs ; nvs ; t]
      ) |> disj
    in
    let (a,b) =
      K.get_vars t
      |> K.Dnf.get_partial |> List.map (fun ((pvs',nvs'), mono) ->
        let pvs' = TVarSet.construct pvs' in
        let nvs' = TVarSet.construct nvs' in
        if TVarSet.diff pvs pvs' |> TVarSet.is_empty &&
            TVarSet.diff nvs nvs' |> TVarSet.is_empty
        then
          let pvs' = TVarSet.diff pvs' pvs in
          let nvs' = TVarSet.diff nvs' nvs in
          ([((pvs',nvs'),mono)], [])
        else ([], [((pvs',nvs'),mono)])
      ) |> List.split in
    let (a,b) = (List.concat a, List.concat b) in
    let (a,b) = (rebuild_partial a, rebuild_partial b) in
    (K.mk a, K.mk b)
  in
  let t = fold (fun (a,b) pack t ->
      let (a',b') = match pack with
      | Absent -> (Base.empty, Base.absent)
      | Abstract m | Char m | Int m | Atom m -> treat_kind m t
      | Times m ->
          let module K = (val m :> Kind) in
          treat_kind (module K) t
      | Xml m ->
          let module K = (val m :> Kind) in
          treat_kind (module K) t
      | Function m ->
          let module K = (val m :> Kind) in
          treat_kind (module K) t
      | Record m ->
          let module K = (val m :> Kind) in
          treat_kind (module K) t
      in
      (Base.cup a a', Base.cup b b')
    ) (Base.empty, Base.empty) t
  in t
