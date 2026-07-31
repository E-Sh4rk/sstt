open Core

type fvar = RowVar.t * Label.t
type t = Subst.t * Subst.t

module RVH = Hashtbl.Make(RowVar)
let mk labels rvs =
  let labels = LabelSet.elements labels in
  let original_rv = RVH.create 10 in
  let s, rs = rvs |> RowVarSet.elements |> List.map (fun rv ->
    let bindings = labels |> List.map (fun lbl ->
        let rv' = RowVar.mk (RowVar.name rv) in
        RVH.add original_rv rv' rv ;
        lbl, rv'
      ) in
    (rv, Row.mk (List.map (fun (lbl, rv') -> lbl, Ty.F.mk_var rv') bindings) (Ty.F.mk_var rv)),
    (List.map (fun (_, rv') -> rv', Row.id_for rv) bindings)
  ) |> List.split in
  Subst.of_list2 s, List.concat rs |> Subst.of_list2
let singl (rv, lbl) =
  let rv' = RowVar.mk (RowVar.name rv) in
  Subst.singleton2 rv (Row.mk [lbl, Ty.F.mk_var rv'] (Ty.F.mk_var rv)),
  Subst.singleton2 rv' (Row.id_for rv)
let empty = Subst.identity, Subst.identity

let merge (s1, rs1) (s2, rs2) =
  let s = Subst.compose s2 s1 in
  let rs = Subst.remove_many2 (Subst.domain2 rs1) rs2 |> Subst.combine rs1 in
  s, Subst.restrict2 (Subst.intro2 s) rs
let merge_many ts = List.fold_left merge empty ts

let fresh_vars (_,rs) = Subst.domain2 rs
let fvars (s,_) =
  Subst.bindings2 s |> List.concat_map (fun (rv, r) ->
      Row.bindings r |> List.map (fun (lbl,_) -> rv, lbl)
    )

let decorrelate (s,_) ty = Subst.apply s ty
let recombine (_,rs) ty = Subst.apply rs ty
let recombine' (s,rs as t) sol =
  Subst.compose sol s |> Subst.remove_many2 (fresh_vars t) |> Subst.compose_restr rs
  
let get_var f = match Ty.F.get_vars f |> RowVarSet.elements with [rv] -> rv | _ -> assert false
let fresh_var_of_fvar (s,_) (rv,lbl) = Subst.find2 s rv |> Row.find lbl |> get_var
let fvar_of_fresh_var (s,rs) rv =
  let rv' = Subst.find2 rs rv |> Row.tail |> get_var in
  Subst.find2 s rv' |> Row.bindings |> List.find_map (fun (lbl,f) ->
    let rv'' = get_var f in
    if RowVar.equal rv rv'' then Some (rv',lbl) else None
  )

(* ===== of_tys ===== *)

let tl_labels tys =
  let labels = ref LabelSet.empty in
  let _ = tys |> List.iter (fun ty -> Ty.def ty |> VDescr.map (fun d ->
        let _ = d |> Descr.get_records |> Records.map (fun r ->
            labels := LabelSet.union !labels (Records.Atom.dom r) ; r
        ) in d
      ) |> ignore
  ) in !labels
let tl_rvs tys = tys
  |> List.map Ty.row_vars_toplevel
  |> List.fold_left RowVarSet.union RowVarSet.empty
let tl_ctx delta tys =
  let rvs = RowVarSet.diff (tl_rvs tys) delta in
  mk (tl_labels tys) rvs

let of_tys delta tys =
  let module NS = Dependencies.NodeSet in
  let visited = ref [] in
  let rec aux tys =
    let tys = NS.remove Ty.any tys |> NS.remove Ty.empty in
    if List.exists (NS.subset tys) !visited then empty
    else begin
      visited := tys::(List.filter (fun tys' -> NS.subset tys' tys |> not) !visited) ;
      let tys = NS.elements tys in
      let deps = tys |> List.map Dependencies.of_ty |> Dependencies.merge_many in
      let ctx = tl_ctx delta tys in
      ctx::(Dependencies.PosMap.bindings deps |> List.map (fun (_,tys) -> aux tys))
      |> merge_many
    end
  in
  aux (NS.of_list tys)
