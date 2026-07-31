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

(* ===== dependencies ===== *)

module Dependencies = struct

  module NodeSet = Set.Make(Ty)

  (* A position under which the direct dependencies (subnodes) of a type can be found. *)
  module Position = struct
    type t =
      | Dom                (* domain of an arrow *)
      | Codom              (* codomain of an arrow *)
      | Tuple of int * int (* index and length of a tuple *)
      | Tag of Tag.t       (* content of a tagged type *)
      | Field of Label.t   (* field of a record *)
      | Tail               (* tail of a record *)

    let rank = function
      | Dom -> 0 | Codom -> 1 | Tuple _ -> 2 | Tag _ -> 3 | Field _ -> 4 | Tail -> 5
    let compare p1 p2 =
      match p1, p2 with
      | Tuple (i1,n1), Tuple (i2,n2) ->
        let c = Int.compare n1 n2 in if c <> 0 then c else Int.compare i1 i2
      | Tag t1, Tag t2 -> Tag.compare t1 t2
      | Field l1, Field l2 -> Label.compare l1 l2
      | _, _ -> Int.compare (rank p1) (rank p2)
  end
  module PosMap = Map.Make(Position)

  type t = NodeSet.t PosMap.t

  let empty : t = PosMap.empty

  let merge d1 d2 =
    let tail d = PosMap.find_opt Position.Tail d |> Option.value ~default:NodeSet.empty in
    let tl1, tl2 = tail d1, tail d2 in
    PosMap.merge (fun pos tys1 tys2 ->
        (* A label that is not explicitly bound has the dependencies of the tail. *)
        let dflt1, dflt2 = match pos with
          | Position.Field _ -> tl1, tl2
          | _ -> NodeSet.empty, NodeSet.empty in
        Some (NodeSet.union (Option.value tys1 ~default:dflt1)
                            (Option.value tys2 ~default:dflt2))
      ) d1 d2
  let merge_many ds = List.fold_left merge empty ds

  (* Top-level descriptors of the type [ty] (one for each summand of its definition). *)
  let tl_descrs ty =
    let descrs = ref [] in
    let _ = Ty.def ty |> VDescr.map (fun d -> descrs := d::(!descrs) ; d) in
    !descrs

  (* Atoms of a component, given the [map] function iterating over its atoms. *)
  let component_atoms map c =
    let atoms = ref [] in
    let _ = map (fun a -> atoms := a::(!atoms) ; a) c in
    !atoms

  (* [Records.FTy] is used instead of [Ty.F] so that no useless simplification is performed. *)
  let add_field_nodes f tys =
    let tys = ref tys in
    let _ = Records.FTy.map_nodes (fun n -> tys := NodeSet.add n !tys ; n) f in
    !tys

  (* Domains are only compared with domains, and codomains with codomains. *)
  let arrow_dependencies descrs =
    let atoms = descrs |> List.concat_map
      (fun d -> Descr.get_arrows d |> component_atoms Arrows.map) in
    match atoms with
    | [] -> []
    | _ -> [ Position.Dom, atoms |> List.map fst |> NodeSet.of_list ;
             Position.Codom, atoms |> List.map snd |> NodeSet.of_list ]

  (* Tuples of different lengths are never compared, and the i-th component
     of a tuple is only compared with the i-th component of other tuples. *)
  let tuple_dependencies descrs =
    let comps = descrs |> List.concat_map
      (fun d -> Descr.get_tuples d |> Tuples.components |> fst) in
    comps |> List.map TupleComp.len |> List.sort_uniq Int.compare
    |> List.concat_map (fun n ->
        comps |> List.filter (fun c -> TupleComp.len c = n)
        |> List.concat_map (component_atoms TupleComp.map)
        |> List.fold_left (fun acc atom -> List.map2 NodeSet.add atom acc)
          (List.init n (fun _ -> NodeSet.empty))
        |> List.mapi (fun i tys -> Position.Tuple (i,n), tys)
      )

  (* Types with different tags are never compared. *)
  let tag_dependencies descrs =
    let comps = descrs |> List.concat_map
      (fun d -> Descr.get_tags d |> Tags.components |> fst) in
    comps |> List.map TagComp.tag |> List.sort_uniq Tag.compare
    |> List.map (fun tag ->
        Position.Tag tag,
        comps |> List.filter (fun c -> TagComp.tag c |> Tag.equal tag)
        |> List.concat_map (component_atoms TagComp.map)
        |> List.fold_left (fun acc (_,ty) -> NodeSet.add ty acc) NodeSet.empty
      )

  (* There is one position for each label explicitly bound by a record atom,
     plus one for the tail. Note that the tail of a record atom is taken into account
     for every label it does not bind, as it gives the type of these labels
     (for instance, `{ ;; t }` is equivalent to `{ a: t ; b: t ;; t }`). *)
  let record_dependencies descrs =
    let atoms = descrs |> List.concat_map
      (fun d -> Descr.get_records d |> component_atoms Records.map) in
    match atoms with
    | [] -> []
    | _ ->
      let labels = atoms |> List.fold_left
        (fun acc r -> LabelSet.union acc (Records.Atom.dom r)) LabelSet.empty in
      let position field = atoms |> List.fold_left
        (fun acc r -> add_field_nodes (field r) acc) NodeSet.empty in
      (LabelSet.elements labels |> List.map (fun lbl ->
          Position.Field lbl, position (Records.Atom.find lbl)))
      @ [ Position.Tail, position (fun r -> r.Records.Atom.tail) ]

  let of_ty ty =
    let descrs = tl_descrs ty in
    [ arrow_dependencies descrs ; tuple_dependencies descrs ;
      tag_dependencies descrs ; record_dependencies descrs ]
    |> List.concat
    |> List.fold_left (fun acc (pos,tys) -> PosMap.add pos tys acc) empty

end

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
