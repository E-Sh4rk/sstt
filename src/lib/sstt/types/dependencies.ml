open Core

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
let of_arrows descrs =
  let atoms = descrs |> List.concat_map
    (fun d -> Descr.get_arrows d |> component_atoms Arrows.map) in
  match atoms with
  | [] -> []
  | _ -> [ Position.Dom, atoms |> List.map fst |> NodeSet.of_list ;
           Position.Codom, atoms |> List.map snd |> NodeSet.of_list ]

(* Tuples of different lengths are never compared, and the i-th component
   of a tuple is only compared with the i-th component of other tuples. *)
let of_tuples descrs =
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
let of_tags descrs =
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
let of_records descrs =
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
  [ of_arrows descrs ; of_tuples descrs ;
    of_tags descrs ; of_records descrs ]
  |> List.concat
  |> List.fold_left (fun acc (pos,tys) -> PosMap.add pos tys acc) empty
