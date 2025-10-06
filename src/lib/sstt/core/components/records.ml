open Base
open Sigs
open Sstt_utils

module FieldVar = struct
  include Id.NamedIdentifier()
  let simplify t = t
end

module NodeLeaf(N:Node) = struct
  include N
  (* Nodes should not be simplified here (and indeed Node.simplify has a
  different signature) *)
  let simplify t = t
end

module BTy(N:Node) = struct
  include Bdd.Make(FieldVar)(NodeLeaf(N))
  let is_any t =
    match t with
    | Leaf (l,_) -> N.is_any l
    | Node _ -> false
  let is_empty t =
    leaves t |> List.for_all (fun n -> N.is_empty n)
  let leq t1 t2 =
    diff t1 t2 |> is_empty
  let equiv t1 t2 =
    leq t1 t2 && leq t2 t1
  let disjoint t1 t2 =
    diff t1 t2 |> is_empty |> not
  let get t =
    match t with
    | Leaf (l,_) -> l
    | Node _ -> N.any
end
module BUndef(N:Node) = struct
  include Bdd.Make(FieldVar)(Bdd.BoolLeaf)
  let is_any t =
    match t with
    | Leaf (l,_) -> l
    | Node _ -> false
  let is_empty t =
    leaves t |> List.for_all (fun b -> not b)
  let leq t1 t2 =
    diff t1 t2 |> is_empty
  let equiv t1 t2 =
    leq t1 t2 && leq t2 t1
  let disjoint t1 t2 =
    diff t1 t2 |> is_empty |> not
end

module FieldTy(N:Node) = struct
  type node = N.t

  (* The left component is a union of intersections of field variables or types.
  It is represented as a BDD of Field Variables, with types as leaves. Every
  path of the BDD represents an intersection of the traversed variables (negated
  or not), intersected with the type at the leaf. *)

  (* The right component represents a union of intersections of \bot and field
  variables, where \bot corresponds to undefined fields.  Thus, the
  representation is just a BDD of field variables and boolean leaves: each path
  of the BDD ending with 1 represents an intersection of the traversed variables
  (negated or not) and \bot *)

  module Left = BTy(N)
  module Right = BUndef(N)
  type t = Left.t * Right.t

  let mk t b =
    if b then (Left.leaf t, Right.any) else (Left.leaf t, Right.empty)
  let destruct (l,r) =
    if Right.is_any r then (Left.get l, true) else (Left.get l, false)
  let any = (Left.any, Right.any)
  let empty = (Left.empty, Right.empty)
  let absent = (Left.empty, Right.any)
  let required t = (Left.leaf t, Right.empty)
  let optional t = (Left.leaf t, Right.any)
  let get (l,_) = Left.get l

  let cap (n1, b1) (n2, b2) = (Left.cap n1 n2, Right.cap b1 b2)
  let cap = fcap ~empty ~any ~cap
  let cup (n1, b1) (n2, b2) = (Left.cup n1 n2, Right.cup b1 b2)
  let cup = fcup ~empty ~any ~cup
  let diff (n1, b1) (n2, b2) = (Left.diff n1 n2, Right.diff b1 b2)
  let diff = fdiff ~empty ~any ~diff
  let neg (n, b) = (Left.neg n, Right.neg b)
  let neg = fneg ~empty ~any ~neg
  let conj lst =
    let ns, bs = List.split lst in
    (Left.conj ns, Right.conj bs)
  let disj lst =
    let ns, bs = List.split lst in
    (Left.disj ns, Right.disj bs)

  let is_empty (l,r) =
    Left.is_empty l && Right.is_empty r
  let is_any (l,r) = Left.is_any l && Right.is_any r
  let is_absent (l,r) = Right.is_any r && Left.is_empty l
  let is_optional (_,r) = Right.is_any r
  let is_required (_,r) = not (Right.is_any r)
  let leq (l1,r1) (l2,r2) = Left.leq l1 l2 && Right.leq r1 r2
  let equiv (l1,r1) (l2,r2) = Left.equiv l1 l2 && Right.equiv r1 r2
  let disjoint (l1,r1) (l2,r2) = Left.disjoint l1 l2 && Right.disjoint r1 r2

  let equal (l1,r1) (l2,r2) = Left.equal l1 l2 && Right.equal r1 r2
  let compare (l1,r1) (l2,r2) =  Left.compare l1 l2 |> ccmp Right.compare r1 r2

  let map_nodes f (n,b) = (Left.map_leaves f n, b)

  let hash (l, r) = Hash.(mix (Hashtbl.hash l) (Hashtbl.hash r))
end

module Tail = struct
  type t = tail = Open | Closed | RowVar of RowVar.t

  let get_opt_var = function
    | RowVar v -> Some v
    | _ -> None

  let is_open = function
    | Open | RowVar _ -> true
    | Closed -> false

  let equal t1 t2 =
    match t1, t2 with
    | Open, Open -> true
    | Closed, Closed -> true
    | RowVar v1, RowVar v2 -> RowVar.equal v1 v2
    | _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | Open, Open -> 0
    | Closed, Closed -> 0
    | RowVar v1, RowVar v2 -> RowVar.compare v1 v2
    | Open, _ -> -1
    | _, Open -> 1
    | Closed, _ -> -1
    | _, Closed -> 1
end

module MakeLabelMap(N : Node) = Hash.MapList(Label)(FieldTy(N))

module Atom(N:Node) = struct
  module FieldTy = FieldTy(N)
  module LabelMap = MakeLabelMap(N)
  type node = N.t
  type t = { bindings : LabelMap.t ; tail : tail }

  let hash t =
    Hash.(mix (Hashtbl.hash t.tail) (LabelMap.hash t.bindings))

  let map_nodes f t =
    { t with bindings = LabelMap.map (FieldTy.map_nodes f) t.bindings }

  let direct_nodes t =
    t.bindings |> LabelMap.values |> List.map FieldTy.get
  let dom t = LabelMap.dom t.bindings
  let def_t = function Open | RowVar _ -> FieldTy.any | Closed -> FieldTy.absent

  let default t ot =
    match ot with
      Some on -> on
    | None -> def_t t.tail

  let find lbl t = default t (LabelMap.find_opt lbl t.bindings)
  let to_tuple dom t =
    LabelMap.values_for_domain dom (def_t t.tail) t.bindings
  let to_tuple_with_default dom t =
    (def_t t.tail)::(to_tuple dom t)

  let simplify t =
    let is_default =
      match t.tail with
      | Closed -> FieldTy.is_absent
      | _ -> FieldTy.is_any
    in
    let bindings = LabelMap.filter (fun _ on -> not (is_default on)) t.bindings in
    if bindings == t.bindings then t else { t with bindings }
  let equal t1 t2 =
    Tail.equal t1.tail t2.tail &&
    LabelMap.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare (Tail.is_open t1.tail) (Tail.is_open t2.tail) |> ccmp
      LabelMap.compare t1.bindings t2.bindings
end

module Atom'(N:Node) = struct
  module FieldTy = FieldTy(N)
  module LabelMap = MakeLabelMap(N)

  type tail = Tail.t

  type node = N.t
  type t = { bindings : LabelMap.t ; tail : tail ; required : LabelMap.Set.t option }
  let hash t =
    Hash.(mix3 (Hashtbl.hash t.tail) (LabelMap.hash t.bindings) (Hashtbl.hash t.required))

  let dom t = LabelMap.dom t.bindings

  let def_t = function Open | RowVar _ -> FieldTy.any | Closed -> FieldTy.absent
  let default t ot =
    match ot with
      Some on -> on
    | None -> def_t t.tail

  let find lbl t = default t (LabelMap.find_opt lbl t.bindings)
  let simplify t =
    let is_default =
      match t.tail with
      | Closed -> FieldTy.is_absent
      | _ -> FieldTy.is_any
    in
    let bindings = LabelMap.filter (fun _ on -> not (is_default on)) t.bindings in
    let required =
      match t.required with
      | None -> None
      | Some lbls ->
        if bindings |> LabelMap.exists (fun l on -> LabelMap.Set.mem l lbls |> not && FieldTy.is_required on)
        then None
        else Some (lbls |> LabelMap.Set.filter (fun l -> find l t |> FieldTy.is_absent |> not))
    in
    if bindings == t.bindings && required == t.required then t else
      { t with bindings ; required }
  let is_empty t =
    let is_empty_binding _ on = FieldTy.is_empty on in
    let required_ok =
      match t.required with
      | None -> true
      | Some _ when (Tail.is_open t.tail) -> true
      | Some req ->
        t.bindings |> LabelMap.exists
          (fun l o -> LabelMap.Set.mem l req |> not && FieldTy.is_absent o |> not)
    in
    not required_ok ||
    LabelMap.exists is_empty_binding t.bindings
  let equal t1 t2 =
    Tail.equal t1.tail t2.tail &&
    Option.equal LabelMap.Set.equal t1.required t2.required &&
    LabelMap.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare (Tail.is_open t1.tail) (Tail.is_open t2.tail) |> ccmp
      (Option.compare LabelMap.Set.compare) t1.required t2.required |> ccmp
      LabelMap.compare t1.bindings t2.bindings
end

module Make(N:Node) = struct
  module Atom = Atom(N)
  module Atom' = Atom'(N)

  module F = Atom.FieldTy
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)

  module Tail = Tail

  type t = Bdd.t
  type node = N.t

  let any = Bdd.any
  let empty = Bdd.empty

  let mk a = Bdd.singleton a

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let conj n ps =
    let init = fun () -> List.init n (fun _ -> F.any) in
    mapn init F.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> F.empty) in
    mapn init F.disj ps
  let dnf_line_to_tuple (ps, ns) =
    let open Atom in
    let line_dom line acc =
      List.fold_left
        (fun acc a -> LabelMap.Set.union acc (dom a))
        acc line
    in
    let dom = line_dom ns (line_dom ps LabelMap.Set.empty) in
    let ps = ps |> List.map (Atom.to_tuple_with_default dom) in
    let ns = ns |> List.map (Atom.to_tuple_with_default dom) in
    (ps, ns), LabelMap.Set.cardinal dom + 1

  (* Splits the positive intersection into one record with the intersection
  of the fields (closed if at least one of the positives is closed), and the
  set of all row variables *)
  let normalize_ps ps d =
    let open Atom in
    let intersect_fields lbl =
      List.fold_left (fun acc a -> F.cap acc (Atom.find lbl a))
        F.any ps
    in
    let pos_fields =
      LabelMap.Set.elements d |> List.map (fun lbl -> lbl, intersect_fields lbl)
      |> LabelMap.of_list
    in
    let rowvars, r_tail =
      List.fold_left
      (fun (rvs, t) r ->
        match r.Atom.tail with
        | Open -> rvs, t
        | Closed -> rvs, Closed
        | RowVar v -> RowVarSet.add v rvs, t)
      (RowVarSet.empty, Open)
      ps
    in
    {Atom.bindings=pos_fields; tail=r_tail}, rowvars

  let rec new_psi r0 rvs ns =
    let open Atom in
    let check_field r ns_rest l ty =
      let ty_r = Atom.find l r in
      F.leq ty ty_r
      ||
      let updated_bindings =
        LabelMap.add l (F.cap ty (F.neg ty_r)) r0.bindings
      in
      new_psi {r0 with bindings = updated_bindings} rvs ns_rest
    in
    match ns with
    | [] -> false
    | r::ns_rest ->
      begin match r.Atom.tail with
      | Open ->
          LabelMap.for_all (check_field r ns_rest) r0.bindings
      | v when Tail.equal v r0.tail ->
          LabelMap.for_all (check_field r ns_rest) r0.bindings
      | RowVar v when RowVarSet.mem v rvs ->
          LabelMap.for_all (check_field r ns_rest) r0.bindings
      | _ -> new_psi r0 rvs ns_rest
      end

  let is_clause_empty (ps,ns,b) =
    let open Atom in
    if b then
      let dom = List.fold_left
        (fun acc a -> LabelMap.Set.union acc (dom a))
          LabelMap.Set.empty (ps @ ns)
      in
      let r0, rvs = normalize_ps ps dom in
      LabelMap.exists (fun _ y -> F.is_empty y) r0.bindings
      ||
      new_psi r0 rvs ns
    else
      true
  let is_empty t = t |> Bdd.for_all_lines is_clause_empty

  let leq t1 t2 = Bdd.diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module Comp = struct
    type atom = Atom.t
    type atom' = Atom'.t

    let atom_is_valid _ = true
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let any' = { Atom'.bindings=Atom'.LabelMap.empty ; tail=Open ; required=None }

    let to_atom a' =
      let open Atom' in
      let ns =
        match a'.required with
        | None -> []
        | Some lbls ->
          let bindings = LabelMap.constant lbls F.any in
          [{Atom.bindings=bindings ; Atom.tail=Closed}]
      in
      let ps = [{Atom.bindings=a'.bindings ; Atom.tail=a'.tail}] in
      ps, ns
    let to_atom' (a, b) =
      let open Atom' in
      if b then
        [ { bindings=a.Atom.bindings ; tail=a.tail ; required=None } ]
      else
        let not_binding acc l on =
          { bindings=LabelMap.singleton l (F.neg on) ;
            tail=Open ;
            required=None } :: acc
        in
        let res = LabelMap.fold not_binding [] a.Atom.bindings in
        if (Tail.is_open a.tail) then res
        else { bindings=a.Atom.bindings ; tail=a.tail ; required=Some (Atom.dom a) }::res
    let to_atom' (a,b) =
      to_atom' (a,b) |> List.filter (fun a -> Atom'.is_empty a |> not)
      |> List.map Atom'.simplify
    let combine s1 s2 =
      let open Atom' in
      let bindings = LabelMap.merge (fun _ ot1 ot2 ->
          Some (F.cap (default s1 ot1) (default s2 ot2))
        ) s1.bindings s2.bindings
      in
      let tail = (* TODO: better approximation in case of RowVar? *)
        if (Tail.is_open s1.tail) && (Tail.is_open s2.tail) then Open else Closed
      in
      let required =
        match s1.required, s2.required with
        | None, None -> None
        | Some r, None | None, Some r -> Some r
        | Some r1, Some r2 -> Some (LabelMap.Set.union r1 r2)
      in
      let res = { bindings ; tail ; required } in
      if is_empty res then None else Some (simplify res)
  end
  module Dnf = Dnf.LMake'(Comp)
  let dnf t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export |> Dnf.simplify) t
  let dnf' t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export' |> Dnf.simplify') t
  let of_dnf dnf = N.with_own_cache (fun dnf -> Dnf.import dnf |> Bdd.of_dnf) dnf
  let of_dnf' dnf' = N.with_own_cache (fun dnf' -> Dnf.import' dnf' |> Bdd.of_dnf) dnf'

  let direct_nodes t = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f t = Bdd.map_nodes (Atom.map_nodes f) t
  let map f t = Bdd.map_nodes f t

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
  let hash = Bdd.hash
end