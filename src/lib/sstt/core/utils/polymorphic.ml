open Sigs

module Atom(V:Comparable) = struct
  include V
  let simplify t = t
end

module type Leaf = sig
  include Bdd.Leaf
  type node
  val is_empty : t -> bool
  val direct_nodes : t -> node list
  val map_nodes : (node -> node) -> t -> t
end

module Make(N:Node)(V:Comparable)(L:Leaf with type node = N.t) = struct
  module A = Atom(V)
  module Bdd = Bdd.Make(A)(L)
  module VarMap = Map.Make(V)
  module VarSet = Set.Make(V)

  type leaf = L.t
  type var = V.t
  type t = Bdd.t
  type node = N.t

  let any = Bdd.any
  let empty = Bdd.empty

  let mk_var a = Bdd.singleton a
  let mk_descr d = Bdd.leaf d
  let get_descr t = Bdd.leaves t |> List.fold_left L.cup L.empty

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let direct_vars t = Bdd.atoms t |> VarSet.of_list
  let get_vars = direct_vars

  let is_empty t =
    Bdd.leaves t |> List.for_all L.is_empty

  let direct_nodes t =
    Bdd.leaves t |> List.concat_map L.direct_nodes

  let map f t =
    Bdd.map_leaves f t
  
  let map_nodes f =
    map (L.map_nodes f)

  let substitute s t =
    let f v =
      match VarMap.find_opt v s with
      | None -> Bdd.singleton v
      | Some t -> t
    in
    Bdd.substitute f t

  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let disjoint t1 t2 = cap t1 t2 |> is_empty
  let is_any t = neg t |> is_empty

  let conj = List.fold_left cap any
  let disj = List.fold_left cup empty

  module Comp = struct
    type leaf = L.t
    type atom = A.t

    let atom_is_valid _ = true
    let leaf_is_empty l = L.equal l L.empty
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
  end
  module Dnf = Dnf.Make(Comp)
  let dnf t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export |> Dnf.simplify) t
  let of_dnf dnf = N.with_own_cache (fun dnf -> Dnf.import dnf |> Bdd.of_dnf) dnf

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
  let hash = Bdd.hash
end
