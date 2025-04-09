open Sigs

module Atom = Id.NamedIdentifier()

module Make(N:Node) = struct
  module Atom = Atom
  module ASet = Set.Make(Atom)

  type t = Pos of ASet.t | Neg of ASet.t
  type node = N.t

  let any = Neg ASet.empty
  let empty = Pos ASet.empty

  let mk e = Pos (ASet.singleton e)
  let construct (n,es) =
    let es = ASet.of_list es in
    if n then Pos es else Neg es
  let destruct t = match t with
  | Pos s -> true, ASet.elements s
  | Neg s -> false, ASet.elements s

  let cap t1 t2 =
    match t1, t2 with
    | Pos p1, Pos p2 -> Pos (ASet.inter p1 p2)
    | Neg n1, Neg n2 -> Neg (ASet.union n1 n2)
    | Pos p, Neg n | Neg n, Pos p -> Pos (ASet.diff p n)
  let cup t1 t2 =
    match t1, t2 with
    | Pos p1, Pos p2 -> Pos (ASet.union p1 p2)
    | Neg n1, Neg n2 -> Neg (ASet.inter n1 n2)
    | Pos p, Neg n | Neg n, Pos p -> Neg (ASet.diff n p)
  let neg = function
  | Pos s -> Neg s
  | Neg s -> Pos s
  let diff t1 t2 = cap t1 (neg t2)

  let is_any = function
  | Pos _ -> false
  | Neg n -> ASet.is_empty n

  let is_empty = function
  | Pos p -> ASet.is_empty p
  | Neg _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | Pos _, Neg _ -> 1
    | Neg _, Pos _ -> -1
    | Pos s1, Pos s2 | Neg s1, Neg s2 -> ASet.compare s1 s2
  let equal t1 t2 =
    match t1, t2 with
    | Pos _, Neg _ | Neg _, Pos _ -> false
    | Pos s1, Pos s2 | Neg s1, Neg s2 -> ASet.equal s1 s2

  let direct_nodes _ = []
  let map_nodes _ t = t
  let simplify t = t
end
