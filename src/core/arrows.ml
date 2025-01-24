open Sigs
open Sstt_utils.Utils

module Atom(N:Node) = struct
  type node = N.t
  type t = node * node
  let map f (n1,n2) = (f n1, f n2)
  let nodes (n1,n2) = [n1;n2]
  let simplify t = t
  let equal (s1,t1) (s2,t2) =
    N.equal s1 s2 && N.equal t1 t2
  let compare (s1,t1) (s2,t2) =
    N.compare s1 s2 |> ccmp
    N.compare t1 t2
end

module Make(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)

  type t = Bdd.t
  type node = N.t

  let any () = Bdd.any ()
  let empty () = Bdd.empty ()
  let mk a = Bdd.singleton a

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let rec psi t1 t2 ps =
    match ps with
    | [] -> N.is_empty t1 || N.is_empty t2
    | (s1,s2)::ps ->
      N.is_empty t1 || N.is_empty t2 || (* optimisation *)
      (N.leq t1 s1 || N.leq (List.map snd ps |> N.conj) (N.neg t2)) && (* optimisation *)
      psi t1 (N.cap t2 s2) ps &&
      psi (N.diff t1 s1) t2 ps
  let psi_strict t1 t2 ps =
    match ps with
    | [] -> true
    | (s1,s2)::ps -> psi t1 (N.cap t2 s2) ps && psi (N.diff t1 s1) t2 ps
  let is_clause_empty' ps (t1,t2) =
    N.leq t1 (List.map fst ps |> N.disj) &&
    psi_strict t1 (N.neg t2) ps
  let is_clause_empty (ps,ns,b) =
    if b then List.exists (is_clause_empty' ps) ns else true
  let is_empty t =
    Bdd.dnf t |> List.for_all is_clause_empty

  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  module DnfAtom = struct
    type leaf = bool
    type t = Atom.t
    type t' = NeverAtom.t
    type dnf = (t list * t list * leaf) list
    type dnf' = (t' * leaf) list

    let undesirable_leaf = not
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let to_s _ = raise (Invalid_argument "Arrows cannot be combined.")
    let combine t = match (t:NeverAtom.t) with _ -> .
  end
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf t = Bdd.dnf t |> Dnf.mk
  let of_dnf dnf = Dnf.mk dnf |> Bdd.of_dnf

  let direct_nodes t = Bdd.atoms t |> List.map Atom.nodes |> List.concat
  let map_nodes f t = Bdd.map_nodes (Atom.map f) t

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
end
