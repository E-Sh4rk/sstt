open Base
open Sigs
open Sstt_utils

module OAtom(N:Node) = struct
  type node = N.t
  type t = node * bool

  let map_nodes f (n,b) = (f n, b)
  let direct_nodes (n,_) = [n]
  let simplify t = t
  let equal (n1,b1) (n2,b2) =
    Bool.equal b1 b2 && N.equal n1 n2
  let compare (n1,b1) (n2,b2) =
    Bool.compare b1 b2 |> ccmp
      N.compare n1 n2
  let hash (n, b) = Hash.(mix (bool b) (N.hash n))

  let is_optional (_,b) = b
  let is_required (_,b) = not b

  let any = (N.any, true)
  let empty = (N.empty, false)
  let present = (N.any, false)
  let absent = (N.empty, true)
  let required t = (t, false)
  let optional t = (t, true)
  let get (t,_) = t

  (* Set-theoretic operations *)
  let cap (n1, b1) (n2, b2) = (N.cap n1 n2, b1 && b2)
  let cap = fcap ~empty ~any ~cap
  let cup (n1, b1) (n2, b2) = (N.cup n1 n2, b1 || b2)
  let cup = fcup ~empty ~any ~cup
  let diff (n1, b1) (n2, b2) = (N.diff n1 n2, b1 && not b2)
  let diff = fdiff ~empty ~any ~diff
  let neg (n, b) = (N.neg n, not b)
  let neg = fneg ~empty ~any ~neg
  let conj lst =
    let ns, bs = List.split lst in
    (N.conj ns, List.fold_left (&&) true bs)
  let disj lst =
    let ns, bs = List.split lst in
    (N.disj ns, List.fold_left (||) false bs)
  let is_empty (n,b) = not b && N.is_empty n
end

module OTy(N:Node) = struct
  module Atom = OAtom(N)
  module BoolLeaf = Bdd.BoolLeaf
  module Bdd = Bdd.Make(Atom)(BoolLeaf)

  type t = Bdd.t
  type node = N.t

  let any = Bdd.any
  let empty = Bdd.empty
  let mk a = Bdd.singleton a
  let present = mk Atom.present
  let absent = mk Atom.absent
  let required t = Atom.required t |> mk
  let optional t = Atom.optional t |> mk

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff
  let conj = List.fold_left cap any
  let disj = List.fold_left cup empty

  let get (ps,ns) =
    Atom.diff (Atom.conj ps) (Atom.disj ns)
  let is_clause_empty (ps,ns,b) =
    not b || Atom.is_empty (get (ps,ns))
  let is_empty t = Bdd.for_all_lines is_clause_empty t
  let get = Bdd.fold_lines (fun acc (ps,ns,b) ->
    if b then Atom.cup acc (get (ps,ns)) else acc) Atom.empty

  let equal = Bdd.equal
  let compare = Bdd.compare
  let hash = Bdd.hash

  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  module Comp = struct
    type atom = Atom.t
    let atom_is_valid _ = true
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
  end
  module Dnf = Dnf.LMake(Comp)
  let dnf t = Bdd.dnf t |> Dnf.export
  let of_dnf dnf = Dnf.import dnf |> Bdd.of_dnf

  let direct_nodes t = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f t = Bdd.map_nodes (Atom.map_nodes f) t
  let map f t = Bdd.map_nodes f t
  let simplify t = Bdd.simplify equiv t

  (* Derived subtyping function *)
  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let is_any t = neg t |> is_empty
  let disjoint t1 t2 = cap t1 t2 |> is_empty
end

module Make(N:Node) = struct
  module OTy = OTy(N)
  include Polymorphic.Make(RowVar)(OTy)

  (* Derived subtyping function *)
  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let is_any t = neg t |> is_empty
  let disjoint t1 t2 = cap t1 t2 |> is_empty
end
