open Base
open Sigs
open Sstt_utils

module OTy(N:Node) = struct
  type node = N.t
  type t = node * bool

  let any = (N.any, true)
  let empty = (N.empty, false)
  let absent = (N.empty, true)
  let required t = (t, false)
  let optional t = (t, true)
  let get (t,_) = t

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
  let is_any (n,b) = b && N.is_any n
  let is_absent (n,b) = b && N.is_empty n
  let is_optional (_,b) = b
  let is_required (_,b) = not b
  let leq (n1,b1) (n2,b2) = (not b1 || b2) && N.leq n1 n2
  let equiv (n1,b1) (n2,b2) = b1 = b2 && N.equiv n1 n2
  let disjoint (n1,b1) (n2,b2) = not (b1 && b2) && N.disjoint n1 n2

  let equal (n1,b1) (n2,b2) = b1 = b2 && N.equal n1 n2
  let equal' f (n1,b1) (n2,b2) = b1 = b2 && f n1 n2
  let compare (n1,b1) (n2,b2) = Bool.compare b1 b2 |> ccmp N.compare n1 n2
  let compare' f (n1,b1) (n2,b2) = Bool.compare b1 b2 |> ccmp f n1 n2

  let map_nodes f (n,b) = (f n, b)
  let direct_nodes (n,_) = [n]
  let simplify t = t

  let hash (n, b) = Hash.(mix (bool b) (N.hash n))
  let hash' f (n, b) = Hash.(mix (bool b) (f n))
end

module Make(N:Node) = struct
  module OTy = OTy(N)
  include Polymorphic.Make(N)(RowVar)(OTy)

  let equal' f = Bdd.equal' RowVar.equal (OTy.equal' f)
  let compare' f = Bdd.compare' RowVar.compare (OTy.compare' f)
  let hash' f = Bdd.hash' RowVar.hash (OTy.hash' f)
end
