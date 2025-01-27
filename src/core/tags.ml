open Sigs
open Sstt_utils

module Tag = Id.NamedIdentifier()

module Atom(N:Node) = struct
  module Tag = Tag
  type node = N.t
  type t = Tag.t * node
  let any t = (t, N.any ())
  let empty t = (t, N.empty ())
  let tag (t,_) = t
  let cap (t1,n1) (t2,n2) =
    assert (Tag.equal t1 t2) ;
    (t1, N.cap n1 n2)
  let cup (t1,n1) (t2,n2) =
    assert (Tag.equal t1 t2) ;
    (t1, N.cup n1 n2)
  let diff (t1,n1) (t2,n2) =
    assert (Tag.equal t1 t2) ;
    (t1, N.diff n1 n2)
  let neg (t,n) = (t, N.neg n)
  let map_nodes f (t,n) = t, f n
  let direct_nodes (_,n) = [n]
  let simplify t = t
  let is_empty (_,n) = N.is_empty n
  let equal (t1,n1) (t2,n2) =
    Tag.equal t1 t2 && N.equal n1 n2
  let compare (t1,n1) (t2,n2) =
    Tag.compare t1 t2 |> ccmp
    N.compare n1 n2
end

module Make(N:Node) = struct
  module Atom = Atom(N)
  include Tagcomp.Make(N)(Atom)
end
