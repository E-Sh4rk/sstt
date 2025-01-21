open Sigs

module Atom = Id.NamedIdentifier()

module Make(N:Node) = struct
  module Atom = Atom
  include Fcf.Make(Atom)
  type node = N.t
  let mk = mk_singl
  let direct_nodes _ = []
  let map_nodes _ t = t
  let simplify t = t
end
