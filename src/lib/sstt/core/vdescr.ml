open Base
open Sigs

module Make(N:Node) = struct
  module Descr = Descr.Make(N)
  include Polymorphic.Make(N)(Var)(Descr)

  let substitute (s:(t,Descr.Records.Atom.t) MixVarMap.t) t =
    t
    |> Bdd.map_leaves (Descr.substitute (MixVarMap.proj2 s))
    |> substitute (MixVarMap.proj1 s)

  let direct_row_vars t = Bdd.leaves t |> List.fold_left (fun acc d ->
    RowVarSet.union acc (Descr.direct_row_vars d)) RowVarSet.empty
end
