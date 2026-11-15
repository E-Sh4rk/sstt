open Base
open Sigs

module Make(N:Node) = struct
  module Descr = Descr.Make(N)
  include Polymorphic.Make(Var)(Descr)

  let substitute (s:(t,Descr.Records.Atom.t) MixVarMap.t) t =
    let s2 = MixVarMap.proj2 s in
    let t = if RowVarMap.is_empty s2 then t else 
        t |> Bdd.map_leaves (Descr.substitute s2)
    in
    let s1 = MixVarMap.proj1 s in
    if VarMap.is_empty s1 then t else 
      t |> substitute s1

  let direct_row_vars t = Bdd.leaves t |> List.fold_left (fun acc d ->
      RowVarSet.union acc (Descr.direct_row_vars d)) RowVarSet.empty
end
