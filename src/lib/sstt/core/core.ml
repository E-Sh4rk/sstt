open Sigs

include Base

module Ty : Ty = struct
  module N = Node.Node

  type t = N.t

  module VDescr = Node.VDescr

  module O = struct
    include Records.OTy(N)
    let any, empty, absent = any (), empty (), absent ()
  end

  let simpl t = N.with_own_cache N.simplify t ; t
  let s f t = f t |> simpl
  let s' f t = simpl t |> f

  let any, empty = N.any ()|> simpl, N.empty ()|> simpl
  let def, of_def = s' N.def, s N.of_def

  let mk_var, mk_descr, get_descr = s N.mk_var, s N.mk_descr, s' N.get_descr

  let cap t1 t2 = N.cap t1 t2 |> simpl
  let cup t1 t2 = N.cup t1 t2 |> simpl
  let neg t = N.neg t |> simpl
  let diff t1 t2 = N.diff t1 t2 |> simpl
  let conj ts = N.conj ts |> simpl
  let disj ts = N.disj ts |> simpl

  let vars, vars_toplevel, nodes = s' N.vars, s' N.vars_toplevel, s' N.nodes
  let of_eqs eqs = N.of_eqs eqs |> List.map (fun (v,ty) -> v, simpl ty)
  let substitute s t = N.substitute s t |> simpl
  let factorize t = N.with_own_cache N.factorize t |> simpl

  let is_empty t = N.with_own_cache N.is_empty t
  let leq t1 t2 = N.equal t1 t2 || N.with_own_cache (N.leq t1) t2
  let equiv t1 t2 = N.equal t1 t2 || N.with_own_cache (N.equiv t1) t2
  let disjoint t1 t2 = N.with_own_cache (N.disjoint t1) t2
  let is_any t = N.with_own_cache N.is_any t

  let compare, equal, hash = N.compare, N.equal, N.hash
end 
module VDescr = Ty.VDescr
module Descr = VDescr.Descr

module Intervals = Descr.Intervals
module Enums = Descr.Enums
module Tags = Descr.Tags
module TagComp = Tags.TagComp
module Tuples = Descr.Tuples
module TupleComp = Tuples.TupleComp
module Arrows = Descr.Arrows
module Records = Descr.Records
