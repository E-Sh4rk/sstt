open Sigs

module Base = Base
include Base

module Ty : Ty = struct
  module N = Node.Node

  type t = N.t

  module VDescr = N.VDescr

  let simpl t = N.with_own_cache N.simplify t ; t

  let any, empty = N.any () |> simpl, N.empty () |> simpl
  let def = N.def (* Should be simplified already *)
  let of_def t = N.of_def t |> simpl

  let s f t = f t |> simpl
  let mk_var, mk_descr = s N.mk_var, s N.mk_descr
  let get_descr = N.get_descr

  let cap t1 t2 = N.cap t1 t2 |> simpl
  let cup t1 t2 = N.cup t1 t2 |> simpl
  let neg t = N.neg t |> simpl
  let diff t1 t2 = N.diff t1 t2 |> simpl
  let conj ts = List.fold_left N.cap any ts |> simpl
  let disj ts = List.fold_left N.cup empty ts |> simpl

  let vars, vars_toplevel, dependencies = N.vars, N.vars_toplevel, N.dependencies
  let from_eqs eqs = N.from_eqs eqs |> List.map simpl
  let substitute s t = N.substitute s t |> simpl

  let is_empty t = N.with_own_cache N.is_empty t
  let leq t1 t2 = N.equal t1 t2 || N.with_own_cache (N.leq t1) t2
  let equiv t1 t2 = N.equal t1 t2 || N.with_own_cache (N.equiv t1) t2
  let disjoint t1 t2 = N.with_own_cache (N.disjoint t1) t2
  let is_any t = N.with_own_cache N.is_any t

  let compare, equal, hash = N.compare, N.equal, N.hash
end

module VDescr = Ty.VDescr
module Descr = VDescr.Descr
module Arrows = Descr.Arrows
module Atoms = Descr.Atoms
module Intervals = Descr.Intervals
module Records = Descr.Records
module Tuples = Descr.Tuples
module Products = Tuples.Products
