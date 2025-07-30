open Sigs

(**
   {1 Types and their components}

   The API to manipulate them follows a layered structure to account for the fact that types
   are equi-recursive:

   - a type {m t} (implemented by {!Ty.t}) is a reference to a descriptor with top-level variables, {m t = v}
   - a full descriptor with top-level variables {m v} (implemented by type {!VDescr.t}) represents a disjunctive normal form
     of positive and negative top-level variables and monomorphic descriptors:
     {math
     v = \bigcup_{i=i\ldots m} ~~\bigcap_{j=1\ldots p} \alpha_{ij} \cap \bigcap_{j=1\ldots n} \lnot\beta_{ij} ~~\cap~~ d_i
     }
   - a monomorphic descriptor {m d} (implemented by {!Descr.t} is a disjoint union of components. Components are {{!Intervals} intervals},
     {{!Enums} enums}, {{!Tuples} tuples}, {{!Tags} tagged types}, {{!Arrows} arrows} and {{!Records} records}.
   - Components {{!Intervals} intervals} and {{!Enums}enums} correspond to basic types
   - Components such as {{!Tuples} tuples}, {{!Tags} tagged types}, {{!Arrows} arrows} and {{!Records} records} correspond to type
     constructors and are union of intersections of atoms, the latter containing type references {!Ty.t}. For instance,
     the component for {{!Arrows} arrows} represents:
      {math
      a = \bigcup_{i=1\ldots m} \bigcap_{j=1 \ldots p} t_{ij}^1 \rightarrow t_{ij}^2 \cap \bigcap_{j=1 \ldots n} \lnot(t_{ij}^1 \rightarrow t_{ij}^2) 
      }
*)

(** {2 Types and descriptors }*)

module Ty : Ty = struct
  module N = Node.Node

  type t = N.t

  module VDescr = Node.VDescr
  module O = struct
    include Records.OTy(N)
  end
  let simpl t = N.with_own_cache N.simplify t ; t
  let s f t = f t |> simpl
  let s' f t = simpl t |> f

  let any = N.any |> simpl
  let empty =  N.empty |> simpl
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

(** @canonical Sstt.VDescr *)
module VDescr = Ty.VDescr

(** @canonical Sstt.Descr *)
module Descr = VDescr.Descr


(** {2 Components } 

    Components are the building blocks of types. Each component represents a
    union of intersections (a DNF) of a particular "type constructor" (basic
    types such as integers or enums, tuples, arrows, …).

    The following modules are convenience aliases to modules found in
    {!Ty.VDescr.Descr}.
*)

(** {3 Basic components }
    These components represent the two basic types, integers and enums.
*)

(** @canonical Sstt.Intervals *)
module Intervals = Descr.Intervals

(** @canonical Sstt.Enums *)
module Enums = Descr.Enums

(** {3 Constructor components } 

    Type constructor components come in two flavors: simple constructors such as
    arrows or records and families such as tuples or tagged type. The latter are
    infinte sets of components, indexed by a value (the arity for tuples and the tag
    for tagged types).

*)

(** @canonical Sstt.Arrows *)
module Arrows = Descr.Arrows

(** @canonical Sstt.Records *)
module Records = Descr.Records

(** @canonical Sstt.Tuples *)
module Tuples = Descr.Tuples

(** @canonical Sstt.TupleComp *)
module TupleComp = Tuples.Comp

(** @canonical Sstt.Tags *)
module Tags = Descr.Tags

(** @canonical Sstt.TagComp *)
module TagComp = Tags.Comp

(** 
   {1 Named identifiers} 

*)

(** Identifiers (type variables, record fields) all share a common interface.
    The library provides
    {{!Stdlib.Set.S}sets} and {{!Stdlib.Map.S}maps} whose elements and keys are identifiers.
*)

module type NamedIdentifier = Id.NamedIdentifier

(** @inline *) 
include Base
