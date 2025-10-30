
open Base

module type Comparable = sig
  type t
  val compare : t -> t -> int
  (** Comparison working on the internal representation of {!t}. *)

  val equal : t -> t -> bool
  (** Equality, [equal a b] is equivalent to [compare a b = 0]. *)

  val hash : t -> int
  (** Hashing, [hash] is consistent with equality, that is
      [equal a b] implies [hash a = hash b].
  *)

end

module type TyBase = sig
  type t

  type node
  (** An alias for {!Sstt.Ty.t}. *)

  val any : t
  (** The top element of {!t} (the set of all values of type {!t}). *)

  val empty : t
  (** The bottom element of {!t} (the empty set of values of type {!t}). *)

  include Comparable with type t := t
end

module type SetTheoretic = sig
  type t
  val cap : t -> t -> t
  (** The intersection of two types, {m t_1 \cap t_2}. *)

  val cup : t -> t -> t
  (** The union of two types, {m t_1 \cup t_2}. *)

  val diff : t -> t -> t
  (** The difference of two types, {m t_1 \setminus t_2}. *)

  val neg : t -> t
  (** The negation of a type, {m \lnot t}.*)

  val conj : t list -> t
  (** [conj l] is the intersection of all the types in [l]. It returns [any] if
      [l] is the empty list.
  *)

  val disj : t list -> t
  (** [disj l] is the union of all the types in [l]. It returns [empty] if
      [l] is the empty list.
  *)

  val is_empty : t -> bool
  (** Emptyness test. [is_empty t] returns [true] if and only if [t] is semantically equivalent
      to [empty]. *)

  val is_any : t -> bool
  (** Fullness test. [is_any t] returns [true] if and only if [t] is semantically equivalent
      to [any]. *)

  val leq : t -> t -> bool
  (** Subtyping test. [leq t1 t2] returns [true] if and only if [t1] is a subtype of [t2]. *)

  val equiv : t -> t -> bool
  (** Type equivalence test. [equiv t1 t2] returns [true] if and only if [leq t1 t2] and [leq t2 t1]. *)

  val disjoint : t -> t -> bool
  (** Disjointedness test. [disjoint t1 t2] returns true if and only if [cap t1 t2] is empty. *)
end

(* DNF *)

(** ['atom ldnf] represents a disjunctive normal form for leaf components,
    that is, a disjunction of clauses. *)
type 'atom ldnf = ('atom list * 'atom list) list

(** ['atom cdnf] represents a condensed disjunctive normal form, that is,
    a disjunction of atoms. *)
type 'atom cdnf = 'atom list

(** [('atom, 'leaf) dnf] represents a disjunctive normal form, that is, a disjunction of clauses. *)
type ('atom, 'leaf) dnf = ('atom list * 'atom list * 'leaf) list

(* Components *)

module type ComponentBase = sig

  include TyBase

  (** {1 Basics}*)

  (** The minimal signature of a component. *)

  module type Atom
  module Atom : Atom
  (* The abstract module representing atoms, which is refined in explicit signatures. *)

end

module type BasicComponentOps = sig

  type atom
  type t
  type repr

  (** {1 Operations on basic components} *)

  val mk : atom -> t
  (** Creates a component from an atom. *)

  val construct : repr -> t
  (** Builds a component from its explicit representation. *)

  val destruct : t -> repr
  (** Returns the explicit representation of component. *)
end

module type ConstrComponentOps = sig

  type atom
  type t
  type node

  (** {1 Explicit DNF, construction and extraction}*)

  val dnf : t -> atom ldnf
  (** [dnf t] returns a disjunctive normal form of [t]. *)

  val of_dnf : atom ldnf -> t
  (** [dnf d] builds a component from a disjunctive normal form [d]. *)

  val mk : atom -> t
  (** Creates a component from an atom. *)

  val map_nodes : (node -> node) -> t -> t
  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
end

module type IndexedComponentOps = sig

  include ConstrComponentOps

  (** {1 Operations for indexed components}*)

  (** Operations on components that are indexed by a type, such as tuples
      (indexed by integers) or tagged types (indexed by a symbolic tag). *)

  type index
  (** The type of indices. *)

  val any : index -> t
  (** The top element for the given index. *)

  val empty : index -> t
  (** The bottom element for the given index. *)

  val of_dnf : index -> atom ldnf -> t
  (** [of_dnf idx d] builds a component for the given index and DNF. *)
end

module type OptComponent = sig

  (** {1 Condensed DNF}*)

  module type Atom'
  module Atom' : Atom'
  (** The abstract module representing condensed atoms. *)

end

module type OptComponentOps = sig

  type t
  type atom'

  val dnf' : t -> atom' cdnf
  (** [dnf' t] returns the condensed DNF of [t]. *)

  val of_dnf' : atom' cdnf -> t
  (** [of_dnf' d] builds a component from an condensed DNF [d]. *)
end

module type OptIndexedComponentOps = sig

  type t
  type atom'

  (** Operations on optimzed indexed components *)

  type index
  (** The type of indices. *)

  val dnf' : t -> atom' cdnf
  (** [dnf' t] returns the condensed DNF of [t]. *)


  val of_dnf' : index -> atom' cdnf -> t
  (** [of_dnf' idx d] builds a component for the given index and condensed DNF. *)
end

module type ComponentFamily = sig

  (** {1 Basics }*)

  (** A compononet {i family } is a set of components indexed by a values. For
      instances, tuples constitute a family of components indexed by their size.
      For instance, 3-tuples and 4-tuples behave similarly, but are distinct
      components that cannot be mixed (their intersection is empty).
  *)


  include TyBase
  module type Comp
  module Comp : Comp
end

module type ComponentFamilyOps = sig

  (** {1 Indexed component and misc. operations}*)

  type t
  (** The type {!t} represents the a disjoint union of components of the family.
      For instance, in the case of tuples, it can represent a set
      of all 2-tuples and 3-tuples (but nothing else), or the set
      of all tuples that are not 4-tuples.
  *)

  type node
  type atom
  type comp
  type index
  val mk : atom -> t
  val mk_comp : comp -> t

  val components : t -> comp list * bool
  (** [components t] returns a pair [(cs,b)] where [cs] are the tuple components
       explicitely present in [t], and [b] is a boolean indicating whether components
       of other cardinalities are [any] (if [b] is [true]) or [empty] (if [b] is [false]). *)

  val of_components : comp list * bool -> t

  val get : index -> t -> comp
  (** [get n t] returns the tuple component of cardinality [n] in [t]. *)


  (** [map f t] replaces every tuple component [p] in [t] by [f p]. *)
  val map : (comp -> comp) -> t -> t

  val construct : bool * comp list -> t

  (** [destruct t] returns a pair [(b,cs)] such that:
      if [b] is true, then [t] contains exactly the tuple components [cs],
      and if [b] is false, then the negation of [t] contains exactly
      the tuple components [cs]. *)
  val destruct : t -> bool * comp list

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* Enums *)

module type EnumAtom = Id.NamedIdentifier

module type Enums =
sig

  type t
  (** Enums represents sets of symbolic constants. These sets can be either finite or cofinite. *)

  type node

  include ComponentBase
    with type t := t
     and type node := node
     and module type Atom := EnumAtom
  (** @inline
      Symbolic constants are implemented by {!Sstt.NamedIdentifier}.
  *)

  module Atom = Enum

  include BasicComponentOps
    with type t := t
     and type atom := Atom.t
     and type repr := bool * Atom.t list
     (**
        @inline

        The explicit representation of an {!Enums.t} is a pair of a
        boolean, and a list of {{!Atom.t} constants}. If the boolean is [true],
        the set if finite and contains all the elements in the list. If the
        boolean is [false], the set is co-finite and contains all the elements
        not in the list.
     *)

end


(* Intervals *)

module type IntervalAtom = sig
  (** Possibly unbounded integer interval. *)

  type t
  (** [t] represents a non-empty integer interval. The interval can be unbounded
      to the left and to the right. Bounds are arbitrary-precision integers
      provided by the {{!Z}zarith} library.
  *)

  val mk : Z.t option -> Z.t option -> t
  (** [mk b1 b2] creates an interval from bound [b1]
      (inclusive, {m -\infty } if [None]) to bound [b2]
      (inclusive, {m +\infty } if [None]).
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise Invalid_argument if the interval is empty. *)

  val mk_bounded : Z.t -> Z.t -> t
  (** [mk_bounded i1 i2] creates an interval from bound [i1]
      (inclusive) to bound [i2] (inclusive).
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise Invalid_argument if the interval is empty. *)

  val mk_singl : Z.t -> t
  (** [mk_singl i] creates an interval containing exactly [i]. *)

  val get : t -> Z.t option * Z.t option
  (** [get t] returns the boundaries (inclusive) of the interval [t]. *)

  include Comparable with type t := t

  val pp : Format.formatter -> t -> unit
  (** Prints the interval to the given formatter. An interval is printed as [(b1..b2)] where
      [bi] is the bound if it is finite, and the empty string if it is infinite.
  *)
end

module type Intervals = sig

  type t
  (** The type of integers, that is a set of disjoint intervals, possibly unbounded intervals.
      An atom is a possibly unbounded, non-empty interval of integers. *)

  type node

  include ComponentBase with type t := t
                         and type node := node
                         and module type Atom := IntervalAtom
  (** @inline *)

  include BasicComponentOps with type t := t
                             and type atom := Atom.t
                             and type repr := Atom.t list
  (** @inline

      The explicit representation of an {!Intervals.t} is a list of {!Atom.t}. *)

  val destruct_neg : t -> Atom.t list
  (** [destruct_neg t] returns the explicit representation of the negation of [t].
      The negation of [t] is sometimes simpler than [t] itself,
      which may justify working on this negative form
      (for instance when pretty-printing).
  *)

end

(* Arrows *)

module type ArrowAtom = sig
  type node
  (** An alias for the type {!Sstt.Ty.t}. *)

  type t = node * node
  (** Function atoms represents a single function type, given by its domain and co-domain. *)

  include Comparable with type t := t

  val map_nodes : (node -> node) -> t -> t
  (** [map_nodes f a] is [(f (fst a), f (snd a))]. *)

end

module type Arrows = sig

  type t
  (** A function type is union of intersection of positive and negative arrows:
      {math
      \bigcup_{i=1\ldots m} \bigcap_{j=1 \ldots p} t_{ij}^1 \rightarrow t_{ij}^2 \cap \bigcap_{j=1 \ldots n} \lnot(t_{ij}^1 \rightarrow t_{ij}^2)
      }
  *)

  type node

  include ComponentBase with type t := t
                         and type node := node
                         and module type Atom := (ArrowAtom with type node := node)
  (** @inline
      {!Atom}s are single arrows denote by its domain and codomain.
  *)

  include ConstrComponentOps with type t := t
                              and type node := node
                              and type atom := Atom.t
                              (** @inline *)
end

(* Records *)

module type RecordAtom = sig
  type node
  (** An alias for the type {!Sstt.Ty.t}. *)

  type oty = node * bool
  (** An optional type (see {!Sstt.Ty.O}). *)

  type t = { bindings : oty LabelMap.t;(** mapping from labels to optional types *)
             opened : bool (** if [true], denotes an open record *)
           }
  (** A single record type.  *)

  val dom : t -> LabelSet.t
  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)

  val find : Label.t -> t -> oty
  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)

  val to_tuple : Label.t list -> t -> oty list
  (** [to_tuple lst r] returns the list of {!oty} associated with each
      label of [lst] in [r]. Each type is computed using {!find}. *)

  val to_tuple_with_default : Label.t list -> t -> oty list
  (** [to_tuple_with_default lst r] returns the list [d :: to_tuple lst r] where
      - [d] is {!Sstt.Ty.O.any} if [r] is an open record
      - [d] is {!Sstt.Ty.O.absent} if [r] is a closed record
  *)

  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
  (** [map_nodes f r] applies [f] to all nodes in [r.bindings]. *)

end

module type RecordAtom' = sig
  type node
  type oty = node * bool

  type t = { bindings : oty LabelMap.t ; opened : bool ; required : LabelSet.t option }
  (** A compact representation for record types.
      The [bindings] and [opened] field have the same meaning as in {!Records.Atom.t}.
      When the field [required] is equal to [Some labels],
      it means that [t] requires at least one field not in [labels] to be present. *)


  val dom : t -> LabelSet.t
  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)

  val find : Label.t -> t -> oty
  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)

  include Comparable with type t := t
end

module type Records = sig

  type t
  (** Record types is a union of intersection of positive and negative records:
      {math
        \bigcup_{i=1\ldots m} \bigcap_{j=1 \ldots p} \texttt{\{}l_{j}{^1}:o_{j}^1,\ldots,l_{j}^k:o_{j}^{k} ~~ b_j\texttt{\}} \cap
        \bigcap_{j=1 \ldots n} \lnot(\{l_{j}{^1}:o_{j}^1,\ldots,l_{j}^l:o_{j}^{l} ~~ b_j\})
      }
      with {m b_j \in \{\texttt{..},~ \epsilon\}}.

  *)

  type node

  (** @inline*)
  include ComponentBase with type t := t
                         and type node := node
                         and module type Atom := (RecordAtom with type node := node)


  (** @inline*)
  include ConstrComponentOps with type t := t
                              and type node := node
                              and type atom := Atom.t

  val dnf_line_to_tuple : (Atom.t list * Atom.t list) -> (Atom.oty list list * Atom.oty list list) * int
  (** [dnf_line_to_tuple (ps, ns)] converts a line of a [Record] DNF to
      a line of tuples. Each record is projected to a tuple that has as many components
      as the number of distinct labels in [ps] and [ns]. The function also returns the size of the tuples
      in the result (which is the number of labels plus one).
  *)

  (** @inline

      Records have a condensed representation where each record atom requires the existence
      of extra labels.
      For instance, given the (closed) record
      {math
      \texttt{\{} x:t, y:s \texttt{\}}
      }
      its negation can be written as a union of positive condensed atoms:
      {math
      \texttt{\{} x:\lnot t, \texttt{.. \}}\cup
      \texttt{\{} y:\lnot s, \texttt{.. \}}\cup
      \texttt{\{} x:t, y:s, \overline{\{x, y\}}~\texttt{.. \}}
      }
      meaning any record which has label {m x} associated to a type that is not {m t}, or
      any record which has label {m y} associated to a type that is not {m s}, or
      any record which has both labels associated to their original type but which {i also has an
      extra label} that is neither {m x} nor {m y}.
  *)
  include OptComponent with module type Atom' := (RecordAtom' with type node := node)

  (** @inline*)
  include OptComponentOps with type t := t
                           and type atom' := Atom'.t
end

(* Tuples *)

module type TupleAtom = sig
  type node
  type t = node list
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type TupleComp = sig
  (** Component for a tuple of a particular fixed arity. *)

  type t
  (** The component of n-tuples is a union of intersections of positive and negative n-tuples. *)

  type node

  (** @inline

      An atom for a tuple of arity [n] si simply an (orederd) list of [n] nodes.
  *)
  include ComponentBase
    with type t := t
     and type node := node
     and module type Atom := (TupleAtom with type node := node)

  (** @inline *)
  include IndexedComponentOps with type t := t
                               and type node := node
                               and type atom := Atom.t
                               and type index := int

  val len : t -> int
  (** [len t] returns the common arity of all tuple types in [t]. *)

  (** @inline *)
  include OptComponent with module type Atom' := (TupleAtom with type node := node)
                        and module Atom' := Atom

  (** @inline

      Tuples commute with intersection, so a union of intersection of tuples can
      be compactly represented as a union of products.

  *)
  include OptIndexedComponentOps with type t := t
                                  and type atom' := Atom.t
                                  and type index := int

end

module type Tuples = sig

  type t
  (** Tuples are a family of components indexed by an integer (the arity of the tuple).
      Type {!t} represents sets finite or co-finite sets of tuple components of distinct
      arities. For instance, it can represent the type of all tuples execpt those
      of arity 2 and 4.
  *)


  type node

  (** @inline *)
  include ComponentFamily with type t := t
                           and type node := node
                           and module type Comp := (TupleComp with type node := node)

  (** @inline *)
  include ComponentFamilyOps with type t := t
                              and type node := node
                              and type index := int
                              and type atom := Comp.Atom.t
                              and type comp := Comp.t

end

(* Tags *)

module type TagAtom = sig
  type node

  type t = Tag.t * node
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type TagComp = sig
  (** Component for a type tagged with a particular tag. *)

  type t
  (** The type of types tagged with a particular fixed tag. *)

  type node

  (**@inline*)
  include ComponentBase with type t := t
                         and type node := node
                         and module type Atom := (TagAtom with type node := node)

  (**@inline*)
  include IndexedComponentOps with type t := t
                               and type node := node
                               and type atom := Atom.t
                               and type index := Tag.t

  val tag : t -> Tag.t
  (** [tag t] returns the common tag of all the tagged-types in this component. *)

  val line_emptiness_checks : (node -> 'a) -> Tag.t -> (Atom.t list * Atom.t list) -> 'a list
  (** [line_emptiness_checks f tag (ps, ns)] converts a line of a [TagComp] DNF of tag [tag] to
      a list of types [tys] such that emptiness of the DNF line is equivalent to
      emptiness of at least one of the [tys]. The function [f] is called on each [tys].
  *)
end

module type Tags = sig

  type t
  (** This family of component encode tagged types, that is type decoreted with
      a "tag" (e.g. {m \texttt{foo}(\texttt{int})}, which represent integers tagged with the tag [foo]).
      These type are only comparable with
      {%html: <span style='font-size:large'>𝟙</span>%},
      {%html: <span style='font-size:large'>𝟘</span>%},
      and tagged type with the same tag. In particular type {m \texttt{foo}(\texttt{int})} is disinct
      from {m \texttt{bar}(\texttt{int})} or {m \texttt{int}} but is a subtype of
      {m \texttt{foo}(\texttt{int}\cup\texttt{\{ .. \}})} for instance.
  *)

  type node

  (** @inline *)
  include ComponentFamily with type t := t
                           and type node := node
                           and module type Comp := (TagComp with type node := node)

  (** @inline *)
  include ComponentFamilyOps with type t := t
                              and type node := node
                              and type index := Tag.t
                              and type atom := Comp.Atom.t
                              and type comp := Comp.t

end

(* Descr *)

module type Descr = sig
  (** Monomorphic type descriptors. *)

  (** {1 Basics }*)

  type t
  (** The type of monomorphics descriptors. It represents a disjoint union of components. *)

  include TyBase with type t := t

  (** {1 Components} *)

  module Intervals : Intervals with type node := node
  (** Integer components. *)

  module Enums : Enums with type node := node
  (** Enums components. *)

  module Arrows : Arrows with type node := node
  (** Arrow components. *)

  module Records : Records with type node := node
  (** Record components. *)

  module Tuples : Tuples with type node := node
  (** Tuple components. *)

  module Tags : Tags with type node := node
  (** Tagged types components. *)

  (** {2 Extracting components }*)

  (**/**)
  (**
     {@ocaml[
     open Sstt.VDescr.Descr
     ]}*)
  (**/**)

  (** A uniform representation of components, tagged with their kind. It can be usefull if some components
      have a uniform behaviour. For instance:
      {@ocaml[

      let map_arrow_records fr fa = function
      | Arrows a -> Arrows (Arrows.map_nodes fa a)
      | Records r -> Records (Records.map_nodes fr r)
      | c -> c
      ]}

  *)
  type component =
    | Intervals of Intervals.t
    | Enums of Enums.t
    | Arrows of Arrows.t
    | Records of Records.t
    | Tuples of Tuples.t
    | Tags of Tags.t

  val components : t -> component list
  (** Break down a type into its list of components. *)

  val set_component : t -> component -> t
  (** [set_component t c] returns the type {m t ~\setminus~}{%html: <span style='font-size:large'>𝟙</span>%}{_{m c}}{m ~\cup~ c}, where
      {%html: <span style='font-size:large'>𝟙</span>%}{_{m c}} is the top of component {m c}. *)

  val of_components : component list -> t
  (** [of_components [c1; ...; cn]] returns {m \bigcup_{i=1\ldots n} c_i} *)

  val of_component : component -> t
  (** [of_component c] is equivalent to [of_components [c]]. *)

  val get_intervals : t -> Intervals.t
  (** Returns the {!Sstt.Intervals} component of a descriptor. *)

  val get_enums : t -> Enums.t
  (** Returns the {!Sstt.Enums} component of a descriptor. *)

  val get_arrows : t -> Arrows.t
  (** Returns the {!Sstt.Arrows} component of a descriptor. *)

  val get_records : t -> Records.t
  (** Returns the {!Sstt.Records} component of a descriptor. *)

  val get_tuples : t -> Tuples.t
  (** Returns the {!Sstt.Tuples} component of a descriptor. *)

  val get_tags : t -> Tags.t
  (** Returns the {!Sstt.Tags} component of a descriptor. *)


  (** {2 Building descriptors from components} *)

  val mk_enum : Enum.t -> t
  (** Creates a singleton type descriptor from a single enum. *)

  val mk_enums : Enums.t -> t
  (** Creates a type descriptor from an {!Sstt.Enums} component. *)

  val mk_interval : Intervals.Atom.t -> t
  (** Creates a type descriptor from a single interval. *)

  val mk_intervals : Intervals.t -> t
  (** Creates a type descriptor from an {!Sstt.Intervals} component. *)

  val mk_arrow : Arrows.Atom.t -> t
  (** Creates a type descriptor from a single arrow. *)

  val mk_arrows : Arrows.t -> t
  (** Creates a type descriptor from an {!Sstt.Arrows} component. *)

  val mk_record : Records.Atom.t -> t
  (** Creates a type descriptor from a single record. *)

  val mk_records : Records.t -> t
  (** Creates a type descriptor from an {!Sstt.Records} component. *)

  val mk_tuple : Tuples.Comp.Atom.t -> t
  (** Creates a type descriptor from a single tuple. *)

  val mk_tuplecomp : Tuples.Comp.t -> t
  (** Creates a type descriptor from a tuple component of a fixed arity tuple. *)

  val mk_tuples : Tuples.t -> t
  (** Creates a type descriptor from a {!Sstt.Tuples} component family, mixing several tuple arities. *)

  val mk_tag : Tags.Comp.Atom.t -> t
  (** Creates a type descriptor from a single tagged type. *)

  val mk_tagcomp : Tags.Comp.t -> t
  (** Creates a type descriptor from a tagged component of a fixed tag. *)

  val mk_tags : Tags.t -> t
  (** Creates a type descriptor from a {!Sstt.Tags} component family, mixing several tuple arities. *)

  (** {1 Misc. operations } *)

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* VDescr *)

module type VDescr = sig
  (** Full descriptors with top-level variables. *)

  (** {1 Basics }*)

  type t
  (** The type of descriptors with top-level variables (vdescr). It represents a union of intersections of
      positive and negative variables together with type descriptors of type {!Descr.t}:

      {math
      v = \bigcup_{i=i\ldots n} ~~\bigcap_{j=1\ldots m} \alpha_{ij} \cap \bigcap_{j=1\ldots l} \lnot\beta_{ij} ~~\cap~~ d_i
      }
  *)

  (**@inline*)
  include TyBase with type t := t

  (** {1 Descriptors } *)

  module Descr : Descr with type node := node

  val mk_var : Var.t -> t
  (** [mk_var v] builds a full descriptor from a given type variable. *)

  val mk_descr : Descr.t -> t
  (** [mk_descr d] creates a full descriptor from the monomorphic descriptor [d]. *)

  val get_descr : t -> Descr.t
  (** [get_descr t] extracts a monomorphic descriptor from [t],
      which describes [t] by ignoring its top-level type variables. *)

  (** {1 Explicit Disjunctive Normal Form }*)

  val dnf : t -> (Var.t, Descr.t) dnf
  (** Return an explicit DNF representation from a full descriptor. *)

  val of_dnf : (Var.t, Descr.t) dnf -> t
  (** Builds a full descriptor from a DNF. *)

  (** {1 Misc. operations }*)

  val map : (Descr.t -> Descr.t) -> t -> t
  (** [map f t] replaces every descriptor [d] in [t] by the descriptor [f d]. *)

  val map_nodes : (node -> node) -> t -> t
  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)

end

(* Nodes (internal signature, not exposed to the user) *)

(* Expose some additional internal VDescr methods,
   required for the recursive definition *)
module type VDescr' = sig
  include VDescr

  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val is_empty : t -> bool

  val simplify : t -> t
  val direct_nodes : t -> node list
  val direct_vars : t -> Var.t list
  val substitute : t VarMap.t -> t -> t
end

module type PreNode = sig
  type t
  type vdescr
  type descr

  include Comparable with type t:=t
  val def : t -> vdescr
  val of_def : vdescr -> t

  val mk_var : Var.t -> t
  val mk_descr : descr -> t
  val get_descr : t -> descr

  include SetTheoretic with type t := t
  val with_own_cache : ('a -> 'b) -> 'a -> 'b

  val vars : t -> VarSet.t
  val vars_toplevel : t -> VarSet.t
  val nodes : t -> t list

  val of_eqs : (Var.t * t) list -> (Var.t * t) list
  val substitute : t VarMap.t -> t -> t
  val factorize : t -> t
  val simplify : t -> unit

end
module type Node = sig
  include PreNode
  val any : t
  val empty : t
end

(* Ty *)

module type Ty = sig
  (** Set-theoretic types (type references).*)

  (** {1 Basics } *)

  (** @canonical Sstt.Ty.t *)
  type t
  (**
     The type of (set-theoretic) types. A value of type [t] is a reference to a
      descriptor that represent its structure (its variables,
      components, …).
     Each value of type {!t} contains a unique internal identifier which is used
     for equality
  *)

  val equal : t -> t -> bool
  (** [equal t1 t2] returns [true] if and only if [t1] and [t2] denote the same
      reference. It is provided e.g. to implement hash tables indexed by types
      and does {b not} implement syntactic nor semantic equality.
      The function runs in constant time.
  *)

  val compare : t -> t -> int
  (** [compare t1 t2] implements a total order on types, seen as references.
      [compare t1 t2 = 0] if and only if [equal t1 t2 = true].
      The function runs in constant time.
  *)

  val hash : t -> int
  (** [hash t] returns a hash for the given type. The function runs in constant time.*)

  (** {1 Set-theoretic operations }*)

  val empty : t
  (** The empty type, {%html: <span style='font-size:large'>𝟘</span>%}. *)

  val any : t
  (** The top type, {%html: <span style='font-size:large'>𝟙</span>%}. *)

  include SetTheoretic with type t := t

  (** {1 Full descriptors }

      The {!VDescr} module and its operations allow one to access the internal structure of a type.
  *)

  module VDescr : VDescr with type node := t

  val def : t -> VDescr.t
  (** [def t] returns the full descriptor of [t]. For a given type (reference) [t],
      [def t] is not necessarily constant: it may change over time, for instance
      when the descriptor of [t] is simplified. *)

  val of_def : VDescr.t -> t
  (** [of_def d] creates a type from the full descriptor [d]. *)

  val mk_var : Var.t -> t
  (** [mk_var v] creates a type from a type variable. *)

  val mk_descr : VDescr.Descr.t -> t
  (** [mk_descr d] creates a type from the monomorphic descriptor [d]. *)

  val get_descr : t -> VDescr.Descr.t
  (** [get_descr t] extracts a monomorphic descriptor from [t],
      which describes [t] by ignoring its top-level type variables.s *)

  (** {1 Misc. operations }*)

  val vars : t -> VarSet.t
  (** [vars t] returns the set of all variables in [t].
      Note that due to simplifications some variables may or may not be present.
      For instance,
      if {m t_1 \equiv \alpha } and {m t_2 \equiv \lnot \alpha}
  *)

  val vars_toplevel : t -> VarSet.t
  (** [vars_toplevel t] returns the top-level variables of [t], that is, occurrences
      of variables that are not below a constructor.
  *)

  val nodes : t -> t list
  (** [nodes t] returns all the nodes appearing in [t] (including [t] itself). *)

  val of_eqs : (Var.t * t) list -> (Var.t * t) list
  (** [of_eqs [(x1,t1);...;(xn,tn)]] returns the types [x1], ..., [xn]
      satisfying the system of equations [x1=t1], ..., [xn=tn].
      Raises: [Invalid_argument] if the set of equations is not contractive. *)


  val substitute : t VarMap.t -> t -> t
  (** [substitute s t] applies the type variable substitution [s] to [t]. *)

  val factorize : t -> t
  (** [factorize t] factorizes equivalent nodes in [t].
      This operation may be expensive since it calls {!equiv} internally.
  *)


  (** {1 Optional types }*)

  module O : sig
    (** Optional types are subsets of {%html: <span style='font-size:large'>𝟙</span>%}{m \cup\bot}. They are used for the type of
        record fields, to denote the fact that a field may be absent.
    *)

    type node = t
    (** An alias for the type {!Ty.t}. *)

    type t = node * bool
    (** The type of optional types. Whenever
        the boolean component is [true], it means that the type contains the
        undefined element {m \bot}. Otherwise, when the boolean component is [false],
        the type is equivalent to a plain {!Ty.t} type. *)


    include TyBase with type node := node and type t := t (** @inline *)


    val absent : t
    (** [absent] is the singleton type containing the undefined value, {m \bot}. *)

    val required : node -> t
    (** [required t] returns the type [t, false]. *)

    val optional : node -> t
    (** [optional t] returns [t, true] which represents the type {m t\cup \bot}. *)

    (** @inline *)
    include SetTheoretic with type t := t

    val is_absent : t -> bool
    (** Tests whether {m \bot \equiv t}. *)

    val is_required : t -> bool
    (** Tests whether {m \bot \not\in t}. *)

    val is_optional : t -> bool
    (** Tests whether {m \bot \in t}*)

    val get : t -> node
    (** Returns [get (t, b)] returns [t]. *)
  end

end