open Base

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type TyBase = sig
  type t
  type node

  val any : t
  val empty : t

  include Comparable with type t := t
end

module type TyBaseRef = sig
  type t
  type node

  val any : unit -> t
  val empty : unit -> t

  include Comparable with type t := t
end

module type SetTheoretic = sig
  type t
  val cap : t -> t -> t
  (** [cap t1 t2] is the intersection {m t1 ∧ t2}. *)

  val cup : t -> t -> t
  (** [cup t1 t2] is the union {m t1 ∧ t2}. *)

  val diff : t -> t -> t
  (** [diff t1 t2] is the difference {m t1 \ t2}. *)

  val neg : t -> t
  (** [neg t] is the negation of [t], {m ¬t}.*)

  val conj : t list -> t
  (** [conj l] is the intersection of all the types in [l]. It returns [any] if
      [l] is the empty list.
  *)

  val disj : t list -> t
  (** [disj l] is the union of all the types in [l]. It returns [empty] if
      [l] is the empty list.
  *)

  val is_empty : t -> bool
  (** [is_empty t] returns [true] if and only if [t] is semantically equivalent
      to [empty]. *)

  val is_any : t -> bool
  (** [is_any t] returns [true] if and only if [t] is semantically equivalent
      to [any]. *)

  val leq : t -> t -> bool
  (** [leq t1 t2] returns [true] if and only if [t1] is a subtype of [t2]. *)

  val equiv : t -> t -> bool
  (** [equiv t1 t2] returns [true] if and only if [leq t1 t2] and [leq t2 t1]. *)

  val disjoint : t -> t -> bool
  (** [disjoint t1 t2] returns true if and only if [cap t1 t2] is empty. *)
end

(* DNF *)

module type Dnf = sig
  type atom
  type leaf

  (** [t] represents a disjunctive normal form, that is, a disjunction of clauses.
      For leaf components, [leaf] is a boolean that should be true.
      If set to false, the corresponding clause will be ignored. *)
  type t = (atom list * atom list * leaf) list

  (** [simplify t] removes from [t] useless clauses and summands.
      In particular, [simplify t] is ensured not to contain any [empty] summand
      nor [any] clause. *)
  val simplify : t -> t
end

module type Dnf' = sig
  type atom
  type leaf

  (** [t] represents a condensed disjunctive normal form, that is,
      a disjunction of atoms. [leaf] is a boolean that should be true.
      If set to false, the corresponding atom will be ignored. *)
  type t = (atom * leaf) list

  (** [simplify t] removes from [t] useless summands.
      In particular, [simplify t] is ensured not to contain any [empty] summand. *)
  val simplify : t -> t
end

(* Atoms *)

module type AtomAtom = Id.NamedIdentifier

module type Atoms = sig
  include TyBase
  module Atom : AtomAtom
  val mk : Atom.t -> t

  val construct : bool * Atom.t list -> t

  (** [destruct t] returns a pair [(b,atoms)] such that:
      if [b] is true, then [t] contains exactly the atoms [atoms],
      and if [b] is false, then [t] contains exactly the atoms not in [atoms]. *)
  val destruct : t -> bool * Atom.t list
end

(* Intervals *)

module type IntervalAtom = sig
  (** [t] represents a non-empty integer interval. *)
  type t

  (** [mk b1 b2] creates an interval from bound [b1]
      (inclusive, -infinity if [None]) to bound [b2]
      (inclusive, +infinity if [None]).
      Raises: [Invalid_argument] if the interval is empty. *)
  val mk : Z.t option -> Z.t option -> t

  (** [mk_bounded i1 i2] creates an interval from bound [i1]
      (inclusive) to bound [i2] (inclusive).
      Raises: [Invalid_argument] if the interval is empty. *)
  val mk_bounded : Z.t -> Z.t -> t

  (** [mk_singl i] creates an interval containing exactly [i]. *)
  val mk_singl : Z.t -> t

  (** [get t] returns the boundaries (inclusive) of the interval [t]. *)
  val get : t -> Z.t option * Z.t option

  include Comparable with type t := t
  val pp : Format.formatter -> t -> unit
end

module type Intervals = sig
  include TyBase
  module Atom : IntervalAtom
  val mk : Atom.t -> t
  val construct : Atom.t list -> t
  val destruct : t -> Atom.t list

  (** [destruct_neg t] destructs the negation of [t].
      The negation of [t] is sometimes simpler than [t] itself,
      which may justify working on this negative form
      (for instance when pretty-printing). *)
  val destruct_neg : t -> Atom.t list
end

(* Arrows *)

module type ArrowAtom = sig
  type node
  type t = node * node
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type Arrows = sig
  include TyBase
  module Atom : ArrowAtom with type node := node
  module Dnf : Dnf with type atom = Atom.t and type leaf = bool
  val mk : Atom.t -> t

  (** [dnf t] returns a disjunctive normal form of [t]. *)
  val dnf : t -> Dnf.t

  val of_dnf : Dnf.t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* Records *)

type 'node oty = 'node * bool

module type RecordAtom = sig
  type node
  type nonrec oty = node oty

  type t = { bindings : oty LabelMap.t ; opened : bool }

  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)
  val dom : t -> LabelSet.t

  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)
  val find : Label.t -> t -> oty

  val to_tuple : Label.t list -> t -> oty list
  val to_tuple_with_default : Label.t list -> t -> oty list
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type RecordAtom' = sig
  type node
  type nonrec oty = node oty

  (** When the field [required] is equal to [Some labels],
      it means that [t] requires at least one field not in [labels] to be present. *)
  type t = { bindings : oty LabelMap.t ; opened : bool ; required : LabelSet.t option }

  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)
  val dom : t -> LabelSet.t

  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)
  val find : Label.t -> t -> oty

  include Comparable with type t := t
end

module type Records = sig
  include TyBase
  module Atom : RecordAtom with type node := node
  module Atom' : RecordAtom' with type node := node
  module Dnf : Dnf with type atom = Atom.t and type leaf = bool
  module Dnf' : Dnf' with type atom = Atom'.t and type leaf = bool
  val mk : Atom.t -> t

  (** [dnf t] returns a disjunctive normal form of [t]. *)
  val dnf : t -> Dnf.t

  (** [dnf' t] returns a condensed disjunctive form of [t]
      where each clause is a positive literal. *)
  val dnf' : t -> Dnf'.t

  val of_dnf : Dnf.t -> t
  val of_dnf' : Dnf'.t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* Tuples *)

module type TupleAtom = sig
  type node
  type t = node list
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type TupleComp = sig
  include TyBase
  module Atom : TupleAtom with type node := node
  module Dnf : Dnf with type atom = Atom.t and type leaf = bool
  module Dnf' : Dnf' with type atom = Atom.t and type leaf = bool
  val any : int -> t
  val empty : int -> t
  val mk : Atom.t -> t

  (** [len t] returns the cardinality of tuples in [t]. *)
  val len : t -> int

  (** [dnf t] returns a disjunctive normal form of [t]. *)
  val dnf : t -> Dnf.t

  (** [dnf' t] returns a condensed disjunctive form of [t]
      where each clause is a positive literal. *)
  val dnf' : t -> Dnf'.t

  val of_dnf : int -> Dnf.t -> t
  val of_dnf' : int -> Dnf'.t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

module type Tuples = sig
  include TyBase
  module TupleComp : TupleComp with type node := node
  val mk : TupleComp.Atom.t -> t
  val mk_comp : TupleComp.t -> t

  (** [components t] returns a pair [(cs,b)] where [cs] are the tuple components
      explicitely present in [t], and [b] is a boolean indicating whether components
      of other cardinalities are [any] (if [b] is [true]) or [empty] (if [b] is [false]). *)
  val components : t -> TupleComp.t list * bool

  val of_components : TupleComp.t list * bool -> t

  (** [get n t] returns the tuple component of cardinality [n] in [t]. *)
  val get : int -> t -> TupleComp.t

  (** [map f t] replaces every tuple component [p] in [t] by [f p]. *)
  val map : (TupleComp.t -> TupleComp.t) -> t -> t

  val construct : bool * TupleComp.t list -> t

  (** [destruct t] returns a pair [(b,cs)] such that:
      if [b] is true, then [t] contains exactly the tuple components [cs],
      and if [b] is false, then the negation of [t] contains exactly
      the tuple components [cs]. *)
  val destruct : t -> bool * TupleComp.t list

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* Tags *)

module type TagAtom = sig
  type node
  module Tag : Id.NamedIdentifier
  type t = Tag.t * node
  include Comparable with type t := t
  val map_nodes : (node -> node) -> t -> t
end

module type TagComp = sig
  include TyBase
  module Atom : TagAtom with type node := node
  module Tag = Atom.Tag
  module Dnf : Dnf with type atom = Atom.t and type leaf = bool
  val any : Tag.t -> t
  val empty : Tag.t -> t
  val mk : Atom.t -> t

  (** [tag t] returns the tag of the component [t]. *)
  val tag : t -> Tag.t

  (** [dnf t] returns a disjunctive normal form of [t]. *)
  val dnf : t -> Dnf.t

  val of_dnf : Tag.t -> Dnf.t -> t

  (** [as_atom t] returns an atom equivalent to [t]. *)
  val as_atom : t -> Atom.t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

module type Tags = sig
  include TyBase
  module TagComp : TagComp with type node := node
  val mk : TagComp.Atom.t -> t
  val mk_comp : TagComp.t -> t

  (** [components t] returns a pair [(cs,b)] where [cs] are the tag components
      explicitely present in [t], and [b] is a boolean indicating whether components
      of other tags are [any] (if [b] is [true]) or [empty] (if [b] is [false]). *)
  val components : t -> TagComp.t list * bool

  val of_components : TagComp.t list * bool -> t

  (** [get tag t] returns the tag component [tag] in [t]. *)
  val get : TagComp.Tag.t -> t -> TagComp.t

  (** [map f t] replaces every tags component [c] in [t] by [f c]. *)
  val map : (TagComp.t -> TagComp.t) -> t -> t

  val construct : bool * TagComp.t list -> t

  (** [destruct t] returns a pair [(b,cs)] such that:
      if [b] is true, then [t] contains exactly the components [cs],
      and if [b] is false, then the negation of [t] contains exactly
      the components [cs]. *)
  val destruct : t -> bool * TagComp.t list

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* Descr *)

module type Descr = sig
  include TyBase

  module Arrows : Arrows with type node := node
  module Atoms : Atoms with type node := node
  module Intervals : Intervals with type node := node
  module Records : Records with type node := node
  module Tags : Tags with type node := node
  module Tuples : Tuples with type node := node

  type component =
    | Atoms of Atoms.t
    | Arrows of Arrows.t
    | Intervals of Intervals.t
    | Records of Records.t
    | Tags of Tags.t
    | Tuples of Tuples.t

  val mk_atom : Atoms.Atom.t -> t
  val mk_atoms : Atoms.t -> t
  val mk_tag : Tags.TagComp.Atom.t -> t
  val mk_tagcomp : Tags.TagComp.t -> t
  val mk_tags : Tags.t -> t
  val mk_tuple : Tuples.TupleComp.Atom.t -> t
  val mk_tuplecomp : Tuples.TupleComp.t -> t
  val mk_tuples : Tuples.t -> t
  val mk_arrow : Arrows.Atom.t -> t
  val mk_arrows : Arrows.t -> t
  val mk_record : Records.Atom.t -> t
  val mk_records : Records.t -> t
  val mk_interval : Intervals.Atom.t -> t
  val mk_intervals : Intervals.t -> t

  val get_atoms : t -> Atoms.t
  val get_tags : t -> Tags.t
  val get_tuples : t -> Tuples.t
  val get_arrows : t -> Arrows.t
  val get_records : t -> Records.t
  val get_intervals : t -> Intervals.t

  val components : t -> component list
  val set_component : t -> component -> t
  val of_component : component -> t
  val of_components : component list -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t
end

(* VDescr *)

module type VDescr = sig
  include TyBase

  module Descr : Descr with type node := node
  module Dnf : Dnf with type atom = Var.t and type leaf = Descr.t

  val mk_var : Var.t -> t

  (** [mk_descr d] creates a full descriptor from the monomorphic descriptor [d]. *)
  val mk_descr : Descr.t -> t

  (** [get_descr t] extracts a monomorphic descriptor from [t],
      which describes [t] by ignoring its top-level type variables. *)
  val get_descr : t -> Descr.t

  (** [map f t] replaces every descriptor [d] in [t] by the descriptor [f d]. *)
  val map : (Descr.t -> Descr.t) -> t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (node -> node) -> t -> t

  val dnf : t -> Dnf.t
  val of_dnf : Dnf.t -> t
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
  val simplify : t -> t

  val hash : t -> int
end
module type Node = sig
  include PreNode
  val any : t
  val empty : t
end
(* Ty *)

module type Ty = sig
  (** Set-theoretic types.*)

  type t
  (** The type of (set-theoretic) types. A value of type [t] is a reference to a
      descriptor that reflects the structure of the type (its variables,
      components, …).
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

  val empty : t
  (** The empty type, {m 𝟘}. *)

  val any : t
  (** The top type, {m 𝟙}. *)

  module VDescr : VDescr with type node := t
  (** Type descriptors. *)

  module O : sig
    (** Optional types are subsets of {m 𝟙∨⊥}. They are used for the type of
        record fields, to denote the fact that a field may be absent.
    *)

    type node = t
    (** An alias for the type {!Ty.t}. *)

    type t = node oty
    (** The type of optional types. [t] is equal to [(node * bool)]. Whenever
        the boolean component is [true], it means that the type contains the
        absent element {m ⊥}. Otherwise, when the boolean component is [false],
        the type is equivalent to a plain {!Ty.t} type. *)


    include TyBase with type node := node and type t := t (** @inline *)


    val absent : t
    (** [absent] is the singleton type containing the absent value, {m ⊥}. *)

    val required : node -> t
    (** [required t] returns the type [t] (not absent). *)

    val optional : node -> t
    (** [optional t] returns the type {m t∨⊥}. *)

    include SetTheoretic with type t := t
    val is_absent : t -> bool
    val is_required : t -> bool
    val is_optional : t -> bool
    val get : t -> node
  end
  (** Optional types. *)

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
      which describes [t] by ignoring its top-level type variables. *)

  include SetTheoretic with type t := t

  val vars : t -> VarSet.t
  (** [vars t] returns the set of all variables in [t].
      Note that due to simplifications some variables may or may not be present.
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

end