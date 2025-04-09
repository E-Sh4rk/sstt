open Base

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end


module type TyBase = sig
  type t
  val empty : t
  val any : t

end


module type SetTheoretic = sig
  type t
  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val conj : t list -> t
  val disj : t list -> t

  val is_empty : t -> bool
  val is_any : t -> bool
  val leq : t -> t -> bool
  val equiv : t -> t -> bool
  val disjoint : t -> t -> bool
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

module type Atoms = sig
  include TyBase with type t = Tdefs.atoms
  include Comparable with type t := t

  module Atom = Atoms.Atom
  val mk : Atom.t -> t

  val construct : bool * Atom.t list -> t

  (** [destruct t] returns a pair [(b,atoms)] such that:
      if [b] is true, then [t] contains exactly the atoms [atoms],
      and if [b] is false, then [t] contains exactly the atoms not in [atoms]. *)
  val destruct : t -> bool * Atom.t list
end

(* Intervals *)

module type IntervalAtom = sig
  (* TODO: Unused, remove and put the doc string elsewhere *)
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
  include TyBase with type t = Tdefs.intervals
  include Comparable with type t := t

  module Atom = Intervals.Atom
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

  include Comparable with  type t = Tdefs.arrow_atom

  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type Arrows = sig
  include TyBase with type t = Tdefs.arrows
  include Comparable with type t := t


  include Comparable with type t = Tdefs.arrows

  module Atom : ArrowAtom
  module Dnf : Dnf with type atom = Atom.t and type leaf = bool
  val mk : Atom.t -> t

  (** [dnf t] returns a disjunctive normal form of [t]. *)
  val dnf : t -> Dnf.t

  val of_dnf : Dnf.t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

(* Records *)

module type RecordAtom = sig
  type t = Tdefs.record_atom = { bindings : Tdefs.onode LabelMap.t ; opened : bool }
  include Comparable with type t := t

  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)
  val dom : t -> LabelSet.t

  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)
  val find : Label.t -> t -> Tdefs.onode
  val to_tuple : Label.t list -> t -> Tdefs.onode list
  val to_tuple_with_default : Label.t list -> t -> Tdefs.onode list
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type RecordAtom' = sig

  (** When the field [required] is equal to [Some labels],
      it means that [t] requires at least one field not in [labels] to be present. *)
  type t = Tdefs.record_atom' = { bindings : Tdefs.onode LabelMap.t ; opened : bool; required : LabelSet.t option }
  include Comparable with type t := t

  (** [dom t] returns the set of explicit labels in [t].
      Note that this does not mean that labels in [dom t] are present in
      the record values captured by [t]: even if a binding is present
      in [t], it could be associated with a possibly absent type. *)
  val dom : t -> LabelSet.t

  (** [find l t] returns the type associated with the label [l] in [t],
      even if [t] does not have an explicit binding for [l]. *)
  val find : Label.t -> t -> Tdefs.onode

end

module type Records = sig
  include TyBase with type t = Tdefs.records 
  include Comparable with type t := t

  module Atom : RecordAtom
  module Atom' : RecordAtom'
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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

(* Tuples *)

module type TupleAtom = sig
  type t = Tdefs.tuple_atom
  include Comparable with type t := t
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type TupleComp = sig
  include Comparable with type t = Tdefs.tuple_comp

  module Atom : TupleAtom
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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type Tuples = sig
  include TyBase with type t = Tdefs.tuples
  include Comparable with type t := t

  module TupleComp : TupleComp
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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

(* Tags *)

module type TagAtom = sig
  module Tag = Tdefs.Tag
  type t = Tdefs.tag_atom
  include Comparable with type t := t
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type TagComp = sig
  include Comparable with type t = Tdefs.tag_comp
  module Atom : TagAtom
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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

module type Tags = sig
  include TyBase with type t = Tdefs.tags
  include Comparable with type t := t

  module TagComp : TagComp
  module TMap : Map.S with type key = TagComp.Tag.t

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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

(* Descr *)

module type Descr = sig
  include TyBase with type t = Tdefs.descr
  include Comparable with type t := t

  module Arrows : Arrows
  module Atoms : Atoms
  module Intervals : Intervals
  module Records : Records
  module Tags : Tags
  module Tuples : Tuples

  type component =
    | Atoms of Atoms.t
    | Arrows of Arrows.t
    | Intervals of Intervals.t
    | Records of Records.t
    | Tags of Tags.t
    | Tuples of Tuples.t

  val any : t 
  val empty : t

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
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t
end

(* VDescr *)

module type VDescr = sig
  include TyBase with type t = Tdefs.vdescr
  include Comparable with type t := t

  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val is_empty : t -> bool
  val leq : t -> t -> bool
  val equiv : t -> t -> bool

  module Descr : Descr
  module Dnf : Dnf with type atom = Var.t and type leaf = Tdefs.descr

  val mk_var : Var.t -> t

  (** [mk_descr d] creates a full descriptor from the monomorphic descriptor [d]. *)
  val mk_descr : Tdefs.descr -> t

  (** [get_descr t] extracts a monomorphic descriptor from [t],
      which describes [t] by ignoring its top-level type variables. *)
  val get_descr : t -> Tdefs.descr

  (** [map f t] replaces every descriptor [d] in [t] by the descriptor [f d]. *)
  val map : (Tdefs.descr -> Tdefs.descr) -> t -> t

  (** [map_nodes f t] replaces every node [n] in [t] by the node [f n]. *)
  val map_nodes : (Tdefs.node -> Tdefs.node) -> t -> t

  val dnf : t -> Dnf.t
  val of_dnf : Dnf.t -> t

  val simplify : t -> t
  val direct_nodes : t -> Tdefs.node list
  val direct_vars : t -> Var.t list

  val substitute : t VarMap.t -> t -> t
end

(* Nodes *)

module type Node = sig

  include Comparable with type t = Tdefs.node
  include SetTheoretic with type t := t

  val def : t -> Tdefs.vdescr
  val of_def : Tdefs.vdescr -> t

  val mk_var : Var.t -> t
  val mk_descr : Tdefs.descr -> t
  val get_descr : t -> Tdefs.descr

  val with_own_cache : ('a -> 'b) -> 'a -> 'b

  val vars : t -> VarSet.t
  val vars_toplevel : t -> VarSet.t
  val nodes : t -> t list

  val of_eqs : (Var.t * t) list -> (Var.t * t) list
  val substitute : t VarMap.t -> t -> t
  val simplify : t -> unit

  val hash : t -> int
end

(* Ty *)

module type Ty = sig
  include TyBase with type t = Tdefs.node
  include Comparable with type t := t

  module O : sig
    include TyBase with type t = Tdefs.onode 
    include Comparable with type t := t
    include SetTheoretic with type t := t
    val absent : t
    val required : Tdefs.node -> t
    val optional : Tdefs.node -> t    
    val is_absent : t -> bool
    val is_required : t -> bool
    val is_optional : t -> bool
  end

  (** [def t] returns the full descriptor of [t]. For a given type [t],
      [def t] is not necessarily constant: it may change over time, for instance
      when the descriptor of [t] is simplified. *)
  val def : t -> Tdefs.vdescr

  (** [of_def d] creates a type from the full descriptor [d]. *)
  val of_def : Tdefs.vdescr -> t

  val mk_var : Var.t -> t

  (** [mk_descr d] creates a type from the monomorphic descriptor [d]. *)
  val mk_descr : Tdefs.descr -> t

  (** [get_descr t] extracts a monomorphic descriptor from [t],
      which describes [t] by ignoring its top-level type variables. *)
  val get_descr : t -> Tdefs.descr

  include SetTheoretic with type t := t

  val vars : t -> VarSet.t
  val vars_toplevel : t -> VarSet.t

  (** [nodes t] returns all the nodes appearing in [t] (including [t] itself). *)
  val nodes : t -> t list

  (** [of_eqs [(x1,t1);...;(xn,tn)]] returns the types [x1], ..., [xn]
      satisfying the system of equations [x1=t1], ..., [xn=tn].
      Raises: [Invalid_argument] if the set of equations is not contractive. *)
  val of_eqs : (Var.t * t) list -> (Var.t * t) list

  (** [substitute s t] applies the type variable substitution [s] to [t]. *)
  val substitute : t VarMap.t -> t -> t

  val hash : t -> int
end