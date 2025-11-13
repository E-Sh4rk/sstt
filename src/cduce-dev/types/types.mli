open Ident
open Encodings

(** {2 Algebra }

  ℂDuce types can be built using to class of operation : type constructors
  (products, arrows, records, …) and type connectives (union, intersection,
  difference). Types must have two properties :
  - {i regularity }: an infinite (co-inductive) type must have finite number
  of distinct subtrees.
  - {i contractivity}: an infinite (co-inductive) type must crosse inifinitely
  many type {b constructors}.

  These invariants are enforced by the operations in this module.
*)

type descr
(** The type of ℂDuce types, also aliased as [t]. *)

include Custom.T with type t = descr
(** Basic operations on [t]. Note that equality here denotes the operation on
the structural equality on the internal representation of [t], not the semantic
notion of equivalence between ℂDuce types. *)

(** A [Node.t] represent a particular reference to a type. Such reference are
  used to build recursive types. Type connective work on types while type
  constructors work on references (thus enforcing the invariants of recursive
  types).
*)

module Node : Custom.T

val make : unit -> Node.t
(** [make ()] creates a fresh type reference, pointing to the empty type. *)

val define : Node.t -> t -> unit
(** [define n t] updates a reference to set its content to the given type.
    This can be used to define recursive types :
    {[
      let nil = Types.atom (AtomSet.atom (AtomSet.V.mk_ascii "nil")) in
      let x = Types.make () in
      let prod = Types.times (Types.cons Types.Int.any) ilist
      let ilist = Types.cup nil prod in
      define x ilist;
      (* use ilist or x, they represent the same recursive type [ [Int*] ] *)
    ]}

*)

val cons : t -> Node.t
(** [const t] wraps the type [t] in a reference *)

val is_opened : Node.t -> bool
(** [is_opened t] returns whether is node has been defined *)

(**/**)

val internalize : Node.t -> Node.t

(**/**)

val id : Node.t -> int
(** [id n] returns the internal identifier of the reference. *)

val descr : Node.t -> t
(** [descr n] extract the type boxed in the reference [n]. *)

(** {2 Boolean connectives} *)

val empty : t
(** The empty type, 𝟘. *)

val any : t
(** The top type, 𝟙 *)

val var : Var.t -> t
(** [var x] creates the polymorphic variable [x]. *)

val cup : t -> t -> t
(** [cup t1 t2] returns the union of [t1] and [t2] *)

val cap : t -> t -> t
(** [cap t1 t2] returns the intersection of [t1] and [t2] *)

val diff : t -> t -> t
(** [diff t1 t2] returns the difference of [t1] and [t2] *)

val neg : t -> t
(** [neg t] returns the complement of [t] *)

(** {2 Subtyping }

Subtyping operations should only be called on fully defined types (that is,
not on a recursive type whose cycles haven't yet been closed with {!define}).

*)

val is_empty : t -> bool
(** [is_empty t] returns [true] if and only if [t] is empty. *)

val non_empty : t -> bool
(** [non_empty t] returns [true] if and only if [t] is non empty empty.
    This is just a convenience function for [not(is_empty t)].
*)

val subtype : t -> t -> bool
(** [subtype t1 t2] returns [true] if and only if [t1] is a subtype of [t2].
    This is an alias for [is_empty (diff t2 t1)].
*)

val disjoint : t -> t -> bool
(** [disjoint t1 t2] returns [true] if and only if [t1] and [t2] have an
    empty intersection. This is an alias for [is_empty (cap t1 t2)].
*)

val equiv : t -> t -> bool
(** [equiv t1 t2] returns [true] if and only if [t1] and [t2] are semantically
  equal. This is an alias for [subtype t1 t2 && subtype t2 t1].
*)

(** {2 Components of a type }

  A type in ℂDuce is a finite union of disjoint kinds :
  - Integers
  - Atoms
  - Characters
  - Abstract types
  - Products
  - XML products
  - Arrows
  - Records
  - Absent type (representing optional record fields.)

  The following modules allow one to project a particular component of a type,
  retrieve its dijunctive normal form (DNF) and conversely create a ℂDuce type
  from such a DNF. These modules also define the top type of each kind.
*)

(** The components of a type *)
module type Kind = sig
  module Dnf : Bdd.S
  (** A representation of the disjunctive normal form (DNF) and related
    operations for this component. *)

  val any : t
  (** The top type of this component. *)

  val get : t -> Dnf.mono
  (** [get t] extracts the monomorphic part of this component. *)

  val get_vars : t -> Dnf.t
  (** [get t] extracts the DNF of this component. *)

  val mk : Dnf.t -> t
  (** [mk d] creates a type from a DNF of this kind. *)

  val update : t -> Dnf.t -> t
  (** [update t d] replaces the component in [t] with [d].
    Equivalent (but slightly more efficient than)
    [cup (diff t K.any) (K.mk d)].
  *)
end

(** {3 Integers }*)

module Int : Kind with module Dnf = Bdd.VarIntervals
(** access the integer component of a type *)

val interval : Intervals.t -> t
(** [interval i] creates a type from an interval [i]. *)

(** {3 Atoms }*)

module Atom : Kind with module Dnf = Bdd.VarAtomSet
(** Access the atom component of a type *)

val atom : AtomSet.t -> t
(** [atom a] creates a type from an [AtomSet.t]. *)

(** {3 Characters }*)

module Char : Kind with module Dnf = Bdd.VarCharSet
(** Access the char component of a type. *)

val char : CharSet.t -> t

(** [char c] creates a type from a [CharSet.t]. *)

(** {3 Abstract types }*)

module Abstract : Kind with module Dnf = Bdd.VarAbstractSet
(** Access the abstract component of a type. *)

val abstract : AbstractSet.t -> t
(** [abstract a] creates a type from an [AbstractSet.t]. *)

val get_abstract : t -> AbstractSet.t
(** [get_abstract a] is an alias for [Abstract.get a]. *)

(** {3 Products }*)

(** Access the product component of a type. *)
module Times :
  Kind
    with type Dnf.atom = Node.t * Node.t
     and type Dnf.line = (Node.t * Node.t) Bdd.line
     and type Dnf.dnf = (Node.t * Node.t) Bdd.dnf

val times : Node.t -> Node.t -> t
(** [times n1 n2] is an alias for [Times.mk (Times.Dnf.atom (n1, n2))]. *)

val tuple : Node.t list -> t
(** [tuple nl] creates right nested tuples from the elements in [nl]. [tuple
    [t1; t2; t3; … ; tn] ] returns the type [(t1, (t2, (t3, … (tn-1,tn))))]

  @raise Failure if [nl] is not at least of length [2].
 *)

(** {3 XML products }*)

module Xml : Kind with module Dnf = Times.Dnf
(** Access the XML component of a type.
    Note that [Xml.any] is different from the [AnyXml] type of ℂDuce, which is
    a recursive type [X where X = <_ ..>[(X|Char)*]]
*)

val xml : Node.t -> Node.t -> t
(** [xml n1 n2] is an alias for [Xml.mk (Xml.Dnf.atom (n1, n2))].
  Although this constructor can be freely called, it is expected that
  [n1] is a subtype of [Atom] and [n2] a subtype of [({ ..}, [Any*])], to
  represent the type of XML documents.
*)

(** {3 Records }*)

module Rec :
  Kind
    with type Dnf.atom = bool * Node.t label_map
     and type Dnf.line = (bool * Node.t label_map) Bdd.line
     and type Dnf.dnf = (bool * Node.t label_map) Bdd.dnf

(** Access the record component of a type. A record is represented as a pair of
    a boolean indicating whether the record is open ([true]) or closed ([false])
    and a [label_map] {!Ident.LabelMap}. Optional fields are fields whose
    [Absent] component is [Absent.any].
*)

val record_fields : bool * Node.t label_map -> t
(** [record_fiels (b, lm)] is an alias for
    [Rec.mk (Rec.Dnf.atom (b, lm))]. If [b] is [true] the resulting
    record type is open, otherwise it is closed. See {!Ident.LabelMap} to build
    the [lm] argument.

    Example :
    {[
      open Types
      let int = Int.any in
      let int_opt = cup int Absent.any in
      let lab = Ident.Label.mk_ascii "mylabel" in
      let lm = Ident.LabelMap.singleton lab int_opt in
      let r = record_fields (true, lm) in
      (* r is the record { mylabel=?Int ..} *)
    ]}

*)

val record : label -> Node.t -> t
(** [record l n] creates an {b open} record type with a single explicit
    label [l].
*)

val rec_of_list : bool -> (bool * label * t) list -> t
(** [rec_of_list op fields] creates a record type. If [op] is [true] the record
    is open otherwise it is closed. The [fields] argument is a list of triples
    [(opt, lab, t)]. The boolean [opt] indicate whether the field is optionnal
    (when [opt = true]) (thus the type [t] given for a field is expected to not
    be absent already).

    @raise [Failure] if [fields] contains duplicate labels.
*)

val empty_closed_record : t
(** The type [ {} ], that is a closed record with no fields. *)

val empty_open_record : t
(** The type [{ ..}], that is an opened record with no explicit fields.
    This is an alias for [Rec.any].
*)

(** {3 Arrows }*)

module Function : Kind with module Dnf = Times.Dnf
(** Access the arrow component of a type. *)

val arrow : Node.t -> Node.t -> t
(** [arrow n1 n2] is an alias for [Function.mk (Function.Dnf.atom (n1, n2))]. *)

(** {3 Absent types }

  Absent types represent the type of optional fields. They are represented by
  a kind that only has two values [empty ≡ false] and [any ≡ true].

  It is the programmer's responsibility to prevent optional absent types from
  appearing outside of record fields.
*)

(** Access the absent component of a type. *)
module Absent : sig
  module Dnf = Bdd.Bool

  val get : t -> bool
  val any : t
  val mk : bool -> t
  val update : t -> bool -> t
end
(** {2 Constants } *)

(** An auxiliary type to denote syntactic constants and build singleton types
  from its values. *)
type const =
  | Integer of Intervals.V.t
  | Atom of AtomSet.V.t
  | Char of CharSet.V.t
  | Pair of const * const
  | Xml of const * const
  | Record of const label_map
  | String of Utf8.uindex * Utf8.uindex * Utf8.t * const

module Const : Custom.T with type t = const
(** The [const] type equiped with basic operations. *)

val constant : const -> t
(** [constant c] returns a (singleton) type built from the constant description
    [c].*)

(** Constructors **)

type pair_kind =
  [ `Normal
  | `XML
  ]

(** {2 Normalization}

  Type constructors may yield equivalent types that are not structurally equal.
  For instance, the type [t = (A, B) | (A, C)] may also be represented as
  [t = (A, B|C)]. In general, given a union of products, its decomposition is
  not unique.

  The following modules allow one to compute various normalized forms for
  the product, XML, record and arrow type constructor.

  These modules also provide the typing for high level operations such as first
  and second projection for products, field projection and concatenation for
  records and function application.
*)

(** Product normalization. *)
module Product : sig
  type t = (descr * descr) list
  (** A product as a union non-empty rectangles.
      This corresponds to the DNF where the negative products have been
      subtracted from each positive one, and empty rectangles have been removed.

      Note that there may still be redundent rectangles.
  *)

  val partition : descr -> Times.Dnf.t -> t
  (** [partition any_right dnf] simplifies the union of intersection
      of positive and negative products rectangles given by [dnf]
      subtracting the negative products from each positive ones.
  *)

  val get : ?kind:pair_kind -> descr -> t
  (** [get ~kind:k t] returns a set of non-empty rectangles for either
    regular products or XML products. In case of XML products, the second
    component of the rectangle is restricted to [(Any, Any)], since in ℂDuce,
    XML values are encoded  as a product whose first projection is the tag and
    whose second projection is the pair of its attributes (a record) and its
    content (a sequence).
  *)

  val pi1 : t -> descr
  (** [pi1 rl] returns the union of the first components of the rectangles in
    [rl]. *)

  val pi2 : t -> descr
  (** [pi2 rl] returns the union of the second components of the rectangles in
    [rl]. *)

  val pi2_restricted : descr -> t -> descr
  (** [pi2_restricted t rl] returns the union of the second components of
      [rl] whose first component intersects [t].
  *)

  val restrict_1 : t -> descr -> t
  (** [restrict_1 [rl] t] intersects each rectangle in [rl] with [(t, Any)] and
    remove empty products.*)

  val merge_same_first : t -> t
  (** [merge_same_first rl] returns the a list of rectangles where rectangles
      with the same first components are merged, using the equivalence
      [(A, B) | (A, C) = (A, B|C)].
   *)

  val is_empty : t -> bool
  (** [is_empty rl] returns [true] if and only if the list of rectangle [rl] is
      empty.
  *)

  type normal = t
  (** This type represents a list of rectangles whose first component are
    pairwise disjoint.
    The following function expect such a normlized type, but this is not
    enforced by the API (i.e. if an arbitrary list of rectangle is given)
    the results are undefined behaviour.
  *)

  val normal : ?kind:pair_kind -> descr -> normal
  (** [normal ~kind:k t] returns a set of normalized rectangles for either
    regular products or XML products.
  *)

  val constraint_on_2 : normal -> descr -> descr
  (* [constraint_on_2 n t1] returns the largest [t2] such that (t1,t2) is
     a subtype of [n]. Assumes [t1] is a subtype of [pi1 n]. *)

  val need_second : normal -> bool
  (** [need_second n] returns [true] if and only if the decomposition [n]
    as more than one rectangle.
  *)

  val clean_normal : normal -> t
  (** [clean_normal n] merges rectangles with the same second component. *)
end

(** Record normalization. *)
module Record : sig
  val or_absent : t -> t
  (** [or_absent t] adds the absent component to the type.
  This is an alias for [cup t Absent.any] or [Absent.update t true]. *)

  val any_or_absent : t
  (** The top type with the absent component set. *)

  val has_absent : t -> bool
  (** [has_absent t] tests the value of the absent component. Equivalent to
    [non_empty (cap t Absent.any)], but slightly more efficient. *)

  val has_record : t -> bool
  (** [has_record t] returns [true] if and only if the record component of [t]
      is not empty. This is an alias for [non_empty (cap Rec.any t)]. *)

  val split : t -> label -> Product.t
  (** [split t l] returns a list of pair of types.
      For each pair, the first component
      is the type of the label [l] in [t] and the second component is the type
      of the remaining labels in [t], given as a record type.
  *)

  val split_normal : t -> label -> Product.normal
  (** [split_normal t l] returns the same list as [split t l] where the first
    components, that is, the types associated with [l] are pairwise disjoint.
  *)

  val project : t -> label -> t
  (** [project t l] returns the type associted with the label [l] in the record
      component of type [t]. It is equivalent to :
    - computing [n = split t l]
    - computing [s], the union of the first components of the list [n]
    - checking whether the absent type is present in [s]

    @raise Not_found if the
      resulting type may is absent (meaning that [l] is not necessarily present
      in [t]).
    *)

  val project_opt : t -> label -> t
  (** [project_opt t l] is similar to [project t l] but returns the type and
      erase the absent component if it is present. In other words, this function
      returns the the union of all the types associated to [l] for the records
      where it is present.
  *)

  val has_empty_record : t -> bool
  (** [has_empty_record t] returns [true] if and only if the record component of
    [t] contains a record with no label explicitely present for sure (that is
    there is a record [r] in [t] which has no explicit label (open or closed),
    or for which all explicit labels are associated with an absent type).
    [{ }], [{ ..}], [{ a=?Int b=?Any ..}] are examples of such record types.
  *)

  val first_label : t -> label
  (** [first_label t] returns the first label (in the total ordering of label
      names) to appear explicitely in any of the records present in [t]. The
      function returns [Label.dummy] if no such label exists.
  *)

  val empty_cases : t -> bool * bool
  (** [has_empty_cases t] must only be called on types whose record component
      have no explicit labels. It returns a pair of boolean [(some, none)],
      where:
    - [some] is [true] if and only if the record component is open
    - [none] is [true] if and only if the type is non empty.

      In otherwords :
    - [true, true] indicates that the record component of [t] contains [{ ..}]
    - [false, true] indicates that the record component of [t] contains only
      [{}]
    - [false, false] indicates that the record component of [t] is empty.
    - [true, false] is not possible.
  *)

  val merge : t -> t -> t
  (** [merge t1 t2] discards the non record component of [t1] and [t2] and
    returns the type of the [+] operators on records, that is, returns a record
    type [t] where the type of label [l] is the one of [t2] if it is present in
    [t2] and the type of [t1] otherwise.
  *)

  val remove_field : t -> label -> t
  (** [remove_field t l] returns the type of type [t] where the label [l] is
      marked as {i explicitely absent} from [t]. There are two ways to do that:
    - for closed record types, the label [l] is simply removed, if present
    - for open record types, the type is intersected with [{l=?Empty}] meaning
      that either the label [l] is absent, or if present it is associated to the
      empty type, that is, if it is present, the whole record is empty.
  *)

  val get : t -> ((bool * t) label_map * bool * bool) list
  (** [get t] returns a list whose union is the record component of [t]. Each
    element of the list is a triple [(map, op, none)] where [map] is a map from
    labels to pairs of a boolean indicating whether the label is optional or not
    and t the associated (non absent) type. The boolean [op] indicates whether
    the record is open. The boolean [none] is always [true] and is here for
    compatibility reasons with other parts of the ℂDuce compiler which return
    a similar type where [none] can be [false], indicating an empty component.
    Such components are not returned by [get].
  *)

  type t
  (** The abstract type representing a normalized form for records. *)

  val focus : descr -> label -> t
  (** [focus d l] prepares a normal form where the types associated to [l] have
    been isolated.*)

  val get_this : t -> descr
  (** [get_this t] returns the type associated with label [l]. Doing
      [get_this (focus t l)] is equivalent to [project_opt t l]. *)

  val need_others : t -> bool
  (** [need_others t] returns [true] if and only if the focused label yields two
        distinct types. For instance, focusing [t= {x = Int; y = Int } | {x =
        Bool; y = Int }] on [x], [get_this t] yields [Int | Bool], and
        [need_other t] yields [false], since the projection can be represented
        as a single pair : [ [ (Int|Bool, { y = Int }) ] ].

      On the other hand, focusing [t= {x = Char; y = Bool } | {x = Bool; y =
      Int}] on [x], [get_this t] yields [Char | Bool], and [need_other t] yields
      [true], since the projection cannot be represented as a single pair : [
      [(Char, { y = Bool }); (Bool, { y= Int }) ] ].
    *)

  val constraint_on_others : t -> descr -> descr
  (** [constraint_on_others t cstr] returns the intersection of all record types
    for which the type of the focused label has a non-empty intersection with
    [cstr].
  *)
end

(** Arrow normalization. *)
module Arrow : sig
  val trivially_arrow : t -> [ `Arrow | `NotArrow | `Other ]
  (** [trivially_arrow t] returns [`Arrow] if t is a supertype of
      Empty -> Any, [`NotArrow] if it is a subtype of Any \ (Empty -> Any)
      and [`Other] otherwise.
  *)

  val sample : t -> t
  (** [sample t] returns one possible positive arrow type from the DNF of [t].
  *)

  val check_iface : (t * t) list -> t -> bool
  (** [check_iface iface t] takes a list, representing
      an intersection of arrow types given by the pair of their domain and
      co-domain. It checks that all arrows in the interface are a subtype of
      [t]. It is slightly more efficient than combining all arrow types as an
      intersection and testing with the general subtyping test.
  *)

  type t = descr * (descr * descr) list list
  (** The normalized form of an arrow type. A list representing a union of pairs
        [(dom, iface)] where [iface] is an intersection of arrows (as in
        [check_iface])
        and [dom] the union of all domains of that intersection.

        Note that the negative parts of the arrow type are discarded.
  *)

  val is_empty : t -> bool
  (** Check that a normalized form is empty. Constant time, once the arrow type
    has been normalized. *)

  val get : descr -> t
  (** [get t] returns the normalized form of the arrow component of [t]. *)

  val domain : t -> descr
  (** [domain n] returns the domain of the normalized form [n], that is, the
    intersection of all domains of all interfaces.
  *)

  val apply : t -> descr -> descr
  (** [apply n targ] returns the type of the result of applying a function with
    interface [n] to an argument of type [targ].
    Assumes [subtype targ (domain n)].
    *)

  val need_arg : t -> bool
  (** [need_arg n] returns [true] if the type of the argument is needed to
      obtain the type of the result, in which case one must use [apply] to
      compute it. This is need in particular if [n] is an intersection type with
      with distinct domains.
   *)

  val apply_noarg : t -> descr
  (** [apply_noarg n] assumes that [n] is an arrow type and that [need_arg n] is
    false and returns the type of the result, that is the co-domain of [n].
  *)
end

(** {2 Utilities }*)
val non_constructed : t
(** A type that is a union of all basic types, that is, neither a product,
    arrow, XML or record type. *)

val print_witness : Format.formatter -> t -> unit
(** [print_witness fmt t] prints a witness for the non empty type t to the given
    formatter. A witness is a in most case a constant representing an inhabitant
    of the type, except for arrows and abstract types for which a generic type
    name is used.

  @raise Not_found if [t] is the empty type.
*)

(** A {i semantic} cache indexed by types. Semantic equivalence is used to check
  whether a type is present in the cache. *)
module Cache : sig
  type 'a cache
  (** The type of the cache storing values of type ['a]. The complexity of the
   search and add operation is a logarithmic number of calls to the subytping
   algorithm, w.r.t. the number of types in the cache.
  *)

  val emp : 'a cache
  (** The empty cache. *)

  val lookup : t -> 'a cache -> 'a option
  (** [lookup t cache] returns the [Some v] if [v] is associated with a
    type equivalent to [t] in the cache and [None] otherwise. *)

  val find : (t -> 'a) -> t -> 'a cache -> 'a cache * 'a
  (** [find init t cache] search for the value associated with [t] in [cache]:
     - if the binding is present, the old [cache] and the value are returned
     - if the binding is absent, the value is created by calling [init t] and
      the new cache, as well as the value are returned.
  *)

  val memo : (t -> 'a) -> t -> 'a
  (** [memo f] returns a memoized version of [f] that only computes its result
    once for each semantically equivalent types it receives as argument.
  *)
end

(**/**)

val normalize : t -> t

(** Tools for compilation of PM **)

val cond_partition : t -> (t * t) list -> t list

(* The second argument is a list of pair of types (ti,si)
   interpreted as the question "member of ti under the assumption si".
   The result is a partition of the first argument which is precise enough
   to answer all the questions. *)

val forward_print : (Format.formatter -> t -> unit) ref
