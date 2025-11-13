(** Sets of atoms, used for variants ([`nil], [`true], [`false], …) as well as
  for XML tags ([<hello>[]] ≡ [<(`hello)>[]]). *)

(** A module for manipulating atom {!V}alues. *)
module V : sig
  include Custom.T
  (** The type of atoms with custom operations. Atoms are hash-consed and can be
    compared with physical equality. *)

  type value = Ns.QName.t
  (** An alias type for convenience. Atoms correspond to qualified names with a
    namespace and a local part (e.g. [`xhtml:div], with [xhtml] being bound to
    the namespace [http://www.w3.org/1999/xhtml]). Of course for the most
    common case (e.g. [`nil], [`true], [`false]) the namespace is empty.*)

  val mk : value -> t
  (** [mk q] creates an atom from a qualified name (see {!Ns.QName}). *)

  val value : t -> value
  (** [value a] returns a qualified name from an atom.*)

  val mk_ascii : string -> t
  (** [mk_ascii a] creates an atom with an empty namespace. It does not perform
    any check on [a], for instance [mk_ascii "hello:world"] creates an atom with
    an empty namespace and an (illegal) name [hello:world] (here [hello:] is
    made part of the local name.).
  *)

  val get_ascii : t -> string
  (** [get_ascii a] returns the local name of an atom. *)

  (** {2 Formatting functions}*)

  (** The atom is printed as a string [prefix:lname] where [prefix] is a short
     string to which the actual namespace URI is bound. If no prefix has been
     bound explicitely (see {!Ns.add_prefix}), a default prefix [ns1], [ns2], …
     is used (equal URI's have the same prefix). [lname] is the local name of
     the atom.
  *)

  val print : Format.formatter -> t -> unit
  (** [print fmt a] prints an atom to the specified formatter. *)

  val to_string : t -> string
  (** [to_string a] converts the atom to string*)

  val print_quote : Format.formatter -> t -> unit
  (** [print fmt a] prints an atom to the specified formatter in ℂDuce syntax.
  *)
end

include Tset.S with type elem = V.t
(** The type {!t} of sets of atoms with its basic operations. Since atoms live 
    in an open world (basically the set of valid XML tags) such a set can either 
    be finite (listing explicitely the elements it contains) or co-finite
    (listing explicitely the elements it does not contain).
    Furthermore, each such finite or cofinite set can be restrited to a
    particular namespace.
*)

(** {2 Type specific operations: }*)

val any_in_ns : Ns.Uri.t -> t
(** [any_in_ns uri] returns the set of all atoms that have a particular
  namespace. *)

(** {2 Membership: }*)

val is_empty : t -> bool
(** [is_empty t] checks wheter [t] is the empty set. *)

val contains : V.t -> t -> bool
(** [contains a t] checks whether the atom [a] belongs to [t] *)

val single : t -> V.t
(** [single t] assumes [t] is a singleton and returns its unique element.
 @raise [Not_found] if [t] is the empty set
 @raise [Exit] if [t] contains more than one element
*)

val disjoint : t -> t -> bool
(** [disjoint t1 t2] checks whether [t1] and [t2] have an empty intersection.*)

type sample = (Ns.Uri.t * Ns.Label.t option) option
(** The type of a [sample] that is a representation of an element of the set. *)

val sample : t -> sample
(** [sample t] returns a sample for [t] for instance if [t] is [any] returns
    [None]. If [t] contains only co-finite sets for some namespaces [ns1],
    [ns2], … returns [Some (ns1, None)]. Finaly, if [t] contains at least one
    element, [sample t] returns [Some(ns, Some l)] where [ns] is the namespace
    and [l] the local name.
    @raise [Not_found] if [t] is empty.
  *)

val contains_sample : sample -> t -> bool
(** [contains_sample s t] checks whether the given sample represents an element
  of [t].*)

(** {2 Formatting functions :}*)

val print : t -> (Format.formatter -> unit) list
(** [print t] returns, for each namespaces in the set [t], a function that
    prints the set of atoms in that namespace (thus calling each function,
    separated with ["|"] prints the whole set).
*)

val print_tag : t -> (Format.formatter -> unit) option
(** [print_tag t] returns a formatter that outputs the content of the set as a
   single tag if possible, following ℂDuce syntax for patterns:
  - if [t] is a singleton, it prints the atom as a tag, using {!V.print}
  - if [t] is a an cofinite set in a single namespace, it prints the set 
    as [ns:*] where [ns] is the prefix to which the namespace is bound.
  - if [t] is [any], it prints [_]
  
  In all cases above, the function returns [Some f] where [f] is the printer.
  Otherwise it returns [None].
*)

(**/**)

type 'a map

val mk_map : (t * 'a) list -> 'a map
val get_map : V.t -> 'a map -> 'a
val map_map : ('a -> 'b) -> 'a map -> 'b map

val extract :
  t ->
  [ `Finite of (Ns.Uri.t * [ `Finite of V.t list | `Cofinite of V.t list ]) list
  | `Cofinite of
    (Ns.Uri.t * [ `Finite of V.t list | `Cofinite of V.t list ]) list
  ]

val is_finite : t -> bool
