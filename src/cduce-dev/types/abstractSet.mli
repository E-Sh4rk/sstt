(** Sets of abstract types whose content cannot be inspected. An abstract
type represents the whole sets of its values, and does not provide any way
to represent subsets. These abstract types behave like atoms and are used 
to represent e.g. OCaml types such as [float].
*)

module V : sig
  type t = string * Obj.t
  (** Abstract values are simply {!Stdlib.Obj.t} values, that is any OCaml
    value, tagged with their abstract type.

  No special operation is provided to manipulate such values.
  *)
end

(** Contrary to other sets of values, it is not possible to represent propre
  subsets of abstract types. Therefore, the "elements" of the set represented by
  the type [t] are only strings representing the whole abstract type by its name
  (e.g. "float").

  One can therefore represent the set of all abstract values that do not
  contain any float, as [diff any (atom "float")] but cannot represent a
  singleton set containing a particular individual float.
*)

module T : module type of Custom.String
include Tset.S with type elem = T.t

(** {2 Membership: }*)

val is_empty : t -> bool
(** [is_empty t] checks wheter [t] is the empty set. *)

val contains : elem -> t -> bool
(** [contains s t] checks whether the type label  [s] belongs to [t] *)

val disjoint : t -> t -> bool
(** [disjoint t1 t2] checks whether [t1] and [t2] have an empty intersection.*)

val sample : t -> elem option
(** [sample t] returns a sample for [t]. If [t] is not finite, returns [None].
    If [t] is finite and non empty, returns one of its elements.
    @raise [Not_found] if [t] is empty.
  *)

val contains_sample : elem option -> t -> bool
(** [contains_sample s t] checks whether the given sample represents an element
  of [t].*)

(** {2 Formatting functions :}*)

val print : t -> (Format.formatter -> unit) list
(** [print t] returns a list of functions that can print the combination of
  abstract types in [t]. Each abstract typename is prefixed by [!] to
  differenciate it from a type identifier. If [t] is [any], the set is
  simply printed as [Abstract].
*)
