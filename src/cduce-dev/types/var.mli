(** Polymorphic type variables. *)

(** Variables are tagged with a unique identifier. They will compare equal only
  if their identifier is the same (and it is assumed that their external name is
  the same). *)

module V : Custom.T

include module type of V with type t = V.t
(** Type [t = V.t] represents variables. *)

(** Sets of variables. *)
module Set : sig
  include SortedList.S with module Elem = V

  val print : Format.formatter -> t -> unit
end

module Map = Set.Map
(** Maps indexed by variables. *)

val mk : ?kind:[ `generated | `user | `weak ] -> string -> t
(** mk ~kind "name" creates a fresh variable with name [name] and kind [kind].
    It is distinct (in the sense of [equal] and [compare]) from all other
    created variables, even those with the same name. The optional kind is used
    to indicate whether the variable was explicitely written by the user in a
    type definition or annotation, or if it was generated, e.g. during
    constraint generation or unification.
*)

val name : t -> string
(** [name v] returns the display name of variable v. *)

val id : t -> int
(** [id v] returns the internal id of the variable. *)

val kind : t -> [ `generated | `user | `weak ]
(** [kind v] returns the kind of the variable. *)

val print : Format.formatter -> t -> unit
(** [print ppf v] prints the variable's display name. *)

val renaming : Set.t -> t Map.map
(** [renaming vset] returns a map from variables from [vset] 
    to fresh variables with distinct names. In case of aliases,
    one occurrence of the variables in [vset] is unchanged and
    the others are renamed with a suffix [1], [2], …
    The function tries to keep the name of [`user] variables unchanged
    if possible.
 *)

val full_renaming : Set.t -> t Map.map
(** [renaming vset] returns a map from variables from [vset] to fresh variables.
  The kind of the variables are preserved and the new names start at ["a"] and
  follow the scheme ["a"], ["b"], …, ["z"], ["aa"], ["ab"], …
*)

val merge : t list -> t list -> (Set.t * Set.t) option
(** [merge pos neg] returns the pair of sets of variables contained in [pos] and
    [neg] as an option. The option is [None] if [pos] and [neg] have a non-empty
    intersection.
*)
