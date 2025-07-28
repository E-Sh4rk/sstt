(** Type substitutions *)

open Core

type t
(** The type of type substitutions. Substitution are quasi-constant mappings
      that map every variable to itself (as a type), except for a finite set of
      variables.

    The {i domain} of a substitution is the set of variables for which it is not
    constant (see {!domain}).
*)

val identity : t
(** The identity substitutions which maps every variable to itself. *)

val singleton : Var.t -> Ty.t -> t
(** [singleton v t] is the substitutions that maps every variable to itself,
    except [v] which is mapped to [t]. *)

val of_list : (Var.t * Ty.t) list -> t
(** Creates a substitution from the given list of variables. If a variable
    occurs several times, the last occurrence is used.
*)

val refresh : ?names:(Var.t -> string) -> VarSet.t -> t * t
(** [refresh ~names vs] returns a substitution mapping each variable
    in [vs] to a fresh one. If [names] is omitted, each fresh variable
    will have the same name as the original one. *)

val domain : t -> VarSet.t
(** Returns the domain of a substitution, that is the set of variables for which
    the substitution is not the identity.
*)

val bindings : t -> (Var.t * Ty.t) list
(** Returns the substution as a list of bindings from variables to types.
*)


val find : t -> Var.t -> Ty.t
(** Returns the type associated with a variable. This function always succeeds, and will return 
    the type {m \alpha }, if the variable {m \alpha} is not in the domain of the substitution. *)

val add : Var.t -> Ty.t -> t -> t
(** Adds a new binding to the given substitution. If the new binding is the
    identity for the given variable, the substitution is unchanged. *)

val remove : Var.t -> t -> t
(** Remove a variable from the domain of the substitution. *)

val filter : (Var.t -> Ty.t -> bool) -> t -> t
(** [filter p s] restricts the substitution to all variables of the domain for
    which [p] returns [true].*)

val map : (Ty.t -> Ty.t) -> t -> t
(** [map f s] returns the substitution where [f] is applied to each type [t] in the domain of [s]. *)

val compose : t -> t -> t
(** [compose s2 s1] returns a substitution [s] such that applying [s]
    has the same effect as applying [s1] and then [s2]. *)

val equiv : t -> t -> bool
(** Checks whether two substitutions are equivalent, that is, they have the same
    domain and for each variable, the associated types are equivalent (using
    {!Sstt.Ty.equiv}). *)

val is_identity : t -> bool
(** Checks whether the domain of the substitution is empty. *)

val apply : t -> Ty.t -> Ty.t
(** Applies the given susbtitution to the given type. *)