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

val singleton1 : Var.t -> Ty.t -> t
(** [singleton1 v t] is the substitutions that maps every variable to itself,
    except [v] which is mapped to [t]. *)

val singleton2 : RowVar.t -> Row.t -> t
(** [singleton2 v r] is the substitutions that maps every row variable to itself,
    except [v] which is mapped to [r]. *)

val of_list1 : (Var.t * Ty.t) list -> t
(** Creates a substitution from the given list of type variables. If a variable
    occurs several times, the last occurrence is used. *)

val of_list2 : (RowVar.t * Row.t) list -> t
(** Creates a substitution from the given list of row variables. If a variable
    occurs several times, the last occurrence is used. *)

val of_list : (Var.t * Ty.t) list -> (RowVar.t * Row.t) list -> t

val to_core_subst : t -> Ty.subst

val refresh1 : ?names:(Var.t -> string) -> VarSet.t -> t * t
(** [refresh1 ~names vs] returns a substitution mapping each variable
    in [vs] to a fresh one, together with its inverse substitution.
    If [names] is omitted, each fresh variable will have the same name as the original one. *)

val refresh2 : ?names:(RowVar.t -> string) -> RowVarSet.t -> t * t
(** [refresh2 ~names vs] returns a substitution mapping each variable
    in [vs] to a fresh one, together with its inverse substitution.
    If [names] is omitted, each fresh variable will have the same name as the original one. *)

val refresh : ?names1:(Var.t -> string) -> ?names2:(RowVar.t -> string)
    -> MixVarSet.t -> t * t

val domain1 : t -> VarSet.t
(** Returns the domain of a substitution, that is the set of type variables for which
    the substitution is not the identity. *)

val domain2 : t -> RowVarSet.t
(** Returns the domain of a substitution, that is the set of row variables for which
    the substitution is not the identity. *)

val domain : t -> MixVarSet.t

val intro1 : t -> VarSet.t
(** Returns the set of introduced variables, that is, type variables that may appear
    after applying the substitution to a type. *)

val intro2 : t -> RowVarSet.t
(** Returns the set of introduced variables, that is, row variables that may appear
    after applying the substitution to a type. *)

val intro : t -> MixVarSet.t

val bindings1 : t -> (Var.t * Ty.t) list
(** Returns the substution as a list of bindings from type variables to types. *)

val bindings2 : t -> (RowVar.t * Row.t) list
(** Returns the substution as a list of bindings from row variables to rows. *)

val find1 : t -> Var.t -> Ty.t
(** Returns the type associated with a variable. This function always succeeds, and will return 
    the type {m \alpha }, if the variable {m \alpha} is not in the domain of the substitution. *)

val find2 : t -> RowVar.t -> Row.t
(** Returns the row associated with a variable. This function always succeeds. *)

val add1 : Var.t -> Ty.t -> t -> t
(** Adds a new binding to the given substitution. If the new binding is the
    identity for the given variable, the substitution is unchanged. *)

val add2 : RowVar.t -> Row.t -> t -> t
(** Adds a new binding to the given substitution. If the new binding is the
    identity for the given variable, the substitution is unchanged. *)

val remove1 : Var.t -> t -> t
(** Remove a type variable from the domain of the substitution. *)

val remove2 : RowVar.t -> t -> t
(** Remove a row variable from the domain of the substitution. *)

val remove_many1 : VarSet.t -> t -> t
(** Remove a set of type variable from the domain of the substitution. *)

val remove_many2 : RowVarSet.t -> t -> t
(** Remove a set of row variable from the domain of the substitution. *)

val remove_many : MixVarSet.t -> t -> t

val restrict1 : VarSet.t -> t -> t
(** Restrict the domain of a substitution. Keep all the row variable bindings. *)

val restrict2 : RowVarSet.t -> t -> t
(** Restrict the domain of a substitution. Keep all the type variable bindings. *)

val restrict : MixVarSet.t -> t -> t

val filter1 : (Var.t -> Ty.t -> bool) -> t -> t
(** [filter1 p s] restricts the substitution to all variables of the domain for
    which [p] returns [true]. Keep all the row variable bindings. *)

val filter2 : (RowVar.t -> Row.t -> bool) -> t -> t
(** [filter2 p s] restricts the substitution to all variables of the domain for
    which [p] returns [true]. Keep all the type variable bindings. *)

val map1 : (Ty.t -> Ty.t) -> t -> t
(** [map1 f s] returns the substitution where [f] is applied to each type [t] in the domain of [s].
    Keep all the row variable bindings unchanged. *)

val map2 : (Row.t -> Row.t) -> t -> t
(** [map2 f s] returns the substitution where [f] is applied to each type [t] in the domain of [s].
    Keep all the type variable bindings unchanged. *)

val compose : t -> t -> t
(** [compose s2 s1] returns a substitution [s] such that applying [s]
    has the same effect as applying [s1] and then [s2]. *)

val compose_restr : t -> t -> t
(** [compose_restr s2 s1] is the same as [restrict (domain s1) (compose s2 s1)]. *)

val combine : t -> t -> t
(** Combines two substitutions whose domains are disjoint. *)

val equiv : t -> t -> bool
(** Checks whether two substitutions are equivalent, that is, they have the same
    domain and for each variable, the associated types are equivalent (using
    {!Sstt.Ty.equiv}). *)

val is_identity : t -> bool
(** Checks whether the domain of the substitution is empty. *)

val apply : t -> Ty.t -> Ty.t
(** Applies the given susbtitution to the given type. *)

val apply_to_row : t -> Row.t -> Row.t
(** Applies the given susbtitution to the given row type. *)