type t = Types.t Var.Map.map
(** The type representing type substitutions. *)

val from_list : (Var.t * Types.t) list -> t

val print : Format.formatter -> t -> unit
(** [print fmt sub] pretty prints a substitution. *)

val print_list : Format.formatter -> t list -> unit
(** [print_list fmt sub] pretty prints a list of substitution. *)

val vars : Types.t -> Var.Set.t
(** [vars t] returns all the type variable occuring in [t]. *)

val top_vars : Types.t -> Var.Set.t
(** [top_vars t] returns all the type variable occuring in [t] at toplevel (that
  is not below a constructor). 
*)

val extract : Types.t -> Var.t * bool
(** [extract t] returns a variable from [t]. The associated boolean indicates
  whether the variable is a positive occurrence or a negative one.
*)

val check_var : Types.t -> [ `Not_var | `Pos of Var.t | `Neg of Var.t ]
(** [check_var t] tests whether type [t] is a single variable or negation of
    variable and returns a detailed result. *)

val is_var : Types.t -> bool
(** [is_var t] tests whether type [t] is a single variable or negation of
    variable. This is an alias for [check_var t <> `Not_var]*)

val apply : t -> Types.t -> Types.t
(** [apply s t] applies the substitution [s] to the type [t].
    Substitutions are applied in arbitrary order and thus may yield unwanted
    results when the codomain of [s] contains types whose variables are in
    it's domain.
*)

val apply_full : t -> Types.t -> Types.t
(** [apply_full s t] applies the substitution [s] to the type [t]. The
  substitution [s] is sorted in topological order so that types containing
  variables in the domain of [s] are applied later, so that these variables are
  also substituted.

  @raise Failure if there is cycle in the ordering of substitutions.
*)

val refresh : Var.Set.t -> Types.t -> Types.t
(** [refresh vars t] replaces all variables of [t] that are not in [vars]
    by fresh variables with the same name.
*)

val solve_rectype : Types.t -> Var.t -> Types.t
(** [solve_rectype t v] returns the recursive type that is the solution of
    the equation [v = t], where [v] occurs as a type variable in [t].
*)

val clean_type : ?pos:Types.t -> ?neg:Types.t -> Var.Set.t -> Types.t -> Types.t
(**
  [clean_type pos neg vars t] returns the type [t] where :
  - all variables that only have positive occurrences are replaced by [pos]
  - all variables that only have negative occurrences are replaced by [neg]
  - all variables that are invariant or a in [vars] are left untouched.
*)

val min_type : Var.Set.t -> Types.t -> Types.t
(**
  [min_type vars t] returns the largest subtype of any instance of [t], that is
  [t] where all positive occurrences of variables not in [vars] are replaced by
  𝟘 and all negative occurrences of variables not in [vars] are replaced by 𝟙.
*)

val max_type : Var.Set.t -> Types.t -> Types.t
(**
  [max_type vars t] returns the smallest supertype of any instance of [t], that is
  [t] where all positive occurrences of variables not in [vars] are replaced by
  𝟙 and all negative occurrences of variables not in [vars] are replaced by 𝟘.
*)

val var_polarities : Types.t -> [ `Pos | `Neg | `Both ] Var.Map.map
