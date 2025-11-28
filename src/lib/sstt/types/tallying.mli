(** Tallying (unification modulo subtyping constraints). *)

open Core

val solve_rectype : Var.t -> Ty.t -> Ty.t
(** [solve_rectype v t] returns the type captured by [v] in the equation [v=t]
    (where [v] may appear in [t] under a constructor). *)

val solve_recfield : RowVar.t -> Ty.F.t -> Ty.F.t
(** [solve_recfield rv fty] returns the field type captured by [rv] in the equation [rv=fty]
    (where [rv] may appear in [fty] under a constructor). *)

type constr = Ty.t * Ty.t
(** The type of a tallying constraint. A constraint [(s, t)] means
    that we want to find all substitutions for variables of [s] and [t] 
    such that [Ty.leq s t].
*)

(** [tally mono constrs] returns all solutions to the tallying instance
    [constrs], considering that variables in [mono] cannot be substituted.
    The solutions returned do not feature any fresh type variable:
    the type variables already present in [constrs] are reused. *)
val tally : MixVarSet.t -> constr list -> Subst.t list

(** [decompose mono s1 s2] returns a set of substitutions [s] whose domain
    is disjoint with [mono] and such that the composition of [s] and [s1] yields [s2].
    In particular, a non-empty result means that [s1] is more general than [s2]
    (in the sense that [s2] can be obtained by composing [s1] with another substitution). *)
val decompose : MixVarSet.t -> Subst.t -> Subst.t -> Subst.t list

(** {1 Operations on row and field variables}*)

(** A row variable is considered to be a field variable if it only appears in fields of the same label,
    or only appears in a tail position that is never compared to any other field.
    Although field variables are represented using row variables,
    they should only be substituted by rows of the form [ { ;; fty } ] (i.e. rows with no bindings).
    The concept of field variable is used during constraint solving in order to decorrelate the
    row variables appearing under different fields. *)

type field_ctx
(** An environment that captures the correspondance between row variables and field variables. *)

val get_field_ctx : RowVarSet.t -> Ty.t list -> field_ctx
(** Generates a [field_ctx] for a set of types. *)

val fvars_associated_with : field_ctx -> RowVar.t -> RowVarSet.t
(** Returns the set of field variables associated with a row variable in a field context. *)

val rvar_associated_with : field_ctx -> RowVar.t -> (RowVar.t * Label.t) option
(** Returns the row variable associated with a field variable in a field context, together with the
    label of the corresponding field. *)

val decorrelate_fields : field_ctx -> Ty.t -> Ty.t
(** Turn row variables of a type into field variables according to a field context. *)

val recombine_fields : field_ctx -> Ty.t -> Ty.t
(** Recombine field variables of a type into their initial row variable according to a field context. *)

val recombine_fields' : field_ctx -> Subst.t -> Subst.t
(** Transform a substitution manipulating field variables into the corresponding substitution manipulating
    row variables, according to a field context. *)

val tally_fields : MixVarSet.t -> constr list -> Subst.t list
(** Run the tallying algorithm on constraints that are already decorrelated. All row variables
    should be field variables, and the solutions returned will only substitute them
    by rows of the form [ { ;; fty } ]. *)
