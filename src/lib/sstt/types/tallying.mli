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

(** {1 Operations on row variables}*)

(** A field variable is a pair ([rv], [lbl]) that denotes a row variable
    [rv] appearing in a field labeled [lbl]. *)

type field_ctx
(** An environment that defines a correspondance between some field variables and
    fresh row variables. *)

val get_field_ctx' : LabelSet.t -> RowVarSet.t -> field_ctx
(** Generates a [field_ctx] for a set of row variables and labels. *)

val get_field_ctx : RowVarSet.t -> Ty.t list -> field_ctx
(** [field_ctx mono tys] generates a [field_ctx] for the labels and row variables
    appearing in [tys], excluding the row variables in [mono]. *)

val fvars_associated_with : field_ctx -> RowVar.t -> RowVarSet.t
(** Returns the set of fresh row variables associated with a row variable in a field context. *)

val fvar_associated_with : field_ctx -> (RowVar.t * Label.t) -> RowVar.t
(** Returns the fresh row variable associated with a field variable in a field context. *)

val rvar_associated_with : field_ctx -> RowVar.t -> (RowVar.t * Label.t) option
(** Returns the field variable associated with a fresh row variable in a field context. *)

val decorrelate_fields : field_ctx -> Ty.t -> Ty.t
(** Refresh row variables of a type according to a field context. *)

val recombine_fields : field_ctx -> Ty.t -> Ty.t
(** Recombine row variables of a type according to a field context. *)

val recombine_fields' : field_ctx -> Subst.t -> Subst.t
(** Recombine row variables of a substitution according to a field context. *)

val tally_fields : MixVarSet.t -> constr list -> Subst.t list
(** Run a limited version of the tallying algorithm that only looks for
    substitutions involving constant rows (i.e. rows of the form [ { ;; fty } ]).
    Calling [tally_fields] on a tallying instance where fields have been decorrelated
    and recombining fields in the solutions is equivalent to calling [tally]. *)
