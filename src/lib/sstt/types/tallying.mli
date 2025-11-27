(** Tallying (unification modulo subtyping constraints). *)

open Core

val solve_rectype : Var.t -> Ty.t -> Ty.t
val solve_recfield : RowVar.t -> Ty.F.t -> Ty.F.t

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

type fields_ctx
val get_fields_ctx : RowVarSet.t -> Ty.t list -> fields_ctx
val decorrelate_fields : fields_ctx -> Ty.t -> Ty.t
val recombine_fields : fields_ctx -> Ty.t -> Ty.t
val recombine_fields' : fields_ctx -> Subst.t -> Subst.t
val fvars_associated_with : fields_ctx -> RowVar.t -> RowVarSet.t
val rvar_associated_with : fields_ctx -> RowVar.t -> (RowVar.t * Label.t) option
val tally_decorrelated : MixVarSet.t -> constr list -> Subst.t list

(** [decompose mono s1 s2] returns a set of substitutions [s] whose domain
    is disjoint with [mono] and such that the composition of [s] and [s1] yields [s2].
    In particular, a non-empty result means that [s1] is more general than [s2]
    (in the sense that [s2] can be obtained by composing [s1] with another substitution). *)
val decompose : MixVarSet.t -> Subst.t -> Subst.t -> Subst.t list
