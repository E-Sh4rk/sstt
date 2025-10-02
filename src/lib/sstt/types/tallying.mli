(** Tallying (unification modulo subtyping constraints). *)

open Core

type constr = Ty.t * Ty.t
(** The type of a tallying constraint. A constraint [(s, t)] means
    that we want to find all substitutions for variables of [s] and [t] 
    such that [Ty.leq s t].
*)


(** [tally mono constrs] returns all solutions to the tallying instance
    [constrs], considering that variables in [mono] cannot be substituted.
    The solutions returned do not feature any fresh type variable:
    the type variables already present in [constrs] are reused. *)
val tally : VarSet.t -> constr list -> Subst.t list

(** [tally_with_order compare mono constrs] is the same as [tally mono constrs],
    but using the total order [compare] over type variables. The solutions returned
    are such that a variable cannot be substituted by a type featuring a smaller
    non-monomorphic variable at top-level. *)
val tally_with_order : (Var.t -> Var.t -> int) -> VarSet.t -> constr list -> Subst.t list

(** [tally_with_priority lst mono constrs] is the same as [tally mono constrs],
    but using an order that will preserve variables in [lst] when possible.
    The solutions returned are such that a variable in [lst] cannot be substituted
    by a type featuring, at top-level, a non-monomorphic variable further in [lst]
    or not in [lst]. The list of variables [lst] should not have duplicates. *)
val tally_with_priority : Var.t list -> VarSet.t -> constr list -> Subst.t list

(** [decompose mono s1 s2] returns a set of substitutions [s] whose domain
    is disjoint with [mono] and such that the composition of [s] and [s1] yields [s2].
    In particular, a non-empty result means that [s1] is more general than [s2]
    (in the sense that [s2] can be obtained by composing [s1] with another substitution). *)
val decompose : VarSet.t -> Subst.t -> Subst.t -> Subst.t list
