open Sstt_core

type constr = Ty.t * Ty.t

(** [tally mono constrs] returns all solutions to the tallying instance
[constrs], considering that variables in [mono] cannot be substituted.
The solutions returned do not feature any fresh type variable:
the type variables already present in [constrs] are reused. *)
val tally : VarSet.t -> constr list -> Subst.t list
