open Core

type constr = Ty.t * Ty.t

val tally : VarSet.t -> constr list -> Subst.t list
