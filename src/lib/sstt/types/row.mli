open Core

type t = Records.Atom.t
val id_for : RowVar.t -> t
val all_fields : Ty.F.t -> t

val equiv : t -> t -> bool
val equiv_constraints : t -> t -> (Ty.t * Ty.t) list
val substitute : Ty.subst -> t -> t
val dom : t -> LabelSet.t
val vars : t -> VarSet.t
val row_vars : t -> RowVarSet.t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
