open Core

type t = Records.Atom.t
val id_for : RowVar.t -> t

val leq : t -> t -> bool
val equiv : t -> t -> bool
val substitute : Ty.subst -> t -> t
val dom : t -> LabelSet.t
val vars : t -> VarSet.t
val row_vars : t -> RowVarSet.t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
