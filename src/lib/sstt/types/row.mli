open Core

type t
val any : t
val empty : t
val id_for : RowVar.t -> t
val all_fields : Ty.F.t -> t
val mk : (Label.t * Ty.F.t) list -> Ty.F.t -> t

val to_record_atom : t -> Records.Atom.t
val tail : t -> Ty.F.t
val bindings : t -> (Label.t * Ty.F.t) list
val dom : t -> LabelSet.t
val find : Label.t -> t -> Ty.F.t

val equiv : t -> t -> bool
val equiv_constraints : t -> t -> (Ty.t * Ty.t) list
val substitute : Ty.subst -> t -> t
val vars : t -> VarSet.t
val row_vars : t -> RowVarSet.t
val all_vars : t -> MixVarSet.t
val row_vars_toplevel : t -> RowVarSet.t
val map_nodes : (Ty.t -> Ty.t) -> t -> t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
