open Core

type t

val identity : t
val singleton : Var.t -> Ty.t -> t
val mk : (Var.t * Ty.t) list -> t
val renaming : ?names:(Var.t -> string) -> VarSet.t -> t * t

val domain : t -> VarSet.t
val bindings : t -> (Var.t * Ty.t) list
val find : t -> Var.t -> Ty.t

val add : Var.t -> Ty.t -> t -> t
val remove : Var.t -> t -> t
val filter : (Var.t -> Ty.t -> bool) -> t -> t
val map : (Ty.t -> Ty.t) -> t -> t
val compose : t -> t -> t

val equiv : t -> t -> bool
val is_identity : t -> bool

val apply : t -> Ty.t -> Ty.t
