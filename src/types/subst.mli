open Core

type t = Ty.t VarMap.t

val empty : t
val singleton : Var.t -> Ty.t -> t
val of_list : (Var.t * Ty.t) list -> t

val domain : t -> VarSet.t
val bindings : t -> (Var.t * Ty.t) list
val find : t -> Var.t -> Ty.t

val add : Var.t -> Ty.t -> t -> t
val filter : (Var.t -> Ty.t -> bool) -> t -> t
val map : (Ty.t -> Ty.t) -> t -> t
val compose : t -> t -> t
