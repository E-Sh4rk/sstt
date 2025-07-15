open Core

type t

val identity : t
val singleton : Var.t -> Ty.t -> t
val of_list : (Var.t * Ty.t) list -> t

(** [refresh ~names vs] returns a substitution mapping each variable
in [vs] to a fresh one. If [names] is omitted, each fresh variable
will have the same name as the original one. *)
val refresh : ?names:(Var.t -> string) -> VarSet.t -> t * t

val domain : t -> VarSet.t
val bindings : t -> (Var.t * Ty.t) list
val find : t -> Var.t -> Ty.t

val add : Var.t -> Ty.t -> t -> t
val remove : Var.t -> t -> t
val filter : (Var.t -> Ty.t -> bool) -> t -> t
val map : (Ty.t -> Ty.t) -> t -> t

(** [compose s2 s1] returns a substitution [s] such that applying [s]
has the same effect as applying [s1] and then [s2]. *)
val compose : t -> t -> t

val equiv : t -> t -> bool
val is_identity : t -> bool

val apply : t -> Ty.t -> Ty.t
