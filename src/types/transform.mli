open Sstt_core

(** [transform f ty] returns the type obtained by applying [f]
on the full descriptor of [ty], and on the full descriptor of
every node in the result recursively. It uses a cache to avoid
calling [f] twice on the same descriptor. *)
val transform : (VDescr.t -> VDescr.t) -> Ty.t -> Ty.t

(** [simplify ty] returns a type equivalent to [ty] but where
atoms have been merged together when possible. *)
val simplify : Ty.t -> Ty.t
