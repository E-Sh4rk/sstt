open Sstt_core

val transform : (VDescr.t -> VDescr.t) -> Ty.t -> Ty.t
val simplify : Ty.t -> Ty.t
