open Core

val tag : Tag.t

type k = Ninf | Neg | Nzero | Pzero | Pos | Pinf | Nan

val flt : k -> Ty.t
val any : Ty.t

type t = { ninf : bool ; neg : bool ; nzero : bool ; pzero : bool ; pos : bool ; pinf : bool ; nan : bool }
val to_t : Printer.build_ctx -> TagComp.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val any_t : t
val empty_t : t
val neg_t : t -> t
val components : t -> k list

val printer_builder : Printer.extension_builder
val printer_params : Printer.params