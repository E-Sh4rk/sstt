open Core

val tag : Tag.t

val btrue : Ty.t
val bfalse : Ty.t
val bool : bool -> Ty.t
val any : Ty.t

type t = { t : bool ; f : bool }
val to_t : (Printer.ctx -> Ty.t -> Printer.descr) -> Printer.ctx -> Ty.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val any_t : t
val empty_t : t
val neg_t : t -> t
val components : t -> bool list

val printer_builder : Printer.extension_builder
val printer_params : Printer.params