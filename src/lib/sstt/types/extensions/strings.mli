open Core

val tag : Tag.t
val str : string -> Ty.t
val any : Ty.t

type t = bool * string list
val to_t : Printer.build_ctx -> TagComp.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val printer_builder : Printer.extension_builder
val printer_params : Printer.params