open Core

val tag : Tag.t

type interval = char * char

val chr : char -> Ty.t
val interval : interval -> Ty.t
val any : Ty.t

type t = interval list
val to_t : (Printer.ctx -> Ty.t -> Printer.descr) -> Printer.ctx
            -> TagComp.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val any_t : t

val printer_builder : Printer.extension_builder
val printer_params : Printer.params