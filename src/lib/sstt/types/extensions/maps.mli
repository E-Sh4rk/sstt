open Core

type 't field = { dom: 't ; codom: 't }
type 't t = ('t field list * 't field list) list

val tag : Tag.t
val mk' : Ty.t field list -> Ty.t
val mk : Ty.t field list * Ty.t field list -> Ty.t
val any : Ty.t
val destruct : Ty.t -> Ty.t t
val proj : dom:Ty.t -> Ty.t -> Ty.t
val merge : Ty.t -> Ty.t field -> Ty.t

val to_t : Printer.build_ctx -> TagComp.t -> Printer.descr t option
val map : ((Printer.descr -> Printer.descr) -> Printer.descr t -> Printer.descr t)

val printer_builder : Printer.extension_builder
val printer_params : Printer.params
