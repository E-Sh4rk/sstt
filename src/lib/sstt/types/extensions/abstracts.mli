open Core

type variance = Cov | Contrav | Inv
type 't params = 't list
type 't t = ('t params list * 't params list) list

val define : string -> variance list -> Tag.t
val is_abstract : Tag.t -> bool
val params_of : Tag.t -> variance list
val mk : Tag.t -> Ty.t list -> Ty.t
val mk_any : Tag.t -> Ty.t
val destruct : Tag.t -> Ty.t -> Ty.t t

val to_t : (Printer.ctx -> Ty.t -> Printer.descr) -> Printer.ctx
            -> TagComp.t -> Printer.descr t option
val map : ((Printer.descr -> Printer.descr) -> Printer.descr t -> Printer.descr t)

val printer_builder : Tag.t -> Printer.extension_builder
val printer_params : Tag.t -> Printer.params
