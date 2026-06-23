open Core

val tag : Tag.t

val tt : Ty.t
val ff : Ty.t
val bool : bool -> Ty.t
val any : Ty.t

type t = { t : bool ; f : bool }
val to_t : Printer.build_ctx -> TagComp.t -> t option
val map : t Printer.map
val print : int -> Prec.assoc -> Format.formatter -> t -> unit

val any_t : t
val empty_t : t
val neg_t : t -> t
val components : t -> bool list

val printer_builder : Printer.extension_builder
val printer_params : Printer.params