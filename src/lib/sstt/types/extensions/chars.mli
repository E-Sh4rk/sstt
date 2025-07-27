open Core

val tag : Tag.t

type interval = char * char

val chr : char -> Ty.t
val interval : interval -> Ty.t
val any : Ty.t

type t = interval list
val any_t : t

type printer = int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params
