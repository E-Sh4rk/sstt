open Core

val tag : Tag.t
val str : string -> Ty.t
val any : Ty.t

type t = bool * string list
type printer = int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params
