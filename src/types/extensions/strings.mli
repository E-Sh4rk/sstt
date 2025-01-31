open Sstt_core

val tag : TagComp.Tag.t
val add_tag : Ty.t -> Ty.t

val str : string -> Ty.t
val any : Ty.t

type t = bool * string list
type printer = Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params
