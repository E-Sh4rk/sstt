open Sstt_core

val tag : TagComp.Tag.t
val add_tag : Ty.t -> Ty.t

val cons : Ty.t -> Ty.t -> Ty.t
val nil : Ty.t
val any : Ty.t

val basic_printer_params : Printer.params

type 'a regexp =
| Letter of 'a
| Concat of 'a regexp list
| Union of 'a regexp list
| Star of 'a regexp
| Plus of 'a regexp
| Option of 'a regexp

type printer = Format.formatter -> Printer.descr regexp -> unit

val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params