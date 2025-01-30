open Sstt_core

val tag : TagComp.Tag.t
val add_tag : Ty.t -> Ty.t

val cons : Ty.t -> Ty.t -> Ty.t
val nil : Ty.t
val any : Ty.t

val basic_printer_params : Printer.params
val printer_params : Printer.params
