open Sstt_core

val tag : TagComp.Tag.t
val add_tag : Ty.t -> Ty.t
val proj_tag : Ty.t -> Ty.t

val btrue : Ty.t
val bfalse : Ty.t
val bool : bool -> Ty.t
val any : Ty.t

type t = { t : bool ; f : bool }
val any_t : t
val empty_t : t
val neg_t : t -> t
val components : t -> bool list

type printer = Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params
