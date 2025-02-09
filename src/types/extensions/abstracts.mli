open Sstt_core

type variance = Cov | Contrav | Inv

val mk : TagComp.Tag.t -> Ty.t list -> Ty.t

type params = Printer.descr list
type t = (params list * params list) list
type printer = TagComp.Tag.t -> Format.formatter -> t -> unit
val print : printer

val define : printer -> string -> variance list -> TagComp.Tag.t * Printer.params
val define' : string -> variance list -> TagComp.Tag.t * Printer.params
