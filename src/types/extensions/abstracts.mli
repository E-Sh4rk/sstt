open Sstt_core

type variance = Cov | Contrav | Inv

val is_abstract : TagComp.Tag.t -> bool
val params_of : TagComp.Tag.t -> variance list
val mk : TagComp.Tag.t -> Ty.t list -> Ty.t
val mk_any : TagComp.Tag.t -> Ty.t

type params = Printer.descr list
type t = (params list * params list) list
type printer = TagComp.Tag.t -> int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val define : printer -> string -> variance list -> TagComp.Tag.t * Printer.params
val define' : string -> variance list -> TagComp.Tag.t * Printer.params
