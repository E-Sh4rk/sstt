open Core

type variance = Cov | Contrav | Inv

val is_abstract : Tag.t -> bool
val params_of : Tag.t -> variance list
val mk : Tag.t -> Ty.t list -> Ty.t
val mk_any : Tag.t -> Ty.t
val destruct : TagComp.t -> (Tag.t * (Ty.t list list * Ty.t list list) list) option

type params = Printer.descr list
type t = (params list * params list) list
type printer = Tag.t -> int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val define : printer -> string -> variance list -> Tag.t * Printer.params
val define' : string -> variance list -> Tag.t * Printer.params
