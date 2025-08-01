open Core

val tag : Tag.t

type k = Ninf | Neg | Nzero | Pzero | Pos | Pinf | Nan

val flt : k -> Ty.t
val any : Ty.t

type t = { ninf : bool ; neg : bool ; nzero : bool ; pzero : bool ; pos : bool ; pinf : bool ; nan : bool }
val any_t : t
val empty_t : t
val neg_t : t -> t
val components : t -> k list

type printer = int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params
