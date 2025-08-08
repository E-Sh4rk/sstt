open Core

module Node : Id.NamedIdentifier

type hierarchy
val new_hierarchy : unit -> hierarchy
val new_node : hierarchy -> name:string -> subnodes:(Node.t list) -> Node.t
val mk : hierarchy -> Node.t -> Ty.t

type t = line list
and line = L of Node.t * t

type printer = int -> Prec.assoc -> Format.formatter -> t -> unit
val print : printer

val printer_params : printer -> hierarchy -> Printer.params
val printer_params' : hierarchy -> Printer.params
