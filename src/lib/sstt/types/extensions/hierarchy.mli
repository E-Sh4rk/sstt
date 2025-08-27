open Core

module Node : Id.NamedIdentifier

type hierarchy
val new_hierarchy : unit -> hierarchy
val new_node : hierarchy -> name:string -> subnodes:(Node.t list) -> Node.t
val mk : hierarchy -> Node.t -> Ty.t

type t = line list
and line = L of Node.t * t
val to_t : hierarchy -> (Printer.ctx -> Ty.t -> Printer.descr) -> Printer.ctx
            -> TagComp.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val printer_builder : hierarchy -> Printer.extension_builder
val printer_params : hierarchy -> Printer.params
