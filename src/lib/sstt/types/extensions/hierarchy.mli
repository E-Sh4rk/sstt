(** Hierarchies of abstract types.

    A hierarchy is a set of nodes ordered by a user-defined subtyping relation:
    each node is declared together with the nodes it must be a supertype of.
    The whole hierarchy is encoded with a single tag, applied to the union of
    the enums of the transitive subnodes of a node, so that a node is a subtype
    of another one exactly when it has been declared as such (transitively).
*)

open Core

module Node : Id.NamedIdentifier
(** The nodes of a hierarchy. *)

type hierarchy
(** A hierarchy, together with the tag used to encode it. *)

val new_hierarchy : unit -> hierarchy
(** Creates a new, empty hierarchy, with a fresh tag. *)

val new_node : hierarchy -> name:string -> subnodes:(Node.t list) -> Node.t
(** [new_node h ~name ~subnodes] adds to [h] a new node of name [name], which
    is a supertype of the nodes [subnodes] (and, transitively, of their own
    subnodes).
    @raise Invalid_argument if a node of [subnodes] does not belong to [h]. *)

val mk : hierarchy -> Node.t -> Ty.t
(** [mk h n] is the type denoted by the node [n] of [h], that is, the union of
    [n] and of its subnodes.
    @raise Invalid_argument if [n] does not belong to [h]. *)

type t = line list
(** A type of the hierarchy, as a union of lines. *)

and line = L of Node.t * t
(** [L (n, excluded)] denotes the node [n] deprived of the types [excluded]
    (which are built from its subnodes). *)

val to_t : hierarchy -> Printer.build_ctx -> TagComp.t -> t option
(** Recognizes a type of the given hierarchy (see {!Sstt.Extensions}). *)

val map : t Printer.map
(** Such a type contains no sub-type: this is the identity. *)

val print : int -> Prec.assoc -> Format.formatter -> t -> unit
(** Prints a type of a hierarchy, using the names of its nodes. *)

val printer_builder : hierarchy -> Printer.extension_builder
(** The printer extension for the given hierarchy. *)

val printer_params : hierarchy -> Printer.params
(** Printing parameters recognizing the given hierarchy. *)
