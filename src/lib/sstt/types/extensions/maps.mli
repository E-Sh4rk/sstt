(** Maps (finite or infinite), encoded as a tag applied to an arrow type.

    A map type is a set of fields [dom => codom], meaning that the values of
    the map at the keys of type [dom] have type [codom]; a negative field
    [dom ~> codom] states the converse. As arrows, fields are intersected: the
    type of the maps sending integers to booleans and anything else to
    anything is written [{{ int => bool }}] and encoded as the arrow type
    [(int -> bool) & (any -> any)].
*)

open Core

type 't field = { dom: 't ; codom: 't }
(** A single constraint of a map type: the keys of type [dom] are associated
    with values of type [codom]. *)

type 't t = ('t field list * 't field list) list
(** A map type, as a union of clauses made of positive and negative fields. *)

val tag : Tag.t
(** The tag used to encode maps. *)

val mk' : Ty.t field list -> Ty.t
(** [mk' fields] is the type of the maps satisfying all the [fields]. *)

val mk : Ty.t field list * Ty.t field list -> Ty.t
(** [mk (fields, nfields)] is the type of the maps satisfying all the [fields]
    and none of the [nfields]. *)

val any : Ty.t
(** The type of all maps. *)

val destruct : Ty.t -> Ty.t t
(** [destruct ty] returns the map type encoded by [ty].
    @raise Invalid_argument if [ty] is not a valid encoding of a map type. *)

val proj : dom:Ty.t -> Ty.t -> Ty.t
(** [proj ~dom ty] returns the type of the values that a map of type [ty]
    associates with the keys of type [dom]. *)

val merge : Ty.t -> Ty.t field -> Ty.t
(** [merge ty field] returns the type of the maps of type [ty] updated so as to
    satisfy [field]. *)

val to_t : Printer.build_ctx -> TagComp.t -> Printer.descr t option
(** Recognizes a map type (see {!Sstt.Extensions}). *)

val map : Printer.descr t Printer.map
(** Applies the given function to the domain and codomain of every field. *)

val print : int -> Prec.assoc -> Format.formatter -> Printer.descr t -> unit
(** Prints a map type. *)

val printer_builder : Printer.extension_builder
(** The printer extension for maps. *)

val printer_params : Printer.params
(** Printing parameters recognizing maps. *)
