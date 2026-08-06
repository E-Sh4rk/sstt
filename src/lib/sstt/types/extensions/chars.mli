(** Characters, encoded as a tag applied to an integer interval
    (the character codes, between [0] and [255]). *)

open Core

val tag : Tag.t
(** The tag used to encode characters. *)

type interval = char * char
(** A range of characters, both bounds included. *)

val chr : char -> Ty.t
(** [chr c] is the singleton type containing the character [c]. *)

val interval : interval -> Ty.t
(** [interval (c1,c2)] is the type of the characters between [c1] and [c2]
    (both included).
    @raise Invalid_argument if [c1] is greater than [c2]. *)

val any : Ty.t
(** The type of all characters. *)

type t = interval list
(** A character type, given as a union of ranges. *)

val to_t : Printer.build_ctx -> TagComp.t -> t option
(** Recognizes a character type (see {!Sstt.Extensions}). *)

val map : t Printer.map
(** A character type contains no sub-type: this is the identity. *)

val print : int -> Prec.assoc -> Format.formatter -> t -> unit
(** Prints a character type. *)

val any_t : t
(** The representation of {!any}. *)

val printer_builder : Printer.extension_builder
(** The printer extension for characters. *)

val printer_params : Printer.params
(** Printing parameters recognizing characters. *)
