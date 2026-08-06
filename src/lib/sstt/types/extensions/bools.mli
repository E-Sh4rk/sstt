(** Booleans, encoded as a tag applied to the union of the two enums
    [true] and [false]. *)

open Core

val tag : Tag.t
(** The tag used to encode booleans. *)

val tt : Ty.t
(** The singleton type containing [true]. *)

val ff : Ty.t
(** The singleton type containing [false]. *)

val bool : bool -> Ty.t
(** [bool b] is {!tt} if [b] is [true], and {!ff} otherwise. *)

val any : Ty.t
(** The type of all booleans. *)

type t = { t : bool ; f : bool }
(** A boolean type, given by the constants it contains. *)

val to_t : Printer.build_ctx -> TagComp.t -> t option
(** Recognizes a boolean type (see {!Sstt.Extensions}). *)

val map : t Printer.map
(** A boolean type contains no sub-type: this is the identity. *)

val print : int -> Prec.assoc -> Format.formatter -> t -> unit
(** Prints a boolean type. *)

val any_t : t
(** The representation of {!any}. *)

val empty_t : t
(** The representation of the empty type. *)

val neg_t : t -> t
(** [neg_t t] is the representation of the complement of [t] in {!any}. *)

val components : t -> bool list
(** [components t] returns the constants contained in [t]. *)

val printer_builder : Printer.extension_builder
(** The printer extension for booleans. *)

val printer_params : Printer.params
(** Printing parameters recognizing booleans. *)
