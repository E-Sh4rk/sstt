(** Floating-point numbers, abstracted by their kind and encoded as a tag
    applied to a union of enums (one per kind). *)

open Core

val tag : Tag.t
(** The tag used to encode floats. *)

type k = Ninf | Neg | Nzero | Pzero | Pos | Pinf | Nan
(** The kinds of floating-point numbers that are distinguished: negative
    infinity, negative numbers, negative zero, positive zero, positive numbers,
    positive infinity, and NaN. *)

val flt : k -> Ty.t
(** [flt k] is the type of the floats of kind [k]. *)

val any : Ty.t
(** The type of all floats. *)

type t = { ninf : bool ; neg : bool ; nzero : bool ; pzero : bool ; pos : bool ; pinf : bool ; nan : bool }
(** A float type, given by the kinds it contains. *)

val to_t : Printer.build_ctx -> TagComp.t -> t option
(** Recognizes a float type (see {!Sstt.Extensions}). *)

val map : t Printer.map
(** A float type contains no sub-type: this is the identity. *)

val print : int -> Prec.assoc -> Format.formatter -> t -> unit
(** Prints a float type. *)

val any_t : t
(** The representation of {!any}. *)

val empty_t : t
(** The representation of the empty type. *)

val neg_t : t -> t
(** [neg_t t] is the representation of the complement of [t] in {!any}. *)

val components : t -> k list
(** [components t] returns the kinds contained in [t]. *)

val printer_builder : Printer.extension_builder
(** The printer extension for floats. *)

val printer_params : Printer.params
(** Printing parameters recognizing floats. *)
