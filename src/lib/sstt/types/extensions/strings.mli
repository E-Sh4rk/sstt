(** Strings, encoded as a tag applied to enums, one per string literal.

    Two occurrences of the same literal yield the same enum, so that string
    types can be compared. Note that this encoding only supports finite and
    cofinite sets of literals: it says nothing about the structure of strings.
*)

open Core

val tag : Tag.t
(** The tag used to encode strings. *)

val str : string -> Ty.t
(** [str s] is the singleton type containing the string [s]. *)

val any : Ty.t
(** The type of all strings. *)

type t = bool * string list
(** A string type, given by a list of literals and a boolean telling whether
    the type contains them ([true]) or every string but them ([false]). *)

val to_t : Printer.build_ctx -> TagComp.t -> t option
(** Recognizes a string type (see {!Sstt.Extensions}). *)

val map : t Printer.map
(** A string type contains no sub-type: this is the identity. *)

val print : int -> Prec.assoc -> Format.formatter -> t -> unit
(** Prints a string type. *)

val printer_builder : Printer.extension_builder
(** The printer extension for strings. *)

val printer_params : Printer.params
(** Printing parameters recognizing strings. *)
