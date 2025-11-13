(* Wrappers for some functions working on Utf8.t values.
 * No more Pcre here... *)
open Encodings.Utf8

val replace_space : t -> t
val replace_spaces : t -> t
val replace_margins : t -> t
val split_spaces : t -> t list
val next_token : t -> uindex -> Encodings.uchar -> t * uindex
val sub_token : t -> uindex -> int -> t * uindex
val validate_int : t -> int -> int -> bool
