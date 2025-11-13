open Cduce_loc

include (module type of Cduce_error_typ)

type loc_error_t =
  | Unlocated
  | Located of loc
  | PreciselyLocated of loc * precise

exception Error : loc_error_t * ('a error_t * 'a) -> exn
val mk_loc : loc -> ('a error_t * 'a) -> exn
val raise_err : 'a error_t -> 'a -> 'b
val raise_err_loc : loc:loc -> 'a error_t -> 'a -> 'b
val raise_err_generic : string -> 'a
val raise_err_generic_loc : loc:loc -> string -> 'a
val raise_err_precise : loc:loc -> precise -> 'a error_t -> 'a -> 'b
val warning : loc:loc -> string -> 'b -> 'b

(*val raise_err_loc_source : int -> int -> 'a error_t -> 'a -> 'b*)

val print_error_loc : Format.formatter -> loc_error_t -> ('a error_t * 'a) -> unit
val print_exn : Format.formatter -> exn -> unit
