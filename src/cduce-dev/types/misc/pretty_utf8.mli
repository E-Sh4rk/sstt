(** Base module for unicode prettifying  *)

(** {2 UTF8 Symbols} *)

type t

val create : int -> t
(** [create v] creates an Utf8 symbol that can then be added to a {!Ptree.t} associated to string (see {!Ptree.add} for more details) *)

(** {2 Binding functions} *)

val register_utf8_binding : string -> t -> unit
val get_utf8_binding : Uchar.t -> string

(** {2 Prettifier} *)

val prettify : string -> int -> int -> string * int
