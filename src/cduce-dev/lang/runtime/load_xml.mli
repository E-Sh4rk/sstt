open Encodings

val string : string -> Value.t -> Value.t
val attrib : ('a Upool.typed_int * Utf8.t) list -> Value.t Imap.t

val elem :
  Ns.table ->
  AtomSet.V.t ->
  ('a Upool.typed_int * Utf8.t) list ->
  Value.t ->
  Value.t

val only_ws : bytes -> int -> bool
val load_xml : ?ns:bool -> string -> Value.t
val mk_load_xml : (string -> unit) -> ?ns:bool -> string -> Value.t
val load_xml_subst : ?ns:bool -> string -> (Ns.Uri.t * Ns.Uri.t) list -> Value.t
val html_loader : (string -> Value.t) ref
val load_html : string -> Value.t

(* To define and register a parser *)

val xml_parser : (string -> unit) ref
val start_element_handler : string -> (string * string) list -> unit
val start_element_handler_resolved_ns : (string*string) -> ((string*string)*string) list -> unit
val end_element_handler : 'a -> unit
val text_handler : string -> unit
