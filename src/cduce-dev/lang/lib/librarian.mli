open Cduce_types
open Ident
open Encodings

val name : Compunit.t -> Utf8.t
val run_loaded : bool ref
val compile_save : bool -> Utf8.t -> string -> string -> unit
val compile_run : bool -> Utf8.t -> string -> unit
val load_run : Utf8.t -> unit
val run : Compunit.t -> unit
val prepare_stub : bool -> string -> unit

val ocaml_stub :
  string ->
  Types.t array * (Value.t array -> unit) * Value.t array * (unit -> unit)

val stub_ml :
  (bool ->
  string ->
  string ->
  Typer.t ->
  Compile.env ->
  Externals.ext_info option ->
  (Types.t array -> string) ->
  unit)
  ref

val has_virtual_prefix : string -> bool
val exists_with_prefix : string -> bool
val register_static_external : string -> Value.t -> unit
val get_builtins : unit -> string list
val make_wrapper : (bool -> string -> unit) ref
