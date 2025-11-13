open Cduce_loc

type type_fun = loc -> Types.t -> bool -> Types.t

val register :
  string ->
  int ->
  (type_fun list -> type_fun) ->
  (Value.t list -> Value.t) ->
  unit

val register_unary :
  string -> (type_fun -> type_fun) -> (Value.t -> Value.t) -> unit

val register_binary :
  string ->
  (type_fun -> type_fun -> type_fun) ->
  (Value.t -> Value.t -> Value.t) ->
  unit

val register_fun : string -> Types.t -> Types.t -> (Value.t -> Value.t) -> unit

val register_fun2 :
  string ->
  Types.t ->
  Types.t ->
  Types.t ->
  (Value.t -> Value.t -> Value.t) ->
  unit

val register_fun3 :
  string ->
  Types.t ->
  Types.t ->
  Types.t ->
  Types.t ->
  (Value.t -> Value.t -> Value.t -> Value.t) ->
  unit

val register_op :
  string ->
  ?expect:Types.t ->
  (loc -> Types.t -> Types.t) ->
  (Value.t -> Value.t) ->
  unit

val register_op2 :
  string ->
  Types.t ->
  Types.t ->
  Types.t ->
  (Value.t -> Value.t -> Value.t) ->
  unit
