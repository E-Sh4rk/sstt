type ml_type

type ext_info = (string * string * int * ml_type) list


val get : unit -> ext_info option
val register : (bool -> string -> Types.Node.t list -> int * Types.t) ref
val ext_info : (unit -> ext_info) ref
val resolve : string -> Types.Node.t list -> int * Types.t
val typ : string -> Types.Node.t list -> Types.t
