val init_all : unit -> unit
val register : ?priority:int -> string -> string -> (unit -> unit) -> unit
val descrs : unit -> (string * string) list
val inhibit : string -> unit

(* Last registered features are initialized last (and thus take priority) *)
