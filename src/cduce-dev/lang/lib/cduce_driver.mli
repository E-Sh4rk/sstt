open Cduce_types

val toplevel : bool ref
val verbose : bool ref
val extra_specs : (string * Arg.spec * string) list ref
val topinput : ?source:string -> Format.formatter -> Format.formatter -> char Seq.t -> bool
val dump_env : Format.formatter -> unit
val compile : string -> string option -> unit
val compile_run : string -> unit
val run : string -> unit
val set_argv : string list -> unit
val eval : string -> (AtomSet.V.t option * Value.t) list
(* Can be used from CDuce units *)
