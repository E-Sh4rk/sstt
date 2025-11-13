(** Compilation units.

Compilation units (CU) act as unique identifiers that are attached to various
definitions and allow to distinguishe e.g. between two types defined in
different files with the same name.
*)

type t
(** A compilation unit. *)

val compare : t -> t -> int
(** Standard comparison. *)

val hash : t -> int
(** Standard hash function. *)

val equal : t -> t -> bool
(** Standard equality. *)

val pervasives : t
(** A compilation unit for the global scope. *)

val enter : unit -> unit
(** [enter ()] indicate that we enter a new compilation unit. All objects
    created henceforth that are CU sensitive (global functions, types, …),
    are tagged with this CU.

    Compilation unit cannot be nested, it is an error to call [enter()] twice
    without calling [leave()] in between.
*)

val current : unit -> t
(** Returns the current compilation unit. *)

val leave : unit -> unit
(** Leaves the current compilation unit. Can only be called after having
entered a new CU. It is an error to call [leave()] from the global scope. 
*)

(**/**)

val set_hash : t -> int -> int -> unit
val get_hash : t -> int * int
