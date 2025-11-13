(** Positive systems and least solutions. *)

(**
 This modules allows one to create type equations and solve them,
 yielding possibly mutually recursive types.
*)

type v
(** The type of positive terms. *)

val forward : unit -> v
(** [forward ()] creates a fresh recursion variable. *)

val define : v -> v -> unit
(** [define v1 v2] creates the type equation [v1 = v2]. 
    [v1] is expected to be a variable created by [forward()].
*)

val ty : Types.t -> v
(** [ty t] a term created from a concrete type. *)

val cup : v list -> v
(** [cup l] returns the union of all terms in [l]. *)

val times : v -> v -> v
(** [times v1 v2] returns the product of [v1] and [v2]. *)

val xml : v -> v -> v
(** [xml v1 v2] returns the XML product of [v1] and [v2]. *)

val arrow : v -> v -> v
(** [arrow v1 v2] returns the arrow type from [v1] to [v2]. *)

val record : (Ident.label * bool * v) list -> bool -> v
(** [record fields op] returns the record type whose labels are given by
  [fields]. If [op] is true, the record is open. *)

val cap : v list -> v
(** [cap v1 v2] returns the intersection of [v1] and [v2].*)

val diff : v -> v -> v
(** [diff v1 v2] returns the difference between [v1] and [v2].*)

val solve : v -> Types.Node.t
(** [solve v] solves the system for the positive term [v] and returns a
    newly built type node.
 *)

val decompose : ?stop:(Types.t -> v option) -> Types.t -> v
(** [decompose t] *)

(**/**)

val dump : Format.formatter -> v -> unit
