(**
  The minimal interface for a set of values to be part of a type.
  All kinds (for instance the ℂDuce type [Int] {!module:Intervals})
  support at least these operations.
*)

type cardinal =
  | Empty
  | Full
  | Unknown

(** This module represent the basic signature for sets. It should not be used
  directly, see {!S}.
*)
module type S = sig
  type elem
  (** The type of the values in the set *)

  include Custom.T
  (** The type of the set, with mandatory custom operations. *)

  val empty : t
  (** The empty set *)

  val any : t
  (** The full set, containing all possible values for this kind.*)

  val test : t -> cardinal

  val atom : elem -> t
  (** [atom e] creates a singleton set containing element [e]. *)

  (** {2 Set operations :}*)

  val cup : t -> t -> t
  (** [cup t1 t2] returns the unions of [t1] and [t2]. *)

  val cap : t -> t -> t
  (** [cap t1 t2] returns the intersection of [t1] and [t2]. *)

  val diff : t -> t -> t
  (** [diff t1 t2] returns the set of elements of [t1] not in [t2]. *)

  val neg : t -> t
  (** [neg t] returns the set [diff any t]. *)
end
