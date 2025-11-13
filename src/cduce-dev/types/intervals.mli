(** Sets of integers represented as disjoint intervals. *)

(** A module for manipulating integer {!V}alues. *)
module V : sig
  include Custom.T
  (** The type of arbitrary-precision integers with custom operations. ℂDuce
    integers use {!Z.t} from the [zarith] library internaly, Most
    functions in this module are wrappers around those in {!Z}.
  *)

  val print : Format.formatter -> t -> unit
  (** [print fmt i] prints [i] to the given formatter. *)

  val mk : string -> t
  (** [mk s] creates an integer from its string decimal representation. The
    string can be prefixed with an optional [+] or [-]. *)

  val from_int : int -> t
  (** [from_int i] creates an integer from an OCaml [int]. *)

  val from_Z : Z.t -> t
  (** [from_Z i] creates an integer from {!Z.t}, from the OCaml
     [zarith] library. *)

  val to_string : t -> string
  val to_float : t -> float
  val is_int : t -> bool
  val get_int : t -> int
  val get_Z : t -> Z.t
  val is_zero : t -> bool
  val max : t -> t -> t
  val min : t -> t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val negat : t -> t
  val lt : t -> t -> bool
  val leq : t -> t -> bool
  val gt : t -> t -> bool
  val zero : t
  val one : t
  val minus_one : t
  val from_int32 : Int32.t -> t
  val from_int64 : Int64.t -> t
  val to_int32 : t -> Int32.t
  val to_int64 : t -> Int64.t
end

include Tset.S with type elem = V.t

(** {2 Type specific operations: }*)

val bounded : V.t -> V.t -> t
(** [bounded i j] returns the closed interval \[ [i], [j] \].*)

val left : V.t -> t
(** [left j] returns the left opened interval ( -∞, [j] \].*)

val right : V.t -> t
(** [right i] returns the right opened interval \[ [i], +∞ ). *)

val add : t -> t -> t
(** [add t1 t2] returns the intervals that are the sums of the individual
intervals of [t1] and [t2]. For instance :
  {[
    (1--2 | 5--10)  + (3--4 | 5--6) = 4--6 | 8--14 | 6--8 | 10--16 
                                    = 4--16
  ]}
*)

val negat : t -> t
(** [negat t] negates all the bounds of [t] *)

val sub : t -> t -> t
(** [sub t1 t2] subtract two intervals. [sub t1 t2] is equivalent to
    [add t1 (negat t2)].
*)

val mul : t -> t -> t
(** [mul t1 t2] returns the intervals that are the products of the individual
intervals of [t1] and [t2].
*)

val div : t -> t -> t
(** [div t1 t2] returns [any] (this is a convenience function). *)

val modulo : t -> t -> t
(** [modulo t1 t2] returns [any] (this is a convenience function). *)

val int32 : t
(** [int32] represent the set of integers representable by an OCaml [int32] *)

val int64 : t
(** [int64] represent the set of integers representable by an OCaml [int64] *)

val is_bounded : t -> bool * bool
(** [is_bounded t] returns a pair of boolean indicating whether the interval is
    left and right bounded.
*)

(** {2 Membership: }*)

val is_empty : t -> bool
(** [is_empty t] checks wheter [t] is the empty set. *)

val contains : V.t -> t -> bool
(** [contains i t] checks whether the integer [a] belongs to [t] *)

val single : t -> V.t
(** [single t] assumes [t] is a singleton and returns its unique element.
 @raise [Not_found] if [t] is the empty set
 @raise [Exit] if [t] contains more than one element
*)

val disjoint : t -> t -> bool
(** [disjoint t1 t2] checks whether [t1] and [t2] have an empty intersection.*)

val sample : t -> V.t
(** [sample t] returns an element of [t].
  @raise [Not_found] if [t] is empty.
*)

(** {2 Formatting functions :}*)

val print : t -> (Format.formatter -> unit) list
(** [print t] returns, for each interval in the set [t], a function that prints
    the interval. Left (resp. right) opened intervals are printed as [*--n]
    (resp. [n--*]). Singleton intervals are juste printed as [n]. The intervals
    are always disjoints and printed in increasing order of their lower-bound,
    separated by ["|"]. As a special case, [any] is printed as [Int].
*)
