(** Sets of characters (Unicode code points) represented as disjoint
 intervals. The range of valid code points is \[0-0x10ffff\].
   Surrogate pairs, that is code points in the range \[0xd7ff-0xdfff\] are
   allowed but their use is not recommended.
*)

(** A module for manipulating atom {!V}alues. *)
module V : sig
  include Custom.T
  (** The type of characters operations.  *)

  val mk_int : int -> t
  (** [mk_int i] creates a character from it Unicode code point.
  @raise [Failure] if [i] is not a valid code point.
  *)

  val mk_char : char -> t
  (** [mk_char c] creates a character from an OCaml [char], that is
  a code point in the range \[0-255\].
  *)

  val to_int : t -> int
  (** [to_int t] returns the integer corresponding to the code point. 
  *)

  val to_char : t -> char
  (** [to_char t] returns the [char] corresponding to the code point.
   @raise [Failure] if [t] is not in the range \[0-255\].
  *)

  val print : Format.formatter -> t -> unit
  (** [print_in_string fmt t] prints the code point correctly escaped, between a
      pair of single quotes. Code points 9 (horizontal tab), 10 (new line), 13
      (carriage return), 34 (double quote) 39 (single quote) are printed as
      ['\t'], ['\n'], ['\r'], ['\"'] and ['\'']  repsectively. 

    Code points below 32 (non printable characters) or above 127 (non ascii
    characters) are printed as their ℂDuce escape sequence \ddddd.
  *)

  val print_in_string : Format.formatter -> t -> unit
  (** [print_in_string fmt t] prints the code point as [print] but without the
    surrounding single quotes.*)
end

include Tset.S with type elem = V.t

(** {2 Type specific operations: } *)

val char_class : V.t -> V.t -> t
(** [char_class i j] returns the set of codepoints between [i] and [j]
    inclusive. Returns [empty] if [i > j].
  *)

val mk_classes : (V.t * V.t) list -> t
(** [mk_classes l] returns the set of disjoint unions of ranges of code points
  in [l]. Overlaping ranges are supported (and simplified).
*)

val is_char : t -> V.t option
(** [is_char t] returns [Some c] if [t] is the singleton containing [c],
  otherwise returns [None].*)

val extract : t -> (V.t * V.t) list
(** [extract t] returns the list of interval of code points in [t]. 
    The returned interval are disjoint and in increasing order of their lower
    bound.
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
      the interval. Singleton intervals are juste printed as [c] (using
      {!V.print}). The intervals are always disjoints and printed in increasing
      order of their lower-bound, separated by ["|"]. As a special case, [any],
      that is the interval [0-0x1f0000] is printed as [Char].
*)

(**/**)

type 'a map

val mk_map : (t * 'a) list -> 'a map
val get_map : V.t -> 'a map -> 'a
val map_map : ('a -> 'b) -> 'a map -> 'b map
