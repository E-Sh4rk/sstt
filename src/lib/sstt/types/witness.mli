open Core

type single_t = Int of Z.t
|String of string
|Arrow of Arrows.Atom.t 


(**[mk_intervals t] return one value present in the interval part of t.
Assume that the interval part of t is non-empty.
If t = ]-inf; +inf\[, return 42.
If t as a part ]-inf; x], return x.
Otherwise, return the lowest bound of t.

*)
val mk_intervals : Ty.t -> single_t

(**[mk_enum t] return one value present  in the interval part of [t].
Assume that the Enums part of [t] is non-empty.
If what is NOT inside [t] is known, return the word composed of the letter 'a' repeated
one more time than the max length of the atoms not in [t].
*)
val mk_enums : Ty.t -> single_t

(** [make t] returns one value that is in the type t. 
Assume that t!= empty.
*)
val make : Ty.t -> single_t

(**[pp fmt w] is the pretty printer for the witness type*)
val pp : Format.formatter -> single_t -> unit

(**[equal t1 t2] return [true] if the 2 witness [t1] and [t2] are equals, [false] otherwise.*)
val equal : single_t -> single_t -> bool

(**[is_in w t] return [true] if the witness [w] is an inhabitant of [t], [false] otherwise.
    Create the type t' that only has w in it, then check if t' is a subtype of t.*)
val is_in : single_t -> Ty.t -> bool