open Core

type t = private
Int of Z.t
|Enum of Enums.Atom.t
|Arrow of Arrows.t 
|Tuple of t list
|Tag of (Tag.t * t)
|Other
|Wrong


(**[singl_to_Ty t] transform the singleton type t in a real type Ty*)
val to_ty : t -> Ty.t

(**[mk_intervals t] return one value present in the interval part of t.
Assume that the interval part of t is non-empty.
If t = ]-inf; +inf\[, return 42.
If t as a part ]-inf; x], return x.
Otherwise, return the lowest bound of t.
*)
val mk_intervals : Ty.t -> t

(**[mk_enum t] return one value present  in the enum part of [t].
Assume that the enum part of [t] is non-empty. 
If what is inside [t] is known, return the first element of [t].
If what is NOT inside [t] is known, return the word composed of the letter 'a' repeated
one more time than the max length of the atoms not in [t].
*)
val mk_enums : Ty.t -> t

(**[mk_tuple t] return one value present in the tuple part of [t].
Assume that the tuple part of [t] is non-empty.
If the elements of [t] are known, return one inhabitant of one element of [t].
If the elements of [not t] are known, return (0, 1,...,n, n+1) with n the arity max of the elements not in [t]. 
*)

(** [make t] returns one value that is in the type t. 
Assume that t!= empty.
*)
val mk : Ty.t -> t

(**[pp fmt w] is the pretty printer for the witness type*)
val pp : Format.formatter -> t -> unit

(**[equal t1 t2] return [true] if the 2 witness [t1] and [t2] are equals, [false] otherwise.*)
val equal : t -> t -> bool

(**[is_in w t] return [true] if the witness [w] is an inhabitant of [t], [false] otherwise.
    Create the type t' that only has w in it, then check if t' is a subtype of t.*)
val is_in : t -> Ty.t -> bool