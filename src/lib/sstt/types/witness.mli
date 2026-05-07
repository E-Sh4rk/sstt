open Core

type t = private
    Int of Z.t
  | Enum of Enums.Atom.t
  | Arrow of Arrows.t 
  | Tag of (Tag.t * t)
  | Tuple of t list
  | Record of (Label.t * t ) list * (t option)
  | Var of (Subst.t * t)
  | Other


(**[singl_to_Ty t] transform the singleton type t in a real type Ty*)
val to_ty : t -> Ty.t

(**[compare t1 t2] return a total order of t.*)
val compare : t -> t -> int

(**[pp fmt w] is the pretty printer for the witness type*)
val pp : Format.formatter -> t -> unit

(**[equal t1 t2] return [true] if the 2 witnesses [t1] and [t2] are equals, [false] otherwise.*)
val equal : t -> t -> bool

(**[is_in w t] return [true] if the witness [w] is an inhabitant of [t], [false] otherwise.
    Create the type t' that only has w in it, then check if t' is a subtype of t.*)
val is_in : t -> Ty.t -> bool

(** [make t] returns one value that is in the type t. 
    Assume that t in non-empty.
*)
val mk : Ty.t -> t