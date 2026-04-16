open Core

type t = private
    Int of Z.t
  | Enum of Enums.Atom.t
  | Arrow of Arrows.t 
  | Tag of (Tag.t * t)
  | Tuple of t list
  | Record of (Label.t * t ) list * (t option)
  | Other



(**[singl_to_Ty t] transform the singleton type t in a real type Ty*)
val to_ty : t -> Ty.t

val compare : t -> t -> int

(**[mk_intervals t] return one value present in the interval part of t.
   Assume that the interval part of t is non-empty.
   If t = ]-inf; +inf\[, return 42.
   If t as a part ]-inf; x], return x.
   Otherwise, return the lowest bound of t.
*)
val mk_intervals : Ty.t -> t

(**[mk_enum t] return one value present in the enum part of [t].
   Assume that the enum part of [t] is non-empty. 
   If the elements of [t] are known, return the first element of [t].
   If the elements of [not t] are known, return the word composed of the letter 'a' repeated
   one more time than the max length of the atoms not in [t].
*)
val mk_enums : Ty.t -> t

(**[mk_arrows t] return one arrow present in the arrow part of [t].
   Assume that the arrow part of [t] is non-empty.
   Don't go inside the domains of the arrows, 
   just return the arrow with all the positive atoms 
   and the tinyest subgroup of negative atoms that allow
   to create a subtype of the arrow.
*)
val mk_arrows : Ty.t -> t

(**[mk_tags t] return one value present in the tuple part of [t].
   Assume that the tag part of [t] is non-empty.
   If the elements of [t] are known, take one tag component (tag, type) and return the tag ta(witness ty).
   If the elements of [not t] are known, return tag(16) with tag being the character 'a' repeated n times, n beign a length of no other tag.
*)
val mk_tags : Ty.t -> t

(**[mk_tuple_mem t] return one value present in the tuple part of [t].
   Assume that the tuple part of [t] is non-empty.
   If the elements of [t] are known, return one inhabitant of one element of [t].
   If the elements of [not t] are known, return (0, 1,...,n, n) with n the smallest arity with no elements in [not t]. 
*)
val mk_tuples : Ty.t -> t

(**[mk_record t] return one value present in the record part of [t].
   Assume that the record part of [t] is non-empty.
   Return a record made from one non_empty atom of r the record part of t with the following caracteristics :
   - for every (l_i : f_i) of the binding part of r : if f_i is required, add (l_i : witness f_i) to the inhabitant.
   - for every (L_i : e_i) of the exists part of r : if (e_i && tail) is required, add (l'_i : witness (e_i && tail)) to the binding, with l'_i not in L_i and the label part of the binding.
   - if the tail is not optional, put witness tail in the tail.
*)
val mk_records : Ty.t -> t




(** [make t] returns one value that is in the type t. 
    Assume that t in non-empty.
*)
val mk : Ty.t -> t

(**[pp fmt w] is the pretty printer for the witness type*)
val pp : Format.formatter -> t -> unit

(**[equal t1 t2] return [true] if the 2 witness [t1] and [t2] are equals, [false] otherwise.*)
val equal : t -> t -> bool

(**[is_in w t] return [true] if the witness [w] is an inhabitant of [t], [false] otherwise.
    Create the type t' that only has w in it, then check if t' is a subtype of t.*)
val is_in : t -> Ty.t -> bool