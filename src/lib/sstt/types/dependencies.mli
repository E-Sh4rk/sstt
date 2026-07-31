(**
Direct dependencies (subnodes) of types, grouped by the position under which they appear.
Two subnodes of two types may only be compared (when comparing these types)
if they appear under the same position.
*)

open Core

module NodeSet : Set.S with type elt = Ty.t

(** A position under which the direct dependencies of a type can be found. *)
module Position : sig
  type t =
    | Dom                (** domain of an arrow *)
    | Codom              (** codomain of an arrow *)
    | Tuple of int * int (** index and length of a tuple *)
    | Tag of Tag.t       (** content of a tagged type *)
    | Field of Label.t   (** field of a record *)
    | Tail               (** tail of a record *)

  val compare : t -> t -> int
end

module PosMap : Map.S with type key = Position.t

type t = NodeSet.t PosMap.t
(** The dependencies associated with each position. A position [Field lbl] is only
    bound if [lbl] is explicitly bound in the type the dependencies come from:
    the dependencies of the [Tail] position also apply to any other label
    (this is taken care of by [merge]). *)

val empty : t

val of_ty : Ty.t -> t
(** [of_ty ty] returns the direct dependencies of [ty], by exploring the top-level
    definition of [ty] (that is, without going through any constructor twice). *)

val merge : t -> t -> t
(** [merge d1 d2] gathers, for each position, the dependencies that [d1] and [d2]
    have under this position. Thus, [merge (of_ty t1) (of_ty t2)] associates each
    position with the subnodes of [t1] and [t2] that may be compared together
    when comparing [t1] and [t2]. *)

val merge_many : t list -> t
(** [merge_many ds] merges the dependencies [ds]. *)
