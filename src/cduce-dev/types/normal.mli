(** This module implements the normalisation of Cartesian products of sets.
    Given a DNF (union of intersection of positive and negative products),
    this module contains functions that return:

    - a union of products (that is, the intersections are pushed below the
      products)

    - a union of product in normal form, that is a list of 
      {i (t{_ i}, s {_ i})}
      products where the {i t{_ i}} are pair-wise disjoint and neither the
      {i t{_ i}} nor the {i s{_ i}} are empty. 
*)

(** This signature represent abstract sets *)
module type S = sig
  type t
  (** The type of a set *)

  val any : t
  (** The universal set *)

  val empty : t
  (** The empty set *)

  val cup : t -> t -> t
  (** Boolean connectives *)

  val cap : t -> t -> t
  val diff : t -> t -> t

  val is_empty : t -> bool
  (** Test for emptiness*)
end

type 'a bool = ('a list * 'a list) list
(** An DNF given as a List of pairs of list:
    [ [ (p1, n1);  (p2, n2); ... ] ] where the [pi] are lists of positive
    atoms and the [ni] are lists of negative atoms.
*)

(** The functor implementing the normalisation of products *)
module Make (X1 : S) (X2 : S) : sig
  type t = (X1.t * X2.t) list
  (** The type of a simplified DNF : a list of products *)

  val normal : t -> t
  (** Returns the normal form  of simplified form:
      [normal l] returns a list [ [(t1, s1);  (t2, s2); ... ] ] where :
      - the [ti] are pair-wise disjoint
      - the [(ti,si)] are non empty products.

      The empty product is returned as [ [] ].
  *)

  val boolean_normal : (X1.t * X2.t) bool -> t
  (** [boolean_normal l] returns the normal form from a DNF representation *)

  val boolean : (X1.t * X2.t) bool -> t
  (** [boolean l] returns a simplified form from the DNF representation *)

  val pi1 : t -> X1.t
  (** [pi1 l] returns the union of all first components of [l] *)

  val pi2 : t -> X2.t
  (** [pi2 l] returns the union of all second components of [l] *)

  val pi2_restricted : X1.t -> t -> X2.t
  (** [pi2 restr l] returns the union of all second components for
  which the first component intersects [restr] *)
end
