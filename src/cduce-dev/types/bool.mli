(** Representation of formulæ. *)

(** A general compact representation for a formula using union, intersection and
    negation between atoms. This is used for sets of type constructors for
    which, unlike for basic types, there isn't a single canonical formula.

    Internaly, the data-structure use a particular kind of BDD. The formula
    can then be retrieved in disjunctive normal form (DNF) to perform high
    level operation on atoms.
*)
module type S = sig
  include Tset.S
  (** The type [t] representing formula, equiped with custom and set operations.
  *)

  val get : t -> (elem list * elem list) list
  (** [get d] returns an explicit DNF from [d]. The outer list is the
    disjunction. Each pair represent a cunjunction of positive atoms (first
    component) and negative atoms (second component).
  *)

  val iter : (elem -> unit) -> t -> unit
  (** [iter f d] traverses the formula and applies [f] to each atom. Note that the
    same atom may occur several times.
  *)

  val compute :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(elem -> 'b) ->
    t ->
    'b
  (** [compute ~empty ~any ~cup ~cap ~diff ~atom d] performs a computation that
      follows the formula : whenever the formula is empty (resp. full) the value
      associated with ~empty (resp. ~any) is returned. Whenever the formula
      performs a union (resp. intersection or difference) between two subterms
      [~cup] (resp. [~cap] or [~diff]) is called to combine the result of the
      computation for both subterms. Whenever the formula is an atom, [~atom]
      is called on that atom to return a result.

      Note that the formula may contain redundent subformulæ, for instance:
      [Empty ∪ A ∪ A] (for some atom [A]). On such a formula, [~empty] will be
      returned and [~elem] and [~cup] will be called twice.
    *)
end

module Make : functor (E : Custom.T) -> S with type elem = E.t
