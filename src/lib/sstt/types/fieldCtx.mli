(**
Manages a context that can be used to temporarily decorrelate independant field variables from a type.
For instance, [{ lbl1:`r ; lbl2:`r ;; `r }, { lbl1:`r }]
can temporarily be turned into [{ lbl1:`r1 ; lbl2:`r2 ;; `r }, { lbl1:`r1 }],
making it obvious which occurrences of [`r] capture different fields.
*)

open Core

type fvar = RowVar.t * Label.t
(** A field variable is a pair ([rv], [lbl]) that denotes a row variable
    [rv] appearing in the context of a field labeled [lbl]. *)

type t
(** An environment that defines a correspondance between field variables and
    fresh row variables. *)

val mk : LabelSet.t -> RowVarSet.t -> t
(** Generates a [t] for a set of row variables and labels. *)

val singl : fvar -> t
(** Generates a [t] for a single field variable. *)

val empty : t

val merge : t -> t -> t
(** [merge t1 t2] returns a new field context containing bindings
    from [t1] and from [t2], with priority to [t1]. *)

val merge_many : t list -> t
(** [merge_many ts] merges the field contexts [ts] in order. *)

val fresh_vars : t -> RowVarSet.t
(** Returns the set of fresh row variables introduced by a field context. *)

val fvars : t -> fvar list
(** Returns the set of field variables captured by a field context. *)

val fresh_var_of_fvar : t -> fvar -> RowVar.t
(** [fresh_var_of_fvar t fvar] returns the fresh row variable associated with [fvar] in [t].
    Returns the row variable associated with [fvar] if no fresh row variable is found. *)

val fvar_of_fresh_var : t -> RowVar.t -> fvar option
(** [fvar_of_fresh_var t rv] returns the field variable associated with [rv] in [t].
    Returns [None] if no associated field variable is found. *)

val decorrelate : t -> Ty.t -> Ty.t
(** Refresh row variables of a type according to a field context. *)

val recombine : t -> Ty.t -> Ty.t
(** Recombine row variables of a type according to a field context. *)

val recombine' : t -> Subst.t -> Subst.t
(** Recombine row variables of a substitution according to a field context. *)

val of_tys : RowVarSet.t -> Ty.t list -> t
(** [of_tys mono tys] generates a [t] that decorrelates the field variables
    that may be compared when comparing the types [tys], excluding the row variables in [mono]. *)


(** Direct dependencies (subnodes) of types, grouped by the position under which
    they appear. Two subnodes of two types may only be compared (when comparing
    these types) if they appear under the same position. *)
module Dependencies : sig

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
end
