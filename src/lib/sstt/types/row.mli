(** Rows, that is, the images of row variables.

    A row associates a {{!Sstt.Ty.F}field type} to every label: explicitly for
    the labels of its domain, and through its tail for all the others.
    For instance, the row [{ l : int ;; `r }] maps [l] to [int] and every other
    label to the field variable [`r]. Substituting a row variable by such a row
    is what drives row polymorphism.
*)

open Core

type t
(** The type of rows. *)

val any : t
(** The row mapping every label to {!Sstt.Ty.F.any}. *)

val empty : t
(** The row mapping every label to {!Sstt.Ty.F.empty}. *)

val id_for : RowVar.t -> t
(** [id_for rv] is the row mapping every label to the row variable [rv]. It is
    the identity substitution for [rv]. *)

val all_fields : Ty.F.t -> t
(** [all_fields f] is the row mapping every label to the field type [f]. *)

val mk : (Label.t * Ty.F.t) list -> Ty.F.t -> t
(** [mk bindings tail] is the row mapping each label of [bindings] to its field
    type, and every other label to [tail]. *)

val to_record_atom : t -> Records.Atom.t
(** Rows and record atoms are the same thing: this is the identity. *)

val tail : t -> Ty.F.t
(** [tail r] returns the field type associated by [r] with the labels that are
    not in its domain. *)

val bindings : t -> (Label.t * Ty.F.t) list
(** [bindings r] returns the explicit bindings of [r], that is, the labels of
    its domain together with their field type. *)

val dom : t -> LabelSet.t
(** [dom r] returns the labels explicitly bound by [r]. Note that a label
    outside of [dom r] is still associated with a field type, namely
    [tail r]. *)

val find : Label.t -> t -> Ty.F.t
(** [find l r] returns the field type associated by [r] with the label [l],
    even if [l] is not in the domain of [r]. *)

val equiv : t -> t -> bool
(** [equiv r1 r2] tests whether [r1] and [r2] associate equivalent field types
    with every label. *)

val equiv_constraints : t -> t -> (Ty.t * Ty.t) list
(** [equiv_constraints r1 r2] returns a set of subtyping constraints (to be
    used as a {!Sstt.Tallying} instance) whose solutions are the substitutions
    making [r1] and [r2] equivalent. *)

val substitute : Ty.subst -> t -> t
(** [substitute s r] applies the substitution [s] to [r]. *)

val vars : t -> VarSet.t
(** [vars r] returns the type variables occurring in [r]. *)

val row_vars : t -> RowVarSet.t
(** [row_vars r] returns the row variables occurring in [r]. *)

val all_vars : t -> MixVarSet.t
(** [all_vars r] returns both the type variables and the row variables of
    [r]. *)

val row_vars_toplevel : t -> RowVarSet.t
(** [row_vars_toplevel r] returns the row variables occurring in the field
    types of [r], that is, not below a type constructor. *)

val map_nodes : (Ty.t -> Ty.t) -> t -> t
(** [map_nodes f r] replaces every type [n] occurring in [r] by [f n]. *)

val compare : t -> t -> int
(** Comparison working on the internal representation of rows. *)

val equal : t -> t -> bool
(** Equality, [equal r1 r2] is equivalent to [compare r1 r2 = 0]. It is a
    syntactic notion: use {!equiv} for the semantic one. *)

val hash : t -> int
(** Hashing, consistent with {!equal}. *)
