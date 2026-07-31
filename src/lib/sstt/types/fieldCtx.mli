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


val labels_toplevel : Ty.t -> LabelSet.t
(** [labels_toplevel ty] returns the explicit labels appearing at top-level in [ty]. *)

val labels : Ty.t -> LabelSet.t
(** [labels ty] returns the explicit labels appearing in [ty]. *)

val of_tys : RowVarSet.t -> Ty.t list -> t
(** [of_tys mono tys] generates a [t] that decorrelates the field variables
    that may be compared when comparing the types [tys], excluding the row variables in [mono]. *)
