open Sstt_core

exception EmptyAtom

module Arrows : sig
    type t = Arrows.t

    (** [dom t] returns the domain of the arrow component [t]. *)
    val dom : t -> Ty.t

    (** [apply t arg] returns the type resulting from the application
    of an argument of type [arg] to a function [t]. An argument not
    in the domain will yield the resulting type [any]. *)
    val apply : t -> Ty.t -> Ty.t

    (** [worra t res] returns the type that must necessarily have an argument
    applied to the function [t] for the result to have type [res]
    (assuming the application did not diverge). *)
    val worra : t -> Ty.t -> Ty.t
end

module Products : sig
    type t = Products.t
    type atom = Products.Atom.t

    (** [as_union t] expresses [t] as an union of non-empty atoms. *)
    val as_union : t -> atom list

    (** [approx t] approximates [t] as a non-empty atom.
    Raises: [EmptyAtom] if [t] is empty. *)
    val approx : t -> atom

    (** [proj n t] returns the type resulting from the projection on the
    [n]-th component (0-indexed) of [t]. *)
    val proj : int -> t -> Ty.t

    (** [merge t1 t2] returns the atom resulting from the concatenation of
    [t1] and [t2]. *)
    val merge : atom -> atom -> atom
end

module Records : sig
    type t = Records.t
    type atom = Records.Atom.t

    (** [as_union t] expresses [t] as an union of non-empty atoms. *)
    val as_union : t -> atom list

    (** [approx t] approximates [t] as a non-empty atom.
    Raises: [EmptyAtom] if [t] is empty. *)
    val approx : t -> atom

    (** [proj l t] returns the (possibly absent) type resulting
    from the projection on the label [l] of [t]. *)
    val proj : Label.t -> t -> Records.Atom.OTy.t

    (** [merge t1 t2] returns the atom resulting from the merging of
    [t1] and [t2] (non-absent fields in [t2] override those in [t1]). *)
    val merge : atom -> atom -> t

    (** [remove t l] returns the atom obtained by making the field [l]
    absent in [t]. *)
    val remove : atom -> Label.t -> t
end
