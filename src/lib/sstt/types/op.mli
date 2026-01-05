(** High-level operations on types. *)

open Core


exception EmptyAtom
(** Exception raised by some operations on tuples and records. *)

module Arrows : sig
  (** Operations on arrow types. *)

  type t = Arrows.t

  (** [dom t] returns the domain of the arrow component [t]. *)
  val dom : t -> Ty.t

  (** [apply t arg] returns the type resulting from the application
      of an argument of type [arg] to a function [t]. An argument not
      in the domain will yield the resulting type [any]. *)
  val apply : t -> Ty.t -> Ty.t

  (** [worra t res] returns the type that must necessarily have an argument
      applied to the function [t] for the result to have type [res] (assuming
      the application did not diverge). For instance, for an arrow type 
      {m t\equiv (\texttt{int}\rightarrow\texttt{bool})\cap(\texttt{string}\rightarrow\texttt{int})},
      {m \texttt{worra} t \texttt{bool}} returns {m \texttt{int}} ([worra] is
      [arrow] in reverse).
  *)
  val worra : t -> Ty.t -> Ty.t
end

module TupleComp : sig
  (** Operations on tuple types. *)

  type t = TupleComp.t
  type atom = TupleComp.Atom.t

  (** [as_union t] expresses [t] as an union of non-empty atoms. *)
  val as_union : t -> atom list

  (** [of_union n atoms] returns the [n]-uple component composed of the union [atoms]. *)
  val of_union : int -> atom list -> t

  (** [approx t] over-approximates [t] as a non-empty atom.
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise EmptyAtom if [t] is empty. *)
  val approx : t -> atom

  (** [proj n t] returns the type resulting from the projection on the
      [n]-th component (0-indexed) of [t]. 
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise Invalid_argument if [n] is negative or greater than the arity of [t].
  *)
  val proj : int -> t -> Ty.t

  (** [merge t1 t2] returns the atom resulting from the concatenation of
      [t1] and [t2]. *)
  val merge : atom -> atom -> atom
end

module Records : sig
  (** Operations on record types, ignores row variables. *)

  module Atom : sig
    module LabelMap : Map.S with type key=Label.t
    type t = { bindings : Ty.O.t LabelMap.t ; tail : Ty.O.t }
    val dom : t -> LabelSet.t
    val find : Label.t -> t -> Ty.O.t
  end

  type t = Records.t
  type atom = Atom.t

  (** [of_atom t] returns the record component of an atom. *)
  val of_atom : atom -> t

  (** [as_union t] over-approximates [t] as an union of non-empty atoms. *)
  val as_union : t -> atom list

  (** [of_union atoms] returns the record component composed of the union [atoms]. *)
  val of_union : atom list -> t

  (** [approx t] over-approximates [t] as a non-empty atom.
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise EmptyAtom if [t] is empty. *)
  val approx : t -> atom

  (** [proj l t] returns the (possibly absent) type resulting
      from the projection on the label [l] of [t]. 
  *)
  val proj : Label.t -> t -> Ty.O.t

  (** [merge t1 t2] returns the atom resulting from the merging of
      [t1] and [t2] (non-absent fields in [t2] override those in [t1]). *)
  val merge : atom -> atom -> t

  (** [remove t l] returns the atom obtained by making the field [l]
      absent in [t]. *)
  val remove : atom -> Label.t -> t
end

module Records' : sig
  (** Operations on record types. *)

  module Atom : sig
    module LabelMap : Map.S with type key=Label.t
    type t = { bindings : Ty.F.t LabelMap.t ; tail : Ty.F.t }
    val dom : t -> LabelSet.t
    val find : Label.t -> t -> Ty.F.t
  end

  type t = Records.t
  type atom = Atom.t

  (** [of_atom t] returns the record component of an atom. *)
  val of_atom : atom -> t

  (** [as_union t] over-approximates [t] as an union of non-empty atoms. *)
  val as_union : t -> atom list

  (** [of_union atoms] returns the record component composed of the union [atoms]. *)
  val of_union : atom list -> t

  (** [approx t] over-approximates [t] as a non-empty atom.
      {%html: <style>ul.at-tags > li > p { display: inline }</style>%}
      @raise EmptyAtom if [t] is empty. *)
  val approx : t -> atom

  (** [proj l t] returns the (possibly absent) type resulting
      from the projection on the label [l] of [t]. 
  *)
  val proj : Label.t -> t -> Ty.F.t

  (** [merge t1 t2] returns the atom resulting from the merging of
      [t1] and [t2] (non-absent fields in [t2] override those in [t1]). *)
  val merge : atom -> atom -> t

  (** [remove t l] returns the atom obtained by making the field [l]
      absent in [t]. *)
  val remove : atom -> Label.t -> t
end

module TagComp : sig
  (** Operations on tag types. *)

  type t = TagComp.t
  type atom = TagComp.Atom.t

  (** [is_identity t] returns [true] if and only if the component [t]
    is a tag component whose underlying interpretation is isomorphic
    to the identity, that is, if it is monotonic and {m \cap}-{m \cup}-preserving. *)
  val is_identity : t -> bool

  (** [preserves_cap t] returns [true] if and only if the component [t]
    is a tag component whose underlying interpretation preserves intersections. *)
  val preserves_cap : t -> bool

  (** [preserves_cup t] returns [true] if and only if the component [t]
    is a tag component whose underlying interpretation preserves unions. *)
  val preserves_cup : t -> bool

  (** [as_atom t] expresses [t] as an atom.
      Raises: [Invalid_argument] if the tag component does not satisfy [is_identity]. *)
  val as_atom : t -> atom

  (** [as_union t] expresses [t] as a union of atoms.
      Raises: [Invalid_argument] if the tag component does not satisfy [preserves_cap]. *)
  val as_union : t -> atom list
end
