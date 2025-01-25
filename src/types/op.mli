open Sstt_core

exception EmptyAtom

module Arrows : sig
    type t = Arrows.t

    val dom : t -> Ty.t
    val apply : t -> Ty.t -> Ty.t
    val worra : t -> Ty.t -> Ty.t
end

module Products : sig
    type t = Products.t
    type atom = Products.Atom.t

    val as_union : t -> atom list
    val approx : t -> atom (* Can raise EmptyAtom *)
    val proj : int -> t -> Ty.t
    val merge : atom -> atom -> atom
end

module Records : sig
    type t = Records.t
    type atom = Records.Atom.t

    val as_union : t -> atom list
    val approx : t -> atom (* Can raise EmptyAtom *)
    val proj : Label.t -> t -> Ty.t * bool
    val merge : atom -> atom -> t
    val remove : atom -> Label.t -> t
end
