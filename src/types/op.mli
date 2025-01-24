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

    val approx : t -> Products.Atom.t (* Can raise EmptyAtom *)
    val proj : int -> t -> Ty.t
    val merge : t -> t -> t
end

module Records : sig
    type t = Records.t

    val approx : t -> Records.Atom.t (* Can raise EmptyAtom *)
    val proj : Label.t -> t -> Ty.t * bool
    val merge : t -> t -> t
    val remove : t -> Label.t -> t
end
