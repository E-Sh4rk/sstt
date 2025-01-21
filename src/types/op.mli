open Core

module Arrows : sig
    type t = Descr.Arrows.t

    val dom : t -> Ty.t
    val apply : t -> Ty.t -> Ty.t
    val worra : t -> Ty.t -> Ty.t
end

module Products : sig
    type t = Descr.Tuples.Products.t

    val proj : int -> t -> Ty.t
end

module Records : sig
    type t = Descr.Records.t

    val proj : Label.t -> t -> Ty.t * bool
end
