type uchar = int

module Utf8 : sig
  include Custom.T

  type uindex = private int

  val mk_check : string -> t option
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
  val empty : t
  val is_empty : t -> bool
  val end_index : t -> uindex
  val start_index : t -> uindex
  val equal_index : uindex -> uindex -> bool
  val mk : string -> t
  val mk_latin1 : string -> t
  val mk_char : int -> t
  val mk_idx : int -> uindex
  val get_str : t -> string
  val get_idx : uindex -> int
  val get : t -> uindex -> uchar
  val advance : t -> uindex -> uindex
  val rewind : t -> uindex -> uindex
  val next : t -> uindex -> uchar * uindex
  val concat : t -> t -> t
  val store : Buffer.t -> uchar -> unit
  val copy : Buffer.t -> t -> uindex -> uindex -> unit
  val get_substr : t -> uindex -> uindex -> string
end
