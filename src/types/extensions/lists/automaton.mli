
module Make(R:Regexp.Regexp) : sig
    type state
    type t

    val create : unit -> t

    val is_initial : t -> state -> bool
    val is_final : t -> state -> bool

    (** The first state created is initial. *)
    val mk_state : t -> state

    val add_trans : t -> state -> R.lt -> state -> unit

    val set_final : t -> state -> unit

    val to_regexp : t -> R.t
end
