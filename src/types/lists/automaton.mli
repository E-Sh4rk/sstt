(* This module is taken from https://github.com/Tchou/stt *)

module type S = sig

    type lt
    (** The letter type *)

    type state = private int
    (** The state type *)

    type t
    (** The automaton type *)
  
    module R : Regexp.S
    (** The regexp module *)

    type regexp
    (** The regexp type *)
  
    val create : unit -> t
    (** [create ()] generates an empty automaton *)
  
    val mk_state : t -> state
    (** [mk_state auto] generates and returns a fresh state,
        while setting it in [auto]. *)

    val add_trans : t -> state -> lt -> state -> unit
    (** [add_trans auto state1 letter state2] adds a transition labelled [letter]
        between [state1] and [state2] in [auto].
        If the transition is already in [auto], does nothing.
        Raises : [state1] and [state2] must be [auto]'s states. *)
    
    val add_transitions : t -> (state * lt * state) list -> unit
    (** [add_transitions auto transitions] adds several transitions in [auto] *)

    val set_start : t -> state -> unit
    (** [set_start auto state] sets [state] as a start state in [auto].
        If it's already the case, does nothing.
        Raise : [state] must be an [auto]'s state *)

    val set_starts : t -> state list -> unit
    (** [set_starts auto states] sets several states as start states in [auto] *)

    val set_final : t -> state -> unit
    (** [set_final auto state] sets [state] as an final state in [auto].
        If it's already the case, does nothing.
        Raise : [state] must be an [auto]'s state *)
    
    val set_finals : t -> state list -> unit
    (** [set_finals auto states] sets several states as final states in [auto] *)
  
    val check_word : t -> lt list -> bool
    (** [check_word auto word] checks if [word] is recognized by [auto] *)
    
    val to_regex_my : t -> regexp
    (** [to_regex_my auto] returns the regex representing [auto]
        using the McNaughton-Yamada method.
        The returned value might be unsimplified *)
  
end
  
module Make (Lt : Regexp.Letter) : S with type lt = Lt.t
                                    and module R = Regexp.Make(Lt)
                                    and type regexp = Regexp.Make(Lt).t_simp
