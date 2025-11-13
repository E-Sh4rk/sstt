type 'a line = 'a list * 'a list
type 'a dnf = 'a line list
type ('a, 'b) bdd
type 'atom var_bdd = (Var.t, ('atom, bool) bdd) bdd

val empty : ('a, 'b) bdd

module type S = sig
  type atom
  (** The type of atoms in the Boolean combinations *)

  type mono
  (** The type of Boolean combinations of atoms. *)

  include Custom.T with type t = (Var.t, mono) bdd

  type line
  (** An explicit representation of conjunctions of atoms. *)

  type dnf
  (** An explicit representation of the DNF of atoms. *)

  val atom : atom -> t
  val mono : mono -> t
  val mono_dnf : mono -> dnf
  val any : t
  val empty : t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val get : t -> dnf
  val get_mono : t -> mono
  val iter : (atom -> unit) -> t -> unit

  val compute :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(atom -> 'b) ->
    t ->
    'b

  val var : Var.t -> t

  val extract_var : t -> (Var.t * t * t * t) option
  (** {2 Polymorphic interface. }*)

  val get_partial : t -> ((Var.t list * Var.t list) * mono) list
  val get_full : t -> ((Var.t list * Var.t list) * line) list
  val iter_partial : (Var.t -> unit) -> (mono -> unit) -> t -> unit
  val iter_full : (Var.t -> unit) -> (atom -> unit) -> t -> unit

  val compute_partial :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    mono:(mono -> 'b) ->
    var:(Var.t -> 'b) ->
    t ->
    'b

  val compute_full :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(atom -> 'b) ->
    var:(Var.t -> 'b) ->
    t ->
    'b

  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( // ) : t -> t -> t
  val ( ~~ ) : t -> t
end

module Bool : Tset.S with type t = bool and type elem = bool

module Make (E : Custom.T) :
  S
    with type atom = E.t
     and type line = E.t line
     and type dnf = E.t dnf
     and type mono = (E.t, Bool.t) bdd
     and type t = E.t var_bdd

module VarIntervals :
  S
    with type mono = Intervals.t
     and type atom = Intervals.elem
     and type dnf = Intervals.t

module VarCharSet :
  S
    with type mono = CharSet.t
     and type atom = CharSet.elem
     and type dnf = CharSet.t

module VarAtomSet :
  S
    with type mono = AtomSet.t
     and type atom = AtomSet.elem
     and type dnf = AtomSet.t

module VarAbstractSet :
  S
    with type mono = AbstractSet.t
     and type atom = AbstractSet.elem
     and type dnf = AbstractSet.t
