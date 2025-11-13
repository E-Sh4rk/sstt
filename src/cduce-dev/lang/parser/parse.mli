val top_phrases : ?source:string ->  char Seq.t -> Ast.pmodule_item list
(** [top_phrases seq] returns a list of toplevel phrases,
    as described {{: http://www.cduce.org/manual_interpreter.html#phrases} here}.
    Each phrase must end with [;;].
    *)

val prog : ?source:string -> char Seq.t -> Ast.pmodule_item list
(** [prog seq] returns a list of toplevel phrases as can be
    written in a CDuce file.
*)

val pat : ?source:string -> char Seq.t -> Ast.ppat
(** [pat seq] returns a pattern (wich can be a type)
    written in [seq].
*)

val expr : ?source:string -> char Seq.t -> Ast.pexpr
(** [expr seq] returns the expression written in [seq].
*)

val seq_of_in_channel : in_channel -> char Seq.t
(** [seq_of_in_channel ic] returns an ephemeral char sequence
    from [ic].
*)

val seq_of_fun : (unit -> char option) -> char Seq.t
(** [seq_of_fun f] returns an ephemeral char sequence created
    from *)

(**/**)

val sync : unit -> unit