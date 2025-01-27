open Sstt_core

module NodeId : sig
    type t
    val mk : unit -> t
    val has_name : t -> bool
    val name : t -> string
    val rename : t -> string -> unit
    val hash : t -> int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
end

type unop =
| PNeg
type binop =
| PCup | PCap | PDiff | PArrow
type varop =
| PTuple
type builtin =
| PEmpty | PAny | PAnyTuple | PAnyAtom | PAnyTag | PAnyInt
| PAnyArrow | PAnyRecord | PAnyProduct of int
type t = descr * defs list
and defs = NodeId.t * descr
and descr = op * Ty.t
and op =
| PNamed of string
| PNode of NodeId.t
| PBuiltin of builtin
| PVar of Var.t
| PAtom of Atoms.Atom.t
| PInterval of Z.t option * Z.t option
| PRecord of (Label.t * descr * bool) list * bool
| PVarop of varop * descr list
| PBinop of binop * descr * descr
| PUnop of unop * descr

type aliases = (Ty.t * string) list

(** [get aliases ty] transforms the type [ty] into an algebraic form,
recognizing type aliases [aliases]. *)
val get : aliases -> Ty.t -> t

(** [print fmt t] prints the algebraic form [t] using formatter [fmt]. *)
val print : Format.formatter -> t -> unit

(** [print_ty aliases fmt ty] prints the type [ty] using formatter [fmt],
recognizing type aliases [aliases]. Same as [print fmt (get aliases ty)]. *)
val print_ty : aliases -> Format.formatter -> Ty.t -> unit

(** [print_subst aliases fmt s] prints the substitution [s] using formatter [fmt],
recognizing type aliases [aliases]. *)
val print_subst : aliases -> Format.formatter -> Subst.t -> unit

(** [print_ty' fmt ty] prints the type [ty] using formatter [fmt].
Same as [print_ty [] fmt ty]. *)
val print_ty' : Format.formatter -> Ty.t -> unit

(** [print_subst' fmt s] prints the substitution [s] using formatter [fmt].
Same as [print_subst [] fmt s]. *)
val print_subst' : Format.formatter -> Subst.t -> unit
