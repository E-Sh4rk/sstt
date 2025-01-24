open Core

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
| PEmpty | PAny | PAnyTuple | PAnyAtom | PAnyInt
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

val get : aliases -> Ty.t -> t
val print : Format.formatter -> t -> unit
val print_ty : aliases -> Format.formatter -> Ty.t -> unit
val print_subst : aliases -> Format.formatter -> Subst.t -> unit

val print_ty' : Format.formatter -> Ty.t -> unit
val print_subst' : Format.formatter -> Subst.t -> unit
