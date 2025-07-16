open Core
open Prec

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

module type CustomNode = sig
    type t
    val v : t
    val print : int -> assoc -> Format.formatter -> t -> unit
end

type builtin =
| Empty | Any | AnyTuple | AnyAtom | AnyTag | AnyInt
| AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of TagComp.Tag.t
type 'c t' = { main : 'c descr' ; defs : 'c def' list }
and 'c def' = NodeId.t * 'c descr'
and 'c descr' = { op : 'c op' ; ty : Ty.t }
and 'c op' =
| Custom of 'c
| Alias of string
| Node of NodeId.t
| Builtin of builtin
| Var of Var.t
| Enum of Enums.Atom.t
| Tag of TagComp.Tag.t * 'c descr'
| Interval of Z.t option * Z.t option
| Record of (Label.t * 'c descr' * bool) list * bool
| Varop of varop * 'c descr' list
| Binop of binop * 'c descr' * 'c descr'
| Unop of unop * 'c descr'

type t = (module CustomNode) t'
type descr = (module CustomNode) descr'
type def = (module CustomNode) def'
type op = (module CustomNode) op'

(* Printer extensions *)
type ('u, 'l, 'r) cparam = PUnprocessed of 'u | PLeaf of 'l | PRec of 'r
type ('u, 'l, 'r) cparams = { pid : int list ; pdef : ('u, 'l, 'r) cparam list }
type custom = CDef of NodeId.t * (Ty.t, descr, custom) cparams list | CNode of NodeId.t
type extracted_params = (Ty.t, Ty.t, Ty.t) cparams list
module type PrinterExt = sig
    type t
    val tag : TagComp.Tag.t
    val extractors : (Ty.t -> extracted_params option) list
    val get : custom -> t
    val print : int -> assoc -> Format.formatter -> t -> unit
end

type aliases = (Ty.t * string) list
type extensions = (module PrinterExt) list
type params = { aliases : aliases ; extensions : extensions }

val cup_descr : descr -> descr -> descr
val cap_descr : descr -> descr -> descr
val neg_descr : descr -> descr

val empty_params : params

val merge_params : params list -> params

(** [get aliases ty] transforms the type [ty] into an algebraic form,
recognizing type aliases [aliases]. *)
val get : params -> Ty.t -> t

(** [print fmt t] prints the algebraic form [t] using formatter [fmt]. *)
val print : Format.formatter -> t -> unit

(** [print_descr fmt d] prints the printer descriptor [d] using formatter [fmt]. *)
val print_descr : Format.formatter -> descr -> unit

(** [print_descr_atomic fmt d] prints the printer descriptor [d] in an atomic way
    (adding parentheses if necessary) using formatter [fmt]. *)
val print_descr_atomic : Format.formatter -> descr -> unit

(** [print_descr_ctx prec assoc fmt d] prints the printer descriptor [d] in a context
with precedence [prec] and associativity [assoc], using formatter [fmt]. *)
val print_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit

(** [print_ty aliases fmt ty] prints the type [ty] using formatter [fmt],
recognizing type aliases [aliases]. Same as [print fmt (get aliases ty)]. *)
val print_ty : params -> Format.formatter -> Ty.t -> unit

(** [print_subst aliases fmt s] prints the substitution [s] using formatter [fmt],
recognizing type aliases [aliases]. *)
val print_subst : params -> Format.formatter -> Subst.t -> unit

(** [print_ty' fmt ty] prints the type [ty] using formatter [fmt].
Same as [print_ty [] fmt ty]. *)
val print_ty' : Format.formatter -> Ty.t -> unit

(** [print_subst' fmt s] prints the substitution [s] using formatter [fmt].
Same as [print_subst [] fmt s]. *)
val print_subst' : Format.formatter -> Subst.t -> unit
