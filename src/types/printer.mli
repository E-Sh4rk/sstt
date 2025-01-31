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
| Neg
type binop =
| Diff | Arrow
type varop =
| Tuple | Cup | Cap
type builtin =
| Empty | Any | AnyTuple | AnyAtom | AnyTag | AnyInt
| AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of TagComp.Tag.t
type t = { main : descr ; defs : def list }
and def = NodeId.t * descr
and descr = { op : op ; ty : Ty.t }
and op =
| Printer of (Format.formatter -> unit)
| Custom of TagComp.Tag.t * custom
| Alias of string
| Node of NodeId.t
| Builtin of builtin
| Var of Var.t
| Atom of Atoms.Atom.t
| Tag of TagComp.Tag.t * descr
| Interval of Z.t option * Z.t option
| Record of (Label.t * descr * bool) list * bool
| Varop of varop * descr list
| Binop of binop * descr * descr
| Unop of unop * descr
and custom_param = CLeaf of descr | CRec of custom
and custom_params = custom_param list
and custom = CDef of NodeId.t * custom_params list | CNode of NodeId.t

type aliases = (Ty.t * string) list
type tag_param = LeafParam of Ty.t | RecParam of Ty.t
type tag_params = tag_param list
type custom_tags = (TagComp.Tag.t * (Ty.t -> tag_params list option)) list
type tags_printers = (TagComp.Tag.t * (custom -> (Format.formatter -> unit))) list
type params = { aliases : aliases ; tags : custom_tags ; printers : tags_printers }

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
