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

type builtin =
  | Empty | Any | AnyTuple | AnyEnum | AnyTag | AnyInt
  | AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of Tag.t

type descr = { op : op ; ty : Ty.t }
and op =
  | Extension of extension_node
  | Alias of string
  | Node of NodeId.t
  | Builtin of builtin
  | Var of Var.t
  | Enum of Enum.t
  | Tag of Tag.t * descr
  | Interval of Z.t option * Z.t option
  | Record of (Label.t * fdescr) list * fdescr
  | Varop of varop * descr list
  | Binop of binop * descr * descr
  | Unop of unop * descr
and fdescr = { fop : fop ; fty : Ty.F.t }
and fop =
  | FVarop of fvarop * fdescr list
  | FBinop of fbinop * fdescr * fdescr
  | FUnop of funop * fdescr
  | FTy of descr * bool
  | FRowVar of RowVar.t
and extension_node
type def = NodeId.t * descr
type 'm t = { main : 'm ; defs : def list }

(** A pretty-printing context. *)

type aliases = (Ty.t * string) list

(* Printer extensions types and helper *)

type extension_builder
type build_ctx = { build : Ty.t -> descr ; build_field : Ty.F.t -> fdescr }

val builder :
  to_t:(build_ctx -> TagComp.t -> 'a option) ->
  map:((descr -> descr) -> 'a -> 'a) ->
  print:(int -> assoc -> Format.formatter -> 'a -> unit) ->
  extension_builder
(** [builder ~to_t ~map ~print] returns an extension builder that knows how
      to print values of a particular extension.

    [to_t] is a function such that [to_t node ctx ty] converts [ty] into [Some
    e], where [e] is some arbitrary representation of [ty]. The function can use
    the parameter [node ctx ty'] if it wishes to convert [ty'] to an algebraic
    representation (of type [descr]). The sharing and aliases of [ty'] is
    controled by the pretty-printing context [ctx]. If the conversion fails,
    [to_t] returns [None].

    [map f e] traverses the representation [e], applying [f] to every [descr] it
    contains.
    
    [print ctx assoc fmt e] pretty-prints the representation [e] at
    precedence [ctx] and associativity [assoc], using the formatter [fmt].
*)

type extensions = (Tag.t * extension_builder) list
type params = { aliases : aliases ; extensions : extensions }

val cup_descr : descr -> descr -> descr
val cap_descr : descr -> descr -> descr
val neg_descr : descr -> descr
val cup_fdescr : fdescr -> fdescr -> fdescr
val cap_fdescr : fdescr -> fdescr -> fdescr
val neg_fdescr : fdescr -> fdescr

val map_descr : (descr -> op) -> (fdescr -> fop) -> descr -> descr
val map_fdescr : (descr -> op) -> (fdescr -> fop) -> fdescr -> fdescr
val map : (descr -> op) -> (fdescr -> fop) -> descr t -> descr t
val map_f : (descr -> op) -> (fdescr -> fop) -> fdescr t -> fdescr t

val empty_params : params

val merge_params : params list -> params

(** [get ~factorize params ty] transforms the type [ty] into an algebraic form,
    recognizing type aliases and extensions in [params]. If [~factorize] is [true]
    (default: [false]), some nodes may be factorized by introducing intermediate definitions
    when it makes the result more concise. *)
val get : ?factorize:bool -> params -> Ty.t -> descr t

(** [get'] is the same as [get] but for converting multiple types at once. *)
val get' : ?factorize:bool -> params -> Ty.t list -> descr list t

(** [get_field f fty] transforms the field type [fty] into an algebraic form. *)
val get_field : ?factorize:bool -> params -> Ty.F.t -> fdescr t

(** [get_field'] is the same as [get_field] but for converting multiple fields at once. *)
val get_field' : ?factorize:bool -> params -> Ty.F.t list -> fdescr list t

(** [print_builtin fmt bt] prints the builtin type [bt] using formatter [fmt]. *)
val print_builtin : Format.formatter -> builtin -> unit

(** [print_interval fmt i] prints the interval [i] using formatter [fmt]. *)
val print_interval : Format.formatter -> (Z.t option * Z.t option) -> unit

(** [print fmt t] prints the algebraic form [t] using formatter [fmt]. *)
val print : Format.formatter -> descr t -> unit

(** [print_descr fmt d] prints the printer descriptor [d] using formatter [fmt]. *)
val print_descr : Format.formatter -> descr -> unit

(** [print_descr_atomic fmt d] prints the printer descriptor [d] in an atomic way
    (adding parentheses if necessary) using formatter [fmt]. *)
val print_descr_atomic : Format.formatter -> descr -> unit

(** [print_descr_ctx prec assoc fmt d] prints the printer descriptor [d] in a context
    with precedence [prec] and associativity [assoc], using formatter [fmt]. *)
val print_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit

(** [print_field fmt fd] prints the algebraic form [fd] using formatter [fmt]. *)
val print_field_ctx : int -> assoc -> Format.formatter -> fdescr -> unit

(** [print_ty params fmt ty] prints the type [ty] using formatter [fmt],
    recognizing type aliases and extensions in [params]. Same as [print fmt (get params ty)]. *)
val print_ty : params -> Format.formatter -> Ty.t -> unit

(** [print_row params fmt r] prints the row [r] using formatter [fmt],
    recognizing type aliases and extensions in [params]. *)
val print_row : params -> Format.formatter -> Row.t -> unit

(** [print_subst params fmt s] prints the substitution [s] using formatter [fmt],
    recognizing type aliases and extensions in [params]. *)
val print_subst : params -> Format.formatter -> Subst.t -> unit

(** [print_ty' fmt ty] prints the type [ty] using formatter [fmt].
    Same as [print_ty [] fmt ty]. *)
val print_ty' : Format.formatter -> Ty.t -> unit

(** [print_row' fmt r] prints the row [r] using formatter [fmt].
    Same as [print_row [] fmt r]. *)
val print_row' : Format.formatter -> Row.t -> unit

(** [print_subst' fmt s] prints the substitution [s] using formatter [fmt].
    Same as [print_subst [] fmt s]. *)
val print_subst' : Format.formatter -> Subst.t -> unit

(** [print_extension_node_ctx prec assoc fmt e] prints the printer
    extensions node [e] in a context with precedence [prec]
    and associativity [assoc], using formatter [fmt]. *)
val print_extension_node_ctx :
  int -> assoc -> Format.formatter -> extension_node -> unit
