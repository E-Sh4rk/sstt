open Core
open Prec

(** Names for the recursive definitions introduced when printing a type. *)
module NodeId : sig
  type t
  (** The identity of a definition. Two occurrences of the same definition
      share the same {!t}. *)

  val mk : unit -> t
  (** Creates a fresh, yet unnamed, identifier. *)

  val has_name : t -> bool
  (** Tests whether a name has already been given to this identifier. *)

  val name : t -> string
  (** Returns the name of this identifier.
      @raise Invalid_argument if it has no name yet. *)

  val rename : t -> string -> unit
  (** Sets the name of this identifier. *)

  val hash : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
  (** Prints the name of this identifier, or a placeholder if it has none. *)
end

(** The types that have a dedicated syntax, and thus are not printed as an
    algebraic combination of smaller types. [AnyTupleComp n] is the type of all
    the [n]-uples, and [AnyTagComp tag] the type of all the values tagged with
    [tag]. *)
type builtin =
  | Empty | Any | AnyTuple | AnyEnum | AnyTag | AnyInt
  | AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of Tag.t

type descr = { op : op ; ty : Ty.t }
(** An algebraic representation [op] of the type [ty]. The two are kept
    together so that transformations of the representation can be checked
    against, or driven by, the type it denotes. *)

and op =
  | Extension of extension_node
      (** A type recognized by one of the {!extensions} of the printing
          context. *)
  | Alias of string  (** A type recognized as one of the {!aliases}. *)
  | Node of NodeId.t
      (** A reference to one of the definitions of the enclosing {!t}, used for
          recursive types and for sharing. *)
  | Builtin of builtin
  | Var of Var.t
  | Enum of Enum.t
  | Tag of Tag.t * descr
  | Interval of Z.t option * Z.t option
      (** An interval, [None] denoting an infinite bound. *)
  | Record of (Label.t * fdescr) list * fdescr
      (** The explicit fields of a record, and its tail. *)
  | Varop of varop * descr list
  | Binop of binop * descr * descr
  | Unop of unop * descr

and fdescr = { fop : fop ; fty : Ty.F.t }
(** An algebraic representation [fop] of the field type [fty]. *)

and fop =
  | FVarop of fvarop * fdescr list
  | FBinop of fbinop * fdescr * fdescr
  | FUnop of funop * fdescr
  | FTy of descr * bool
      (** A plain type, the boolean indicating whether the field is optional
          (that is, whether the type also contains the undefined value). *)
  | FRowVar of RowVar.t

and extension_node
(** The representation of a type by an {!extension_builder}, together with the
    functions needed to traverse and print it. *)

type def = NodeId.t * descr
(** A definition, printed after the [where] keyword. *)

type 'm t = { main : 'm ; defs : def list }
(** The representation [main] of the type(s) being printed (a {!descr}, an
    {!fdescr}, or a list of them), together with the definitions its [Node]
    references refer to. *)

type aliases = (Ty.t * string) list
(** Types that must be printed using the given name. *)

(* Printer extensions types and helper *)

type extension_builder
(** Knows how to recognize and print the tag components of a particular
    extension. Built with {!builder}. *)

type build_ctx = { build : Ty.t -> descr ; build_field : Ty.F.t -> fdescr }
(** Converts the types occurring inside an extension into algebraic
    representations, taking care of their sharing and aliases. *)

(** Traverses the representation of an extension, applying the given functions
    to every {!descr} and {!fdescr} it contains. *)
type 'a map = (descr -> descr) -> (fdescr -> fdescr) -> 'a -> 'a
val builder :
  to_t:(build_ctx -> TagComp.t -> 'a option) ->
  map:'a map ->
  print:(int -> assoc -> Format.formatter -> 'a -> unit) ->
  extension_builder
(** [builder ~to_t ~map ~print] returns an extension builder that knows how
      to print values of a particular extension.

    [to_t ctx comp] converts the tag component [comp] into [Some e], where [e]
    is some arbitrary representation of [comp]. It can use [ctx.build ty] (resp.
    [ctx.build_field fty]) if it wishes to convert a type [ty] (resp. a field
    type [fty]) occurring in [comp] to an algebraic representation of type
    [descr] (resp. [fdescr]); sharing and aliases of these sub-types are then
    taken care of by the printer. If the conversion fails, [to_t] returns
    [None].

    [map f ff e] traverses the representation [e], applying [f] to every
    [descr] and [ff] to every [fdescr] it contains.

    [print prec assoc fmt e] pretty-prints the representation [e] at
    precedence [prec] and associativity [assoc], using the formatter [fmt].
*)

type extensions = (Tag.t * extension_builder) list
(** The extension builder to use for each tag. *)

type params = { aliases : aliases ; extensions : extensions }
(** A pretty-printing context. *)

val any_descr : descr
val empty_descr : descr
val cup_descr : descr -> descr -> descr
val cap_descr : descr -> descr -> descr
val neg_descr : descr -> descr

val any_fdescr : fdescr
val empty_fdescr : fdescr
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

(** [print_field_ctx prec assoc fmt fd] prints the field descriptor [fd] in a
    context with precedence [prec] and associativity [assoc], using formatter
    [fmt]. *)
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
