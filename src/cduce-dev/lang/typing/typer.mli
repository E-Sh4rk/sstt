open Ident
open Encodings

type t

val empty_env : t
val register_types : string -> t -> unit
(* Register types of the environment for the pretty-printer *)

val find_value : id -> t -> Types.t
val enter_type : id -> Types.t * Var.t list -> t -> t
val iter_values : t -> (id -> Types.t -> unit) -> unit
val typ : t -> Ast.ppat -> Types.Node.t
val var_typ : (Utf8.t * Var.t) list -> t -> Ast.ppat -> Types.Node.t
val pat : t -> Ast.ppat -> Patterns.node
val dump_types : Format.formatter -> t -> unit
val dump_ns : Format.formatter -> t -> unit
val set_ns_table_for_printer : t -> unit
val type_using : t -> Cduce_loc.loc -> Utf8.t -> Utf8.t -> t
val type_schema : t -> Cduce_loc.loc -> Utf8.t -> string -> t
val type_ns : t -> Cduce_loc.loc -> Utf8.t -> Ast.ns_expr -> t
val type_open : t -> Cduce_loc.loc -> Utf8.t list -> t
val type_keep_ns : t -> bool -> t
val type_expr : t -> Ast.pexpr -> t * Typed.texpr * Types.descr
val type_defs : t -> (Cduce_loc.loc * Utf8.t * Utf8.t list * Ast.ppat) list -> t

val type_let_decl :
  t -> Ast.ppat -> Ast.pexpr -> t * Typed.let_decl * (id * Types.t) list

val type_let_funs :
  t -> Ast.pexpr list -> t * Typed.texpr list * (id * Types.t) list
(* Assume that all the expressions are Abstractions *)

val check_weak_variables : t -> unit
(* Operators *)

type type_fun = Cduce_loc.loc -> Types.t -> bool -> Types.t

val register_op : string -> int -> (type_fun list -> type_fun) -> unit
val flatten : type_fun -> type_fun

(* Forward definitions *)
val from_comp_unit : (Compunit.t -> t) ref
(* From Librarian *)

val load_comp_unit : (Utf8.t -> Compunit.t) ref
(* From Librarian *)

val has_ocaml_unit : (Utf8.t -> bool) ref
val has_static_external : (string -> bool) ref
