type t = {
  uid : int;
  mutable recurs : int;
  mutable def : def;
}

and def =
  | Link of t
  | Arrow of string * t * t
  | Tuple of t list
  | PVariant of (string * t option) list (* Polymorphic variant *)
  | Variant of string * (Ocaml_common.Ident.t * t list * t option) list * bool
  | Record of string * (Ocaml_common.Ident.t * t) list * bool
  | Builtin of string * t list
  | Abstract of string
  | Var of int
(** A representation of the subset of OCaml types that are supported. *)


module HashType : Hashtbl.S with type key = t

val reg_uid : t -> unit
(** [reg_uid] t internalise the component of the type t that can
    be reached and flags them as recursive if t is  a recursive type.
*)


(* Load an external .cmi *)
val has_cmi : string -> bool
(** [has_name n] returns true if the file (n ^ ".cmi")  exists on the
  load path.
*)

val load_module : string -> (string * t) list
(** [load_module mpath] returns the list of value definitions present in the
  module given by the module path (qualified module name) mpath. If mpath
  denotes an alias, it is resolved. If a proper structure definition is found,
  its content is returned, otherwise an error is raised.
*)

val read_cmi : string -> string * (string * Ocaml_common.Types.type_expr * t) list
(** [read_cmi name] finds and loads in the current load path a file "name" ^
   ".cmi". The content of the .mli file, and a triple of a name, OCaml type and
   simplified type list are returned.
*)

val print : Format.formatter -> t -> unit
val print_ocaml : Format.formatter -> Ocaml_common.Types.type_expr -> unit

val find_value : string -> t * int
(** [find_value name] returns the simple type as well as the number of type parameters
    of the value denoted by [name] which can be a qualified name, for instance for 
    "Stdlib.List.map", the function would return a representation of
    ('a -> 'b) -> 'a list -> 'b list, 2
*)