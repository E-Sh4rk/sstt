(* Representation of programs used by the runtime evaluator.
   Similar to the typed abstract syntax tree representation, but:
   - the pattern matching is compiled;
   - the identifiers locations are resolved. *)

open Ident
open Encodings

type var_loc =
  | Local of int (* Slot in the table of locals *)
  | Env of int (* Slot in the environment *)
  | Ext of {
      cu : Compunit.t;
      index : int;
      mutable value : Value.t;
    }
  (* Global slot from a given compilation unit *)
  | External of {
      cu : Compunit.t;
      index : int;
      mutable value : Value.t;
    }
  (* OCaml External *)
  (* If pos < 0, the first arg is the value *)
  | Builtin of string (* OCaml external embedded in the runtime *)
  | Global of int (* Only for the toplevel *)
  | Dummy

type expr =
  | Var of {
      loc : var_loc;
      mutable value : Value.t;
    }
  | Apply of expr * expr
  | Abstraction of
      var_loc array * (Types.t * Types.t) list * branches * int * bool
    (* environment, interface, branches, size of locals *)
  | Check of expr * Auto_pat.state
  | Const of Value.t
  | Pair of expr * expr
  | Xml of expr * expr * expr
  | XmlNs of expr * expr * expr * Ns.table
  | Record of expr Imap.t
  | String of Utf8.uindex * Utf8.uindex * Utf8.t * expr
  | Match of expr * branches
  | Map of expr * branches
  | Transform of expr * branches
  | Xtrans of expr * branches
  | Try of expr * branches
  | Validate of expr * Schema_validator.t
  | RemoveField of expr * label
  | Dot of expr * label
  | Ref of expr * Types.Node.t
  | Op of {
      name : string;
      args : expr list;
      mutable code : (Value.t list -> Value.t) option;
    }
  | NsTable of Ns.table * expr

and branches = {
  brs_accept_chars : bool;
  brs_disp : Auto_pat.state;
  brs_rhs : expr Auto_pat.rhs array;
  brs_stack_pos : int;
}

type code_item =
  | Eval of expr * int (* expression, size of locals *)
  | LetDecls of expr * int * Auto_pat.state * int
    (* expression, size of locals, dispatcher, number of globals to set *)
  | LetDecl of expr * int

type code = code_item list
