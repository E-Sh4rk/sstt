open Sstt_types

type builtin =
  | TEmpty | TAny | TAnyTuple | TAnyAtom | TAnyTag | TAnyInt
  | TAnyArrow | TAnyRecord | TAnyTupleComp of int | TAnyTagComp of TagComp.Tag.t
type varop = TTuple
type binop = TCap | TCup | TDiff | TArrow
type unop = TNeg
type ty =
  | TBuiltin of builtin
  | TNamed of string
  | TTag of string * ty
  | TVar of string
  | TVarMono of string
  | TInterval of Z.t option * Z.t option
  | TRecord of (string * ty * bool) list * bool
  | TVarop of varop * ty list
  | TBinop of binop * ty * ty
  | TUnop of unop * ty
  | TWhere of ty * (string * ty) list

type op = LEQ | EQ | GEQ

type subst = (bool * string * ty) list
type tally = (ty * op * ty) list
type expr =
  | CTy of ty
  | CSubst of subst
  | CTally of tally
  | CCat of expr * expr
  | CApp of expr * expr
  | CCmp of expr * op * expr

type elt =
  | DefineType of string list * expr
  | Expr of string option * expr

type program = elt list

type command = Elt of elt | End

module StrMap : Map.S with type key=string

type env = { aenv : Atoms.Atom.t StrMap.t ;
             tagenv : TagComp.Tag.t StrMap.t ;
             tenv : Ty.t StrMap.t ;
             venv : Var.t StrMap.t ;
             mvenv : Var.t StrMap.t ;
             mono : VarSet.t ;
             lenv : Label.t StrMap.t
}

val empty_env : env

val build_ty : env -> ty -> Ty.t * env
val build_subst : env -> subst -> Subst.t * env
val build_tally : env -> tally -> Tallying.constr list * env
