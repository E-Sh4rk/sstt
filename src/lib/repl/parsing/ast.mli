open Sstt

type builtin =
  | TEmpty | TAny | TAnyTuple | TAnyEnum | TAnyTag | TAnyInt
  | TAnyArrow | TAnyRecord | TAnyTupleComp of int
type varop = TTuple
type binop = TCap | TCup | TDiff | TArrow
type unop = TNeg
type tail = TOpen | TClosed | TRowVar of string
type ty =
  | TBuiltin of builtin
  | TNamed of string
  | TTag of string * ty option
  | TVar of string
  | TVarMono of string
  | TInterval of Z.t option * Z.t option
  | TRecord of (string * ty * bool) list * tail
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

type tag_prop = PNone | PMono | PAnd | POr | PId | PAndEx | POrEx
type def =
  | DAtom of string
  | DTag of string * tag_prop
type elt =
  | DefineAlias of string list * expr
  | Define of def list
  | Expr of string option * expr

type program = elt list

type command = Elt of elt | End

module StrMap : Map.S with type key=string

type env = { eenv : Enum.t StrMap.t ;
             tagenv : Tag.t StrMap.t ;
             tenv : Ty.t StrMap.t ;
             venv : Var.t StrMap.t ;
             mvenv : Var.t StrMap.t ;
             rvenv : RowVar.t StrMap.t ;
             mono : VarSet.t ;
             lenv : Label.t StrMap.t
}

val empty_env : env

val build_ty : env -> ty -> Ty.t * env
val build_subst : env -> subst -> Subst.t * env
val build_tally : env -> tally -> Tallying.constr list * env
