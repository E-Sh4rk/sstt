open Cduce

exception Unsupported

type ty = Base.typ
module TVarSet = Tvar.TVarSet
module TVar = Tvar.TVar
module Subst = Tvar.Subst

type env
val empty_env : env
val declare_vars : env -> string list -> env * TVar.t list
val build_tys : env -> Ast.ty list -> env * ty list
val tally : TVarSet.t -> (ty * ty) list -> Subst.t list
val tally_with_prio : TVar.t list -> TVarSet.t -> (ty * ty) list -> Subst.t list
