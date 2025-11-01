open Cduce
open Additions

exception Unsupported

let transform_builtin b =
  match b with
  | Ast.TEmpty -> TBase TEmpty
  | TAny -> TBase TAny
  | TAnyTuple ->
    TRecord (true, [("card", TBase (TInt (Some Z.zero, None)), false)])
  | TAnyEnum -> TBase TAtom
  | TAnyInt -> TBase (TInt (None, None))
  | TAnyArrow -> TArrow (TBase TEmpty, TBase TAny)
  | TAnyRecord -> TRecord (true, [])
  | TAnyTupleComp i ->
    let i = Some (Z.of_int i) in
    TRecord (true, [("card", TBase (TInt (i, i)), false)])
  | TAnyTag -> raise Unsupported

let transform_str ty =
  let rec aux ty =
  match ty with
  | Ast.TBuiltin TAnyEnum -> TBase TAny
  | TNamed str -> TBase (TSString str)
  | TUnop (TNeg, ty) -> TNeg (aux ty)
  | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
  | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
  | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
  | _ -> raise Unsupported
  in
  TCap (aux ty, TBase TString)

let transform ty =
  let rec aux ty =
    match ty with
    | Ast.TBuiltin b -> transform_builtin b
    | TNamed n -> TVar n
    | TTag ("str", Some ty) -> transform_str ty
    | TTag _ -> raise Unsupported
    | TVar n | TVarMono n -> TVar n
    | TInterval (i1, i2) -> TBase (TInt (i1, i2))
    | TRecord (bindings, opened) ->
      TRecord (opened, List.map (fun (str,ty,b) -> str, aux ty, b) bindings)
    | TVarop (TTuple, tys) ->
      let n = Some (Z.of_int (List.length tys)) in
      let bindings = tys |>
        List.mapi (fun i ty -> "_"^(string_of_int i), aux ty, false) in
      TRecord (false, ("card", TBase (TInt (n, n)), false)::bindings)
    | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
    | TBinop (TArrow, ty1, ty2) -> TArrow (aux ty1, aux ty2)
    | TUnop (TNeg, ty) -> TNeg (aux ty)
    | TWhere (ty, eqs) ->
      TWhere (aux ty, List.map (fun (str,ty) -> str, [], aux ty) eqs)
  in
  aux ty

type ty = Base.typ
module TVarSet = Tvar.TVarSet
module TVar = Tvar.TVar
module Subst = Tvar.Subst

type env = { tenv : type_env ; vtenv : var_type_env }
let empty_env = { tenv=empty_tenv ; vtenv=empty_vtenv }

let declare_vars env names =
  let vs, vtenv = type_exprs_to_typs env.tenv env.vtenv
    (List.map (fun str -> TVar str) names)
  in
  { env with vtenv }, List.map
    (fun t -> match Tvar.check_var t with `Pos v -> v | _ -> assert false) vs

let build_tys env tys =
  let tys = List.map transform tys in
  let tys, vtenv = type_exprs_to_typs env.tenv env.vtenv tys in
  { env with vtenv }, tys

let tally mono constr =
  Tvar.Raw.tallying mono constr

let tally_with_prio vs mono constr =
  Tvar.Raw.tallying_with_prio vs mono constr
