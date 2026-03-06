open Cduce
open Additions

exception Unsupported of string

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
    let bindings = List.init i (fun i -> "_"^(string_of_int i), TBase TAny, false) in
    let i = Some (Z.of_int i) in
    TRecord (false, ("card", TBase (TInt (i, i)), false)::bindings)
  | TAnyTag -> raise (Unsupported "unsupported AnyTag type")

let transform_str ty =
  let rec aux ty =
    match ty with
    | Ast.TBuiltin TAnyEnum -> TBase TAny
    | TNamed str -> TBase (TSString str)
    | TUnop (TNeg, ty) -> TNeg (aux ty)
    | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
    | _ -> raise (Unsupported "invalid string encoding")
  in
  let top = TBase TString in
  match ty with None -> top | Some ty -> TCap (aux ty, top)

let transform_flt ty =
  let rec aux ty =
    match ty with
    | Ast.TNamed _ -> TBase TAny
    | TUnop (TNeg, ty) -> TNeg (aux ty)
    | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
    | _ -> raise (Unsupported "invalid float encoding")
  in
  let top = TBase TFloat in
  match ty with None -> top | Some ty -> TCap (aux ty, top)

let transform_chr ty =
  let rec aux ty =
    match ty with
    | Ast.TInterval (Some i1, Some i2) ->
      if Z.equal Z.zero i1 && Z.equal (Z.of_int 255) i2 then
        TBase TAny
      else if Z.equal i1 i2 then
        TBase (TSChar (Char.chr (Z.to_int i1)))
      else
        TCup (TBase (TSChar (Char.chr (Z.to_int i1))),
              aux (Ast.TInterval (Some (Z.succ i1), Some i2)))
    | TUnop (TNeg, ty) -> TNeg (aux ty)
    | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
    | _ -> raise (Unsupported "invalid char encoding")
  in
  let top = TBase TChar in
  match ty with None -> top | Some ty -> TCap (aux ty, top)

let transform_bool ty =
  let rec aux ty =
    match ty with
    | Ast.TNamed "true" -> TBase TTrue
    | TNamed "false" -> TBase TFalse
    | TUnop (TNeg, ty) -> TNeg (aux ty)
    | TBinop (TCap, ty1, ty2) -> TCap (aux ty1, aux ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux ty1, aux ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux ty1, aux ty2)
    | _ -> raise (Unsupported "invalid bool encoding")
  in
  let top = TBase TBool in
  match ty with None -> top | Some ty -> TCap (aux ty, top)

type env = { tenv : type_env ; vtenv : var_type_env }
let empty_env = { tenv=empty_tenv ; vtenv=empty_vtenv }

module StrSet = Set.Make(String)
let transform (env:env) ty =
  let env = ref env in
  let rec aux local ty =
    match ty with
    | Ast.TBuiltin b -> transform_builtin b
    | TNamed str ->
      if StrSet.mem str local
      then TCustom ([], str)
      else aux_enum ("enum."^str)
    | TTag ("str", ty) -> transform_str ty
    | TTag ("flt", ty) -> transform_flt ty
    | TTag ("chr", ty) -> transform_chr ty
    | TTag ("bool", ty) -> transform_bool ty
    | TTag (name, arg) -> aux_tag local name arg
    | TVar (Mono, n) | TVar (Poly, n) -> TVar n
    | TVar _ -> raise (Unsupported ("unsupported row variables"))
    | TInterval (i1, i2) -> TBase (TInt (i1, i2))
    | TRecord (bindings, TUnop (TOption, TBuiltin TAny)) ->
      TRecord (true, List.map (aux_binding local) bindings)
    | TRecord (bindings, TUnop (TOption, TBuiltin TEmpty)) ->
      TRecord (false, List.map (aux_binding local) bindings)
    | TRecord _ -> raise (Unsupported ("only closed and open records are supported"))
    | TVarop (TTuple, tys) ->
      let n = Some (Z.of_int (List.length tys)) in
      let bindings = tys |>
        List.mapi (fun i ty -> "_"^(string_of_int i), aux local ty, false) in
      TRecord (false, ("card", TBase (TInt (n, n)), false)::bindings)
    | TBinop (TCap, ty1, ty2) -> TCap (aux local ty1, aux local ty2)
    | TBinop (TCup, ty1, ty2) -> TCup (aux local ty1, aux local ty2)
    | TBinop (TDiff, ty1, ty2) -> TDiff (aux local ty1, aux local ty2)
    | TBinop (TArrow, ty1, ty2) -> TArrow (aux local ty1, aux local ty2)
    | TUnop (TNeg, ty) -> TNeg (aux local ty)
    | TUnop (TOption, _) -> raise (Unsupported ("unexpected option"))
    | TWhere (ty, eqs) ->
      let local = List.fold_left (fun acc (str,_) -> StrSet.add str acc) local eqs in
      TWhere (aux local ty, List.map (fun (str,ty) -> str, [], aux local ty) eqs)
    and aux_binding local (str,tyo) =
      match tyo with
      | Ast.TUnop (TOption, ty) -> str, aux local ty, true
      | ty -> str, aux local ty, false
    and aux_tag local str arg =
      if String.starts_with ~prefix:"_" str then
        raise (Unsupported "unsupported opaque data types") ;
      let str = "tag."^str in
      match arg with
      | None -> TPair (aux_enum str, TBase TAny)
      | Some arg -> TPair (aux_enum str, aux local arg)
    and aux_enum str =
      begin try
          let tenv = Additions.define_atom !env.tenv str in
          env := { !env with tenv }
        with TypeDefinitionError _ -> ()
      end ;
      TCustom ([], String.capitalize_ascii str)
    in
  let res = aux StrSet.empty ty in
  !env, res

type ty = Base.typ
module TVarSet = Tvar.TVarSet
module TVar = Tvar.TVar
module Subst = Tvar.Subst

let resolve_vars env names =
  let vs, vtenv = type_exprs_to_typs env.tenv env.vtenv
    (List.map (fun str -> TVar str) names)
  in
  { env with vtenv }, List.map
    (fun t -> match Tvar.check_var t with `Pos v -> v | _ -> assert false) vs

let build_tys env tys =
  let env, tys = List.fold_left (fun (env, res) ty ->
    let env, ty = transform env ty in env, ty::res) (env,[]) tys in
  let tys, vtenv = type_exprs_to_typs env.tenv env.vtenv (List.rev tys) in
  { env with vtenv }, tys

let tally mono constr =
  Tvar.Raw.tallying mono constr

let tally_with_prio vs mono constr =
  Tvar.Raw.tallying_with_prio vs mono constr
