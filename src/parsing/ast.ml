
open Types

type builtin =
  | TEmpty | TAny | TAnyTuple | TAnyAtom | TAnyInt
  | TAnyArrow | TAnyRecord | TAnyProduct of int
type varop = TTuple
type binop = TCap | TCup | TDiff | TArrow
type unop = TNeg
type ty =
  | TBuiltin of builtin
  | TNamed of string
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
  | DefineAtoms of string list
  | DefineType of string * ty
  | Expr of string option * expr

type program = elt list

type command = Elt of elt | End

module StrMap = Map.Make(String)

type env = { aenv : Atoms.Atom.t StrMap.t ;
             tenv : Ty.t StrMap.t ;
             venv : Var.t StrMap.t ;
             mvenv : Var.t StrMap.t ;
             mono : VarSet.t ;
             lenv : Label.t StrMap.t
}

let empty_env = {
  aenv = StrMap.empty ; tenv = StrMap.empty ;
  venv = StrMap.empty ; mvenv = StrMap.empty ;
  mono = VarSet.empty ; lenv = StrMap.empty
}

let builtin t =
  match t with
  | TEmpty -> Ty.empty | TAny -> Ty.any
  | TAnyAtom -> Descr.mk_atoms (Atoms.any ()) |> Ty.mk_descr
  | TAnyInt -> Descr.mk_intervals (Intervals.any ()) |> Ty.mk_descr
  | TAnyTuple -> Descr.mk_tuples (Tuples.any ()) |> Ty.mk_descr
  | TAnyArrow -> Descr.mk_arrows (Arrows.any ()) |> Ty.mk_descr
  | TAnyRecord -> Descr.mk_records (Records.any ()) |> Ty.mk_descr
  | TAnyProduct n ->
    Tuples.Products.any n |> Descr.mk_products |> Ty.mk_descr

let tvar env str =
  begin match StrMap.find_opt str env.venv with
  | Some v -> v, env
  | None ->
    let v = Var.mk str in
    let venv = StrMap.add str v env.venv in
    let env = { env with venv } in
    v, env
  end

let tvar_mono env str =
  begin match StrMap.find_opt str env.mvenv with
  | Some v -> v, env
  | None ->
    let v = Var.mk str in
    let mvenv = StrMap.add str v env.mvenv in
    let mono = VarSet.add v env.mono in
    let env = { env with mvenv ; mono } in
    v, env
  end

let label env str =
  begin match StrMap.find_opt str env.lenv with
  | Some l -> l, env
  | None ->
    let l = Label.mk str in
    let lenv = StrMap.add str l env.lenv in
    let env = { env with lenv } in
    l, env
  end  

let build_ty env t =
  let rec aux env t =
  match t with
  | TBuiltin b -> builtin b, env
  | TNamed str ->
    begin match StrMap.find_opt str env.aenv with
    | Some a -> Descr.mk_atom a |> Ty.mk_descr, env
    | None -> StrMap.find str env.tenv, env
    end
  | TInterval (lb, ub) ->
    Intervals.Atom.mk' lb ub |> Descr.mk_interval |> Ty.mk_descr, env
  | TVar str ->
    let v, env = tvar env str in
    Ty.mk_var v, env
  | TVarMono str ->
    let v, env = tvar_mono env str in
    Ty.mk_var v, env
  | TRecord (bindings, opened) ->
    let (bindings,env) =
      List.fold_left (fun (res, env) (l,ty,b) ->
        let l, env = label env l in
        let ty, env = aux env ty in
        (l,(ty,b))::res,env
      ) ([], env) bindings in
    let bindings = LabelMap.of_list bindings in
    Descr.mk_record { bindings ; opened } |> Ty.mk_descr, env
  | TVarop (v, tys) ->
    let (tys,env) =
      List.fold_left (fun (res, env) ty ->
        let ty, env = aux env ty in
        ty::res,env
      ) ([], env) tys in
    let tys = List.rev tys in
    begin match v with
    | TTuple -> Descr.mk_product tys |> Ty.mk_descr, env
    end
  | TBinop (b, ty1, ty2) ->
    let ty1, env = aux env ty1 in
    let ty2, env = aux env ty2 in
    begin match b with
    | TArrow -> Descr.mk_arrow (ty1, ty2) |> Ty.mk_descr, env
    | TCap -> Ty.cap ty1 ty2, env
    | TCup -> Ty.cup ty1 ty2, env
    | TDiff -> Ty.diff ty1 ty2, env
    end
  | TUnop (u,ty) ->
    let ty, env = aux env ty in
    begin match u with
    | TNeg -> Ty.neg ty, env
    end
  | TWhere (ty, defs) ->
    let vars = defs |> List.map
      (fun (name, _) -> name, Var.mk name) |> StrMap.of_list in
    let env = ref env in
    List.iter (fun (name, _) ->
      let tenv = StrMap.add name (Ty.mk_var (StrMap.find name vars)) !env.tenv in
      env := {(!env) with tenv}
    ) defs ;
    let eqs = defs |> List.map (fun (name, ty) ->
      let (ty, env') = aux !env ty in
      env := env' ;
      (StrMap.find name vars, ty)
    ) in
    let tys = Ty.from_eqs eqs in
    List.iter2 (fun (name, _) ty ->
      let tenv = StrMap.add name ty !env.tenv in
      env := {(!env) with tenv}
    ) defs tys ;
    let (ty, env') = aux !env ty in
    env := env' ;
    List.iter (fun (name, _) ->
      let tenv = StrMap.remove name !env.tenv in
      env := {(!env) with tenv}
    ) defs ;
    ty, !env
  in
  aux env t

let build_subst env s =
  let env = ref env in
  let s = s |> List.map (fun (b,v,ty) ->
    let v, env' =
      if b then tvar_mono !env v else tvar !env v in
    let ty, env' = build_ty env' ty in
    env := env' ; (v,ty)
  ) |> Subst.mk
  in
  s, !env

let build_tally env cs =
  let env = ref env in
  let cs = cs |> List.map (fun (ty1,op,ty2) ->
    let ty1, env' = build_ty !env ty1 in
    let ty2, env' = build_ty env' ty2 in
    env := env' ;
    match op with
    | LEQ -> [ty1,ty2]
    | GEQ -> [ty2,ty1]
    | EQ -> [ty1,ty2 ; ty2,ty1]
  ) |> List.concat
  in
  cs, !env
