open Ast
open Sstt
open Sstt_utils

open Output

type res = RBool of bool list | RTy of Ty.t list | RSubst of Subst.t list
let empty_env = empty_env

let poly_leq env t1 t2 =
  let vars = VarSet.union (Ty.vars t1) (Ty.vars t2) in
  if VarSet.subset vars env.mono then
    Ty.leq t1 t2
  else
    Tallying.tally env.mono [ t1, t2 ] |> List.is_empty |> not

let rec compute_expr env e =
  match e with
  | CTy ty ->
    let ty, env = build_ty env ty in
    RTy [ty], env
  | CSubst s ->
    let s, env = build_subst env s in
    RSubst [s], env
  | CTally cs ->
    let cs, env = build_tally env cs in
    RSubst (Tallying.tally env.mono cs), env
  | CCat (e1, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let r = match r1, r2 with
    | RBool b1, RBool b2 -> RBool (b1@b2)
    | RTy ty1, RTy ty2 -> RTy (ty1@ty2)
    | RSubst s1, RSubst s2 -> RSubst (s1@s2)
    | _, _ -> failwith "Heterogeneous collection."
    in r, env
  | CApp (e1, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let r = match r1, r2 with
    | RTy tys1, RTy tys2 ->
      let apply (ty1, ty2) =
        let arrow = Ty.get_descr ty1 |> Descr.get_arrows in
        Op.Arrows.apply arrow ty2
      in
      RTy (cartesian_product tys1 tys2 |> List.map apply)
    | RSubst s1, RSubst s2 ->
      RSubst (cartesian_product s1 s2 |> List.map (fun (s1, s2) -> Subst.compose s2 s1))
    | RTy ty, RSubst s ->
      RTy (cartesian_product ty s |> List.map (fun (ty, s) -> Subst.apply s ty))
    | _, _ -> failwith "Invalid application."
    in
    r, env
  | CCmp (e1, op, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let tys1, tys2 =
      match r1, r2 with
      | RTy tys1, RTy tys2 -> tys1, tys2
      | _, _ -> failwith "Comparison between non-type values."
    in
    let aux (ty1, ty2) =
      match op with
      | LEQ -> poly_leq env ty1 ty2
      | GEQ -> poly_leq env ty2 ty1
      | EQ -> poly_leq env ty1 ty2 && poly_leq env ty2 ty1
    in
    RBool (cartesian_product tys1 tys2 |> List.map aux), env

let params pparams env =
  let aliases =
    StrMap.bindings env.tenv |> List.map (fun (str, ty) -> (ty, str))
  in
  Printer.merge_params [ pparams ; { Printer.empty_params with aliases } ]

let print_res pparams env fmt res =
  match res with
  | RBool bs ->
    let print_bool fmt b = Format.fprintf fmt "%b" b in
    Format.fprintf fmt "%a" (print_seq_space print_bool) bs
  | RTy tys ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Printer.print_ty (params pparams env))) tys
  | RSubst ss ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Printer.print_subst (params pparams env))) ss

let treat_def env def =
  match def with
  | DAtom str ->
    let eenv = StrMap.add str (Enum.mk str) env.eenv in
    { env with eenv }
  | DTag (str, props) ->
    let props =
      match props with
      | PNone -> Tag.NoProperty
      | PMono -> Tag.Monotonic { preserves_cap=false ; preserves_cup=false }
      | PAnd  -> Tag.Monotonic { preserves_cap=true ; preserves_cup=false }
      | POr   -> Tag.Monotonic { preserves_cap=false ; preserves_cup=true }
      | PId   -> Tag.Monotonic { preserves_cap=true  ; preserves_cup=true  }  
    in
    let tagenv = StrMap.add str (Tag.mk' str props) env.tagenv in
    { env with tagenv }

let treat_elt ?(pparams=Printer.empty_params) env elt =
  match elt with
  | DefineAlias (ids, e) ->
    let r, env = compute_expr env e in
    begin match r with
    | RTy tys when List.length tys = List.length ids ->
      let tenv = List.fold_left (fun tenv (str,ty) -> StrMap.add str ty tenv)
        env.tenv (List.combine ids tys) in
      { env with tenv }
    | _ -> failwith "Definitions must be types." 
    end
  | Define defs -> List.fold_left treat_def env defs
  | Expr (str, e) ->
    let r, env = compute_expr env e in
    begin match str with
    | None -> print Msg "@[<v 0>%a@]" (print_res pparams env) r
    | Some str -> print Msg "%s:@[<v 0> %a@]" str (print_res pparams env) r
    end ;
    env
