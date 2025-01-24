open Sstt_parsing.Ast
open Sstt_types
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
      RTy (carthesian_product tys1 tys2 |> List.map apply)
    | RSubst s1, RSubst s2 ->
      RSubst (carthesian_product s1 s2 |> List.map (fun (s1, s2) -> Subst.compose s2 s1))
    | RTy ty, RSubst s ->
      RTy (carthesian_product ty s |> List.map (fun (ty, s) -> Subst.apply s ty))
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
    RBool (carthesian_product tys1 tys2 |> List.map aux), env
  
let customs env =
  StrMap.bindings env.tenv |> List.map (fun (str, ty) -> (ty, str))

let print_res env fmt res =
  match res with
  | RBool bs ->
    let print_bool fmt b = Format.fprintf fmt "%b" b in
    Format.fprintf fmt "%a" (print_seq_space print_bool) bs
  | RTy tys ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Printer.print_ty (customs env))) tys
  | RSubst ss ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Printer.print_subst (customs env))) ss

let treat_elt env elt =
  match elt with
  | DefineAtoms lst ->
    let aenv =
      List.fold_left (fun aenv str ->
          let atom = Atoms.Atom.mk str in
          StrMap.add str atom aenv
        ) env.aenv lst
    in
    { env with aenv }
  | DefineType (str, ty) ->
    let ty,env = build_ty env ty in
    let tenv = StrMap.add str ty env.tenv in
    { env with tenv }
  | Expr (str, e) ->
    let r, env = compute_expr env e in
    begin match str with
    | None -> print Msg "@[<v 0>%a@]" (print_res env) r
    | Some str -> print Msg "%s:@[<v 0> %a@]" str (print_res env) r
    end ;
    env
