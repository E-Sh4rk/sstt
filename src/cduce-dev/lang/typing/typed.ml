(* Typed abstract syntax *)

(* Some sub-expression may have to be type-checked several times.
   We first build the ``skeleton'' of the typed ast
   (basically the parsed ast with types and patterns replaced with their
   internal representation), then type check it.

   The exp_typ and br_typ fields are updated to capture all the possible
   values than can result from the expression or flow to the branch
*)

open Cduce_loc
open Ident
module U = Encodings.Utf8

type tpat = Patterns.node
type ttyp = Types.Node.t

type texpr = {
  exp_loc : loc;
  mutable exp_typ : Types.t;
  (* Currently exp_typ is not used. It will be used for compilation ! *)
  exp_descr : texpr';
}

and texpr' =
  | Forget of texpr * ttyp
  | Check of Types.t ref * texpr * ttyp
  (* CDuce is a Lambda-calculus ... *)
  | Var of id
  | ExtVar of Compunit.t * id * Types.t
  | Apply of texpr * texpr
  | Abstraction of abstr
  (* Data constructors *)
  | Cst of Types.const
  | Pair of texpr * texpr
  | Xml of texpr * texpr * Ns.table option
  | RecordLitt of texpr label_map
  | String of U.uindex * U.uindex * U.t * texpr
  | Abstract of AbstractSet.V.t
  (* Data destructors *)
  | Match of texpr * branches
  | Map of texpr * branches
  | Transform of texpr * branches
  | Xtrans of texpr * branches
  | Validate of texpr * Types.t * Schema_validator.t
  | RemoveField of texpr * label
  | Dot of texpr * label
  (* Exception *)
  | Try of texpr * branches
  | Ref of texpr * ttyp
  | External of Types.t * [ `Builtin of (string*int) | `Ext of int ]
  | Op of string * int * texpr list
  | NsTable of Ns.table * texpr'

and abstr = {
  fun_name : id option;
  fun_iface : (Types.t * Types.t) list;
  fun_body : branches;
  fun_typ : Types.t;
  fun_fv : fv;
  fun_is_poly : bool;
}

and let_decl = {
  let_pat : tpat;
  let_body : texpr;
}

and branches = {
  mutable br_typ : Types.t;
  (* Type of values that can flow to branches *)
  br_accept : Types.t;
  (* Type accepted by all branches *)
  br_branches : branch list;
}

and branch = {
  br_loc : loc;
  mutable br_used : bool;
  br_ghost : bool;
  mutable br_vars_empty : fv;
  br_pat : tpat;
  br_body : texpr;
}

let rec is_value e =
  match e.exp_descr with
  | Forget (e, _)
  | Check (_, e, _) ->
      is_value e
  | Var _
  | ExtVar _ ->
      true
  | Apply _ -> false
  | Abstraction _ -> true
  | Cst _ -> true
  | Pair (e1, e2)
  | Xml (e1, e2, _) ->
      true
  | RecordLitt fields ->
      List.for_all (fun (_, e) -> is_value e) (Ident.LabelMap.get fields)
  | String (_, _, _, e) -> is_value e
  | Abstract _ -> true
  (* Data destructors *)
  | Match (e, brs)
  | Map (e, brs)
  | Transform (e, brs)
  | Xtrans (e, brs) ->
      is_value e && is_value_branches brs
  | Validate (e, _, _) -> is_value e
  | RemoveField (e, _) -> is_value e
  | Dot (e, _) -> is_value e
  (* Exception *)
  | Try (e, brs) ->
      is_value e && is_value_branches brs
      (* the "value" (3 :? Bool) may raise an exception. *)
  | Ref _ -> false (* should be seen as a function application. *)
  | External _ -> true
  | Op (_, _, el) -> List.for_all is_value el
  | NsTable (_, e) ->
      is_value
        { exp_loc = Cduce_loc.noloc; exp_typ = Types.empty; exp_descr = e }

and is_value_branches brs =
  List.for_all (fun br -> is_value br.br_body) brs.br_branches
