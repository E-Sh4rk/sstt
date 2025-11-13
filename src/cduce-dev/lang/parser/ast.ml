(*  Abstract syntax as produced by the parser *)

open Cduce_loc
open Ident
module U = Encodings.Utf8

type ns_expr =
  [ `Uri of Ns.Uri.t
  | `Path of U.t list
  ]

type pprog = pmodule_item list
and pmodule_item = pmodule_item' located

and pmodule_item' =
  | TypeDecl of (Cduce_loc.loc * U.t * U.t list) * ppat
  | SchemaDecl of U.t * string
  | LetDecl of ppat * pexpr
  | FunDecl of pexpr
  | Namespace of U.t * ns_expr
  | KeepNs of bool
  | Using of U.t * U.t
  | Open of U.t list
  | EvalStatement of pexpr
  | Directive of toplevel_directive

and debug_directive =
  [ `Filter of ppat * ppat
  | `Sample of ppat
  | `Accept of ppat
  | `Compile of ppat * ppat list
  | `Subtype of ppat * ppat
  | `Single of ppat
  | `Tallying of U.t list * U.t list * (ppat * ppat) list
  ]

and toplevel_directive =
  [ `Quit
  | `Env
  | `Reinit_ns
  | `Help
  | `Dump of pexpr
  | `Print_type of ppat
  | `Debug of debug_directive
  | `Verbose
  | `Silent
  | `Builtins
  ]

and pexpr =
  | LocatedExpr of loc * pexpr
  (* CDuce is a Lambda-calculus ... *)
  | Var of U.t
  | Apply of pexpr * pexpr
  | Abstraction of abstr
  (* Data constructors *)
  | Const of Types.Const.t
  | Integer of Intervals.V.t
  | Char of CharSet.V.t
  | Pair of pexpr * pexpr
  | Atom of U.t
  | Xml of pexpr * pexpr
  | RecordLitt of (label * pexpr) list
  | String of U.uindex * U.uindex * U.t * pexpr
  | Abstract of AbstractSet.V.t
  (* Data destructors *)
  | Match of pexpr * branches
  | Map of pexpr * branches
  | Transform of pexpr * branches
  | Xtrans of pexpr * branches
  | Validate of pexpr * U.t list
  | Dot of pexpr * label
  | TyArgs of pexpr * ppat list
  | RemoveField of pexpr * label
  (* Exceptions *)
  | Try of pexpr * branches
  (* Other *)
  | NamespaceIn of U.t * ns_expr * pexpr
  | KeepNsIn of bool * pexpr
  | Forget of pexpr * ppat
  | Check of pexpr * ppat
  | Ref of pexpr * ppat
  (* CQL *)
  | SelectFW of pexpr * (ppat * pexpr) list * pexpr list

and label = U.t

and abstr = {
  fun_name : (Cduce_loc.loc * U.t) option;
  fun_poly : U.t list;
  fun_iface : (ppat * ppat) list;
  fun_body : branches;
}

and branches = (ppat * pexpr) list

(* A common syntactic class for patterns and types *)
and ppat = ppat' located

and ppat' =
  | Poly of U.t
  | PatVar of (U.t list * ppat list)
  | Cst of pexpr
  | NsT of U.t
  | Recurs of ppat * (Cduce_loc.loc * U.t * ppat) list
  | Internal of Types.descr
  | Or of ppat * ppat
  | And of ppat * ppat
  | Diff of ppat * ppat
  | Prod of ppat * ppat
  | XmlT of ppat * ppat
  | Arrow of ppat * ppat
  | Optional of ppat
  | Record of bool * (label * (ppat * ppat option)) list
  | Constant of U.t * pexpr
  | Regexp of regexp
  | Concat of ppat * ppat
  | Merge of ppat * ppat

and regexp =
  | Epsilon
  | Elem of ppat
  | Guard of ppat
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
  | WeakStar of regexp
  | SeqCapture of Cduce_loc.loc * U.t * regexp

let pat_true = mknoloc (Internal Builtin_defs.true_type)
let pat_false = mknoloc (Internal Builtin_defs.false_type)
let cst_true = Const (Types.Atom Builtin_defs.true_atom)
let cst_false = Const (Types.Atom Builtin_defs.false_atom)
let cst_nil = Const Types.Sequence.nil_cst
let pat_nil = mknoloc (Internal Types.Sequence.nil_type)
let if_then_else cond e1 e2 = Match (cond, [ (pat_true, e1); (pat_false, e2) ])
let logical_and e1 e2 = if_then_else e1 e2 cst_false
let logical_or e1 e2 = if_then_else e1 cst_true e2
let logical_not e = if_then_else e cst_false cst_true

let rec pat_fold f acc p =
  let nacc = f acc p in
  match p.descr with
  | Poly _
  | Cst _
  | NsT _
  | Internal _
  | Constant _ ->
      nacc
  | PatVar (_, pl) -> List.fold_left (fun acc pi -> pat_fold f acc pi) acc pl
  | Recurs (p0, pl) ->
      List.fold_left
        (fun acc (_, _, pi) -> pat_fold f acc pi)
        (pat_fold f nacc p0) pl
  | Or (p1, p2)
  | And (p1, p2)
  | Diff (p1, p2)
  | Prod (p1, p2)
  | XmlT (p1, p2)
  | Arrow (p1, p2)
  | Concat (p1, p2)
  | Merge (p1, p2) ->
      pat_fold f (pat_fold f nacc p1) p2
  | Optional p0 -> pat_fold f nacc p0
  | Record (_, pl) ->
      List.fold_left
        (fun acc (_, (p1, op2)) ->
          let acc1 = pat_fold f nacc p1 in
          match op2 with
          | None -> acc1
          | Some p2 -> pat_fold f acc1 p2)
        nacc pl
  | Regexp e ->
      re_fold
        (fun acc e ->
          match e with
          | Elem p -> pat_fold f acc p
          | _ -> acc)
        nacc e

and re_fold f acc e =
  let nacc = f acc e in
  match e with
  | Epsilon
  | Elem _
  | Guard _ ->
      nacc
  | Seq (e1, e2)
  | Alt (e1, e2) ->
      re_fold f (re_fold f nacc e1) e2
  | Star e0
  | WeakStar e0
  | SeqCapture (_, _, e0) ->
      re_fold f nacc e0

let pat_iter f p = pat_fold (fun () p -> f p) () p
let re_iter f e = re_fold (fun () e -> f e) () e
