%{

open Ast
open Ident

let parsing_error loc =
    Format.kasprintf (Cduce_error.raise_err_loc ~loc Cduce_error.Ast_Parsing_error)

let mk loc x = Cduce_loc.(mk_loc loc x)
let mknoloc x = Cduce_loc.mknoloc x
let lop p = (p,p)
let exp loc e = LocatedExpr (loc, e)
let noloc = Cduce_loc.noloc


let ident s =
  let b = Buffer.create (String.length s) in
  let rec aux i =
    if (i = String.length s) then Buffer.contents b
    else match s.[i] with
      | '\\' -> assert (s.[i+1] = '.'); Buffer.add_char b '.'; aux (i+2)
      | c -> Buffer.add_char b c; aux (i+1)
  in
  aux 0

let label s = U.mk (ident s)
let ident s = U.mk (ident s)

let rec multi_prod loc = function
  | [ x ] -> x
  | x :: l -> mk loc (Prod (x, multi_prod loc l))
  | [] -> assert false

let flatten_multi_prod p =
let rec loop p acc =
  match p.Cduce_loc.descr with
    Prod (p1, p2) -> loop p2 (p1::acc)
    | _ -> List.rev (p::acc)
    in
  loop p []


let rec tuple = function
  | [ x ] -> x
  | x :: l -> Pair (x, tuple l)
  | [] -> assert false

let char = mknoloc (Internal (Types.char CharSet.any))
let string_regexp = Star (Elem char)

let seq_of_string s =
  let open Encodings in
  let s = Utf8.mk s in
  let rec aux i j =
    if Utf8.equal_index i j then []
    else let (c,i) = Utf8.next s i in c :: (aux i j)
  in
  aux (Utf8.start_index s) (Utf8.end_index s)


let parse_char loc s =
  match seq_of_string s with
    | [ c ] -> c
    | _ -> parsing_error loc "invalid character litteral '%s'" s

let mk_rec_field loc lab def =
  let o, x, y =
    match def with
      None -> (false, mknoloc (PatVar ([ident lab],[])), None)
      | Some z -> z
  in
  let x = if o then mk loc (Optional x) else x in
  (label lab, (x, y))

let mk_interval loc it =
  let open Intervals in
  match it with
    None, Some j ->
      let j = V.mk j in
          mk loc (Internal (Types.interval (left j)))
    | Some i, None ->
     let i = V.mk i in
         mk loc (Internal (Types.interval (right i)))
    | Some i, Some j ->
     let i = V.mk i and j = V.mk j in
         mk loc (Internal (Types.interval (bounded i j)))
    | None, None -> parsing_error loc "invalid interval *--*"

let rec is_not = function
  Var id when U.to_string id = "not" -> true
  | LocatedExpr (_, e) -> is_not e
  | _ -> false

let apply_op2_noloc op e1 e2 = Apply (Apply (Var (ident op), e1), e2)
let apply_op2 loc op e1 e2 = exp loc (apply_op2_noloc op e1 e2)

let set_ref e1 e2 = Apply (Dot (e1, U.mk "set"), e2)
let get_ref e = Apply (Dot (e, U.mk "get"), cst_nil)
let let_in e1 p e2 =  Match (e1, [p,e2])
let seq e1 e2 = let_in e1 pat_nil e2
let concat e1 e2 = apply_op2_noloc "@" e1 e2

let id_dummy = U.mk "$$$"

let mk_app_pat loc var l =
  match var.Cduce_loc.descr with
    PatVar(p, []) -> mk loc (PatVar(p, l))
  | _ -> parsing_error loc "invalid parametric type"

let unregexp kind loc e =
  let rec loop e =
   match e with
      Elem x -> x
    | Seq(Elem t1, e2) ->
       let t2 = loop e2 in
       mk_app_pat loc t1 (flatten_multi_prod t2)
    | _ ->
     parsing_error loc "mixing regular expressions and %s is not allowed" kind
  in
  loop e

let parse_pat_list err p f =
  let rec loop r acc =
    match r with
      Elem { descr = e; _ } -> (match f e with Some x -> x :: acc | None -> err ())
    | Seq (r1, r2) -> loop r2 (loop r1 acc)
    | Epsilon -> acc
    | _ -> err ()
 in
  match p.Cduce_loc.descr with
    Regexp (r) -> loop r []
    | _ -> err ()
;;


let parse_poly_list loc p =
  let err () =
      parsing_error loc
      "debug tallying expects a list of variables as first argument"
  in
  let l = parse_pat_list err p (function (Poly e) -> Some e | _ -> None) in
  List.sort_uniq U.compare l
;;
let parse_pat_pair_list loc p =
let err () =
      parsing_error loc
      "debug tallying expects a finite list of pair of types as second argument"
in
  parse_pat_list err p
    (function Prod(p1, p2) -> Some (p1, p2) | _ -> None)
;;

%}
/* Keywords */
%token HASH_PRINT_TYPE "#print_type"
%token HASH_DUMP_VALUE "#dump_value"
%token HASH_ASCII HASH_LATIN1 HASH_UTF8
%token AND   "and"
%token DEBUG "debug"
%token DIV   "div"
%token ELSE  "else"
%token FROM  "from"
%token FUN   "fun"
%token IF    "if"
%token IN    "in"
%token INCLUDE "include"
%token LET     "let"
%token MAP     "map"
%token MATCH   "match"
%token MOD     "mod"
%token NAMESPACE "namespace"
%token OFF       "off"
%token ON        "on"
%token OPEN      "open"
%token OR        "or"
%token REF       "ref"
%token SCHEMA    "schema"
%token SELECT    "select"
%token THEN      "then"
%token TRANSFORM "transform"
%token TRY       "try"
%token TYPE      "type"
%token USING     "using"
%token VALIDATE  "validate"
%token WHERE     "where"
%token WITH      "with"
%token XTRANSFORM "xtransform"


/* Opertors */
%token COLEQ ":="
%token MINUSGT  "->"
%token EQ "=" LTEQ "<=" LTLT "<<" GTGT ">>" GTEQ ">=" BANGEQ "!="
%token PLUS "+" MINUS "-" AT "@"
%token BARBAR "||" BAR "|"
%token SETMINUS
%token STAR "*"
%token AMPAMP "&&" AMP "&"
%token STARSTAR "**"
%token SLASH "/"
%token SLASHAT "/@"
%token SLASHSLASH "//"
%token DOT "."
%token BQUOTE "`"
%token BANG "!"
%token COLCOL "::"
%token DOTDOT ".."
%token MINUSMINUS "--"
%token STARMINUSMINUS "*--"
%token MINUSMINUSSTAR "--*"
%token QMARKQMARK "??" PLUSQMARK "+?" STARQMARK "*?"
%token EQQMARK "=?"
%token UNDERSCORE "_"

/* Separators */
%token LP "(" RP ")" LSB "[" RSB "]" LT "<" GT ">" LCB "{" RCB "}"
%token COLON ":" SEMI ";" SEMISEMI ";;" COMMA "," QMARK "?"

/* Terminals */
%token <string> IDENT
%token <string> ANY_IN_NS
%token <string> STRING1
%token <string> STRING2
%token <string> INT
%token <float> FLOAT
%token <string> POLY
%token <string> HASH_DIRECTIVE
%token <Ast.pprog> RESOLVED_INCLUDE
%token EOI

/* Priorities */
%nonassoc "in"
%nonassoc "->"
%nonassoc "|"
%nonassoc below_SEMI
%nonassoc ";"
//%nonassoc "let" "namespace"
//%nonassoc ";;"
%right ":="
%nonassoc "ref"
%right "from" "where" "and"
%nonassoc "then"
%nonassoc "else"
%left "or" "||"
%left "&&"
%left "=" "<<" ">>" "<=" ">=" "!="
%left "+" "-" "@"
%left "*" "div" "mod"
%left SETMINUS "//" "/@" "/"
%nonassoc ":"
%nonassoc "!" unary_op
%left "::"
%left "."
%nonassoc ","

%start <Ast.pprog> prog
%start <Ast.pprog> top_phrases
%type <Ast.ppat> pat

%start <Ast.ppat> parse_pat
%start <Ast.pexpr> parse_expr
%start <Ast.pmodule_item> parse_pmodule_item
%%

/* Macros */

%inline loc(X):
x=X           { mk $sloc x }
;

%inline iloption(X):
              { [] }
| x = X       { [x] }
;

/*  Toplevel definitions    */
parse_pat:
p = pat EOI { p }
;
parse_expr:
e = expr EOI { e }
;

parse_pmodule_item:
pi = loc(prog_item_) EOI { pi }
;


top_phrases:
| e = multi_expr ";;" { [ mk $sloc (EvalStatement e) ] }
| p = list(prog_item) ";;" { List.concat p }
;

prog:
| e = opt_prog_expr l = prog_items* EOI { e @ List.concat l }
;

%inline opt_prog_expr:
         { [] }
| e = multi_expr         {
 [ mk $sloc (EvalStatement e) ]
   }
;

%inline prog_items:
";;" e = opt_prog_expr
| e = prog_item { e }
;

%inline prog_item:
| item = loc(prog_item_) { [item] }
| "include" items = RESOLVED_INCLUDE { items }
| HASH_ASCII | HASH_LATIN1 | HASH_UTF8
| "include" STRING2 { [] }

;

%inline prog_item_:
| l = let_binding {   let f, p, e = l in
                      if f then FunDecl e  else  LetDecl (p, e)
 }

| n = namespace_binding {
match n with
    | `Prefix (name,ns) ->  Namespace (name, ns)
    | `Keep b ->  KeepNs b
}

| "type" x = ident_or_keyword params = type_params "=" t = pat {
                let id = $loc(x), ident x, params in TypeDecl (id, t)  }

| "using" name = IDENT "=" cu = ident_or_string2 {
                       Using (U.mk name, U.mk cu)
                    }

| "open" ids = separated_nonempty_list(".", ident_or_keyword) {
 Open (List.map ident ids)
 }
| "schema" name = IDENT "=" uri = STRING2 {
    SchemaDecl (U.mk name, uri)
 }
| "debug" d = IDENT "(" l = nonempty_list (var_pat) ")" {
  let tallying ord delta plist loc =
     let ord = parse_poly_list loc ord in
     let delta = parse_poly_list loc delta in
     let plist = parse_pat_pair_list loc plist in
     `Tallying (ord, delta, plist)
  in
  let dir = match d, l with
  "filter", [t; p] -> `Filter(t, p)
  | "accept", [p] -> `Accept p
  | "compile", t ::( _ :: _ as p) -> `Compile (t, p)
  | "sample", [t] -> `Sample t
  | "subtype", [t1; t2] -> `Subtype (t1, t2)
  | "single", [t] -> `Single t
  | "tallying", [delta; plist] -> tallying (mk $sloc (Regexp Epsilon)) delta plist $sloc
  | "tallying", [ord; delta; plist] -> tallying ord delta plist $sloc
  | _ -> parsing_error $loc(d) "invalid debug directive %s" d 
  in Directive (`Debug dir)
 }
| d = HASH_DIRECTIVE {
  let dir = match d with
    "#verbose" -> `Verbose
    | "#slient" -> `Silent
    | "#quit" -> `Quit
    | "#env" -> `Env
    | "#reinit_ns" -> `Reinit_ns
    | "#help" -> `Help
    | "#builtins" -> `Builtins
    | _ -> parsing_error $loc(d) "invalid toplevel directive %s" d
    in Directive dir
}
| HASH_PRINT_TYPE t = arrow_pat { Directive(`Print_type t) }
| HASH_DUMP_VALUE e = expr { Directive (`Dump e) }
;

%inline ident_or_string2:
| s = IDENT { s }
| s = STRING2 { s }
;

%inline type_params:
|            { [] }
| "(" l = separated_nonempty_list(",", poly_var) ")" {
  let seen = ref [] in
  List.map (fun s ->
    if List.mem s !seen then
    parsing_error $loc(l) "duplicate type parameter '%s" s;
    seen := s :: !seen;
  U.mk s) l
};

%inline poly_var:
v = POLY {
  if v <> "" && v.[0] = '_' then
  parsing_error $sloc "Type variable names starting with _ are reserved for weak
  polymorphic variables, they cannot appear in programs.";
  v
};


pat:
| x = arrow_pat "where" l = and_pat_list { mk $sloc (Recurs(x, List.rev l)) }
| x = arrow_pat { x }
;

and_pat_list:
| id = located_ident "=" p = pat { [ (fst id, snd id, p) ] }
| l = and_pat_list "and" id = located_ident "=" p = pat
        { (fst id, snd id, p)::l }
;

arrow_pat:
| x = or_pat "->" y = arrow_pat  { mk $sloc (Arrow(x, y)) }
| x = or_pat "@" y = arrow_pat { mk $sloc (Concat(x, y)) }
| x = or_pat "+" y = arrow_pat { mk $sloc (Merge(x, y)) }
| x = or_pat { x }
;

or_pat:
| x = or_pat "|" y = and_pat { mk $sloc (Or(x, y)) }
| x = and_pat { x }
;

and_pat:
| x = and_pat "&" y = app_pat { mk $sloc (And(x, y)) }
| x = and_pat SETMINUS y = app_pat { mk $sloc (Diff(x, y)) }
| x = app_pat { x }
;

app_pat:
| x = var_pat "(" l = separated_nonempty_list(",", pat) ")" {
  mk_app_pat $sloc x l
 }
| x = var_pat { x }

var_pat:
| id = ident_or_keyword_no_ref_no_where ids = e_list
| id = ident_or_keyword_no_ref_no_where "." ids = separated_nonempty_list(".", ident_or_keyword)
      { let iids = List.map ident (id::ids) in
        mk $sloc (PatVar (iids,[]))
      }
| x = constr_pat { x }
;
%inline e_list:
| { [] }
;


constr_pat:
| "(" a = IDENT ":=" c = expr ")" { mk $sloc (Constant (ident a,c))}
| "(" l = separated_nonempty_list (",", pat) ")" {multi_prod $sloc l}
| i = char { let i = CharSet.V.mk_int i in
    mk $sloc (Internal (Types.char (CharSet.mk_classes [i, i]))) }
| i = char "--" j = char {
  let i = CharSet.V.mk_int i in
  let j = CharSet.V.mk_int j in
   mk $sloc (Internal (Types.char (CharSet.mk_classes [i, j]))) }
| it = interval {
  mk_interval $sloc it
 }
| i = int_pat { mk_interval $sloc (Some i, Some i) }
| s = simple_pat { s }
;

simple_pat:
| p = poly_var             { mk $sloc (Poly (U.mk p)) }
| "{" r = record_spec "}" { r }
| "ref" p = constr_pat {
    let get_fun = mk $sloc (Arrow (pat_nil, p))
    and set_fun = mk $sloc (Arrow (p, pat_nil)) in
    let fields =
      [ label "get", (get_fun, None);
        label "set", (set_fun, None) ]
    in
      mk $sloc (Record (false, fields))
 }
| "!" a = IDENT {	mk $sloc (Internal Types.(abstract (AbstractSet.atom a)))}
| "`" t = tag_type { t }
| "[" r = regexp? q = option(";" q = pat { q }) "]" {
      let r = match r with None -> Epsilon | Some r -> r in
      let r =
        match q with
        Some q -> let any = mk $sloc (Internal Types.any) in
                  Seq (r, Seq(Guard q, Star (Elem any)))
      | None -> r
      in mk $sloc (Regexp r)
 }
| "<" t = tag_type_or_pat a = attrib_spec">" c = var_pat {
      mk $sloc (XmlT (t, multi_prod $sloc [a;c]))
 }
| "_" {  mk $sloc (Internal Types.any) }
| s = STRING2 {
    let s = List.map
      (fun c ->
        mknoloc (Internal (Types.char CharSet.(atom (V.mk_int c)))
        )) (seq_of_string s)
    in
    let s = s @ [ mknoloc (Internal (Types.Sequence.nil_type))]
    in
    multi_prod $sloc s
 }
;

located_ident:
| i = IDENT { ($sloc ,ident i) }
;

char:
| c = STRING1  { parse_char $sloc c }
;

%inline keyword_no_else_no_ref_no_where:
| "and" { "and" }
| "debug" { "debug" }
| "div" { "div" }
| "from" { "from" }
| "fun" { "fun" }
| "if" { "if" }
| "in" { "in" }
| "include" { "include" }
| "let" { "let" }
| "map" { "map" }
| "match" { "match" }
| "mod" { "mod" }
| "namespace" { "namespace" }
| "off" { "off" }
| "on" { "on" }
| "open" { "open" }
| "or" { "or" }
| "schema" { "schema" }
| "select" { "select" }
| "then" { "then" }
| "transform" { "transform" }
| "try" { "try" }
| "type" { "type" }
| "using" { "using" }
| "validate" { "validate" }
| "with" { "with" }
| "xtransform" { "xtransform" }
;

keyword:
k = keyword_no_else_no_ref_no_where { k }
| "ref" { "ref" }
| "else" { "else" }
| "where" { "where" }
;

ident_or_keyword_no_else:
| s = keyword_no_else_no_ref_no_where { s }
| id = IDENT { id }
| "ref"  { "ref" }
| "where" { "where" }
;

ident_or_keyword_no_ref_no_where:
| s =  keyword_no_else_no_ref_no_where { s }
| id = IDENT { id }
| "else"  { "else" }
;

ident_or_keyword:
| s = IDENT { s }
| k = keyword { k }
;

%inline int_pat:
| i = INT  { i }
| "-" i = INT { "-" ^ i }
;

%inline interval:
| i = int_pat "--" j = int_pat { Some i, Some j }
| "*--" j = int_pat { None, Some j}
| i = int_pat "--*"      { Some i, None }
;

tag_type:
l = loc (tag_type_) { l }
;

%inline tag_type_:
| "_"  { Internal (Types.atom (AtomSet.any)) }
| a = ident_or_keyword { Cst (Atom (ident a)) }
| t = ANY_IN_NS { NsT (ident t) }
;

tag_type_or_pat:
| t = tag_type { t }
| "(" p = pat ")" { p }
;

attrib_spec:
| r = record_spec { r }
| "(" t = pat ")" { t }
;

record_spec:
| r = record_spec_fields op = boption("..")
        { mk $sloc (Record(op, r)) }
;

record_spec_fields:
| "else" f = option(field_pat) ";"? l=other_rec_spec {
    (mk_rec_field $sloc "else" f):: l
 }
| l = other_rec_spec { l }
;
other_rec_spec:
|               { [ ] }
| lab = ident_or_keyword_no_else f = option(field_pat) ";"? fields = other_rec_spec
{
  (mk_rec_field $sloc lab f) :: fields
}
;

%inline field_eq:
 "="  { false }
|"=?" { true }
;

field_pat:
| e = field_eq x = arrow_pat  { (e, x, None) }
| e = field_eq x = arrow_pat "else" y = arrow_pat { (e, x, Some y) }
;

regexp:
| x = regexp_or "->" y = regexp {
    let tx = unregexp "arrow" $loc(x) x in
    let ty = unregexp "arrow" $loc(y) y in
    Elem (mk $sloc (Arrow (tx, ty)))
 }
| r = regexp_or { r }
;

regexp_or:
| x = regexp_or "|" y = regexp_and {
      match x, y with
    |Elem x, Elem y -> Elem (mk $sloc (Or (x, y)))
    | _ -> Alt (x, y)
}
| r = regexp_and { r }
;

regexp_and:
| x = regexp_and "&" y = regexp_concat {
    let tx = unregexp "intersection" $loc(x) x in
    let ty = unregexp "intersection" $loc(y) y in
    Elem (mk $sloc (And (tx, ty)))
}

 | x = regexp_and SETMINUS y = regexp_concat {
    let tx = unregexp "difference" $loc(x) x in
    let ty = unregexp "difference" $loc(y) y in
    Elem (mk $sloc (Diff (tx, ty)))
 }

| r = regexp_concat { r }
;

regexp_concat:
| x = regexp_concat y = regexp_acc { Seq (x, y) }
| r = regexp_acc { r }
;

regexp_acc:
| a = IDENT "::" x = regexp_simple {
  SeqCapture ($sloc, ident a, x)
 }
| x = regexp_simple { x }
;

regexp_simple:
| x = regexp_simple "*" { Star x }
| x = regexp_simple "*?" { WeakStar x }
| x = regexp_simple "+" { Seq (x, Star x) }
| x = regexp_simple "+?" { Seq (x, WeakStar x) }
| x = regexp_simple "?" { Alt (x, Epsilon) }
| x = regexp_simple "??" { Alt (Epsilon, x) }
| x = regexp_simple "**" i = INT {
  let rec aux i accu =
	  if (i = 0) then accu else aux (pred i) (Seq (x, accu))
	in
	let i =
	  try
	    let i = int_of_string i in
	    if (i > 1024) then raise Exit else i
    with Failure _ | Exit -> parsing_error $loc(i) "repetition number too large"
  in
  if i <= 0 then parsing_error $sloc "repetition number must be a positive integer";
  aux i Epsilon
 }
| "(" x = separated_nonempty_list(",", regexp) ")" {
    match x with
      [ x ] -> x
      | _ -> let x = List.map (unregexp "product" $sloc) x in
             Elem (multi_prod $sloc x)
 }
| "(" a = IDENT ":=" c = expr ")" { Elem (mk $sloc (Constant (ident a, c))) }
| "/" p = var_pat { Guard p }
| i = char "--" j = char {
  let open CharSet in
  let i = V.mk_int i
	and j = V.mk_int j in
   Elem (mk $sloc (Internal (Types.char (char_class i j))))
 }
| s = STRING1 {
  match seq_of_string s with
  [ c ] -> let c = CharSet.V.mk_int c in
  Elem (mk $sloc (Internal (Types.char (CharSet.mk_classes [c, c]))))
  | l ->
    List.fold_right
      (fun c accu ->
        let c = CharSet.V.mk_int c in
        let c = CharSet.atom c in
        Seq (Elem (mknoloc (Internal (Types.char c))), accu))
      l
  	  Epsilon
 }
| it = interval {
      Elem (mk_interval $sloc it)
 }
| i = int_pat { Elem (mk_interval $sloc (Some i, Some i)) }
| id = ident_or_keyword_no_ref_no_where {
  match id with
    "PCDATA" -> string_regexp
  | _ -> Elem (mk $sloc (PatVar ([ident id],[])))
 }
| id = ident_or_keyword_no_ref_no_where "." ids = separated_nonempty_list(".", ident_or_keyword) {
  let iids = List.map ident (id::ids) in
        Elem (mk $sloc (PatVar (iids,[])))
 }
| p = simple_pat { Elem p }
;


namespace_binding:
  "namespace" uri = STRING2 {`Prefix(U.mk "", `Uri (Ns.Uri.mk (ident uri))) }
| "namespace" name = ident_or_keyword rem = namespace_binding_rem {
  match name, rem with
  | _, `Idents ids ->
  let ids = List.map (fun x -> ident x) (name :: ids) in
   `Prefix(U.mk "", `Path ids)
  | _, (`Uri _ as uri) ->  `Prefix(ident name, uri)
  | _, (`Path _ as path) ->`Prefix(ident name, path)
  | "on", `Empty -> `Keep true
  | "off", `Empty -> `Keep false
  | _ -> parsing_error $sloc "invalid namespace specification"
 }
;

namespace_binding_rem:
"." idents = separated_nonempty_list(".", ident_or_keyword) { `Idents idents  }
| "=" uri = STRING2 { `Uri (Ns.Uri.mk (ident uri))  }
| "=" ids = separated_nonempty_list(".", ident_or_keyword) {
    let ids = List.map (fun x -> ident x) ids in
    `Path ids
  }
|  {`Empty}
;

%inline let_binding:
| "let" "fun" f = located_ident poly = poly_list "(" fd = fun_decl_after_lpar
| "let" f = located_ident poly = poly_list "(" fd = fun_decl_after_lpar {
    let p = mk $sloc (PatVar ([ snd f ],[])) in
    let fun_iface, fun_body = fd in
    let abst = { fun_name = Some f; fun_poly = poly; fun_iface; fun_body  } in
    let e = exp $sloc (Abstraction abst) in
    (true, p, e)
 }
| "let" p = ident_or_let_pat "=" e = multi_expr { (false, p, e) }
| "let" p = ident_or_let_pat ":" check=boption("?") t = pat "=" e = multi_expr {
  (false, p, if check then Check(e, t) else Forget (e, t))
 }
;

%inline poly_list:
|         { [] }
| l = nonempty_list(poly_var) "." { List.map U.mk l }
;

%inline fun_decl_after_lpar:
  x = or_pat "->" y = separated_nonempty_list ("->", or_pat)
  other_arrows =
  list (";" p1 = or_pat  "->" p2 = separated_nonempty_list("->", or_pat) {(p1,p2)})
  ")"
  b = branches {
    let pre_intf = (x, y) :: other_arrows in
    let intf = List.map (fun (x, y) ->
      (x, let y = List.rev y in List.fold_left (fun acc e ->
      let loc = Cduce_loc.(merge_loc e.loc acc.loc) in
      Cduce_loc.mk_loc loc (Arrow (e,acc))) (List.hd y)(List.tl y))
    ) pre_intf
    in
      (intf, b)
   }
| x = or_pat ":" t = pat args = loption(p = pair(",",
                      separated_nonempty_list(",", x = pat ":" t = pat { (x, t)})) { snd p})
  ")"
  others = list(delimited("(",separated_nonempty_list(",", separated_pair(pat,":", pat)) ,")"))
 ":" tres = pat "=" body = multi_expr {
   let mkfun args =
	       multi_prod Cduce_loc.noloc (List.map snd args),
	       multi_prod Cduce_loc.noloc (List.map fst args)
	 in
	 let _, tres, body = List.fold_right
		 (fun args (i, tres, body) ->
		    let (targ,arg) = mkfun args in
        let name = ($sloc, label ("anonymous_" ^ (string_of_int i))) in
		    let e = Abstraction
			      { fun_name = Some name; fun_poly = []; fun_iface = [targ,tres];
				    fun_body = [arg,body] }
        in
		    let t = mknoloc (Arrow (targ,tres)) in
		    (i+1, t,exp $sloc e)
		 )
		 others (0, tres, body)
    in
	  let (targ,arg) = mkfun ((x,t) :: args) in
	  [(targ,tres)],[(arg,body)]

  }

;

ident_or_let_pat:

| p1 = ident_or_let_pat "&" p2 = ident_or_let_pat_constr { mk $sloc (And(p1, p2)) }
| p = ident_or_let_pat_constr { p }
;

ident_or_let_pat_constr:
| id = located_ident { mk $sloc (PatVar ([ (snd id) ],[])) }
| p = constr_pat { p }

;

branches_:
| b = branch {  [ b ] }
| b = branch "|" bl = branches_  { b :: bl }
;

%inline branches:
"|"? b = branches_ { b }
;

%inline branch:
 p = or_pat "->" e = multi_expr { (p, e) }
;

multi_expr:
e = expr %prec below_SEMI { e }
| e1 = expr SEMI e2 = multi_expr { exp $sloc (seq e1 e2) }
;

expr:
| "match" e = multi_expr "with" b = branches { exp $sloc (Match (e, b)) }
| "try" e = multi_expr "with" b = branches { exp $sloc (Try (e, b)) }
| "map" e = multi_expr "with" b = branches { exp $sloc (Map (e, b)) }
| "transform" e = multi_expr "with" b = branches { exp $sloc (Transform (e, b)) }
| "xtransform" e = multi_expr "with" b = branches { exp $sloc (Xtrans (e, b)) }
| "validate" e = multi_expr "with" r = schema_ref {
  exp $sloc (Validate (e, [fst r; snd r]))
 }
| "select" e = expr "from" l = from_list { exp $sloc (SelectFW (e, l, [])) }
| "select" e = expr "from" l = from_list w = where_condition { exp $sloc (SelectFW (e, l, w)) }
| "fun" f = located_ident? poly = poly_list "(" fd = fun_decl_after_lpar {
   let fun_iface, fun_body = fd in
    let abst = { fun_name = f; fun_poly = poly; fun_iface; fun_body  } in
    exp $sloc (Abstraction abst)
  }
| "if" e = expr "then" e1 = expr "else" e2 = expr {
    exp $sloc (if_then_else e e1 e2)
  }
| "if" e = expr "then" e1 = expr {
    exp $sloc (if_then_else e e1 cst_nil)
  }
| l = let_binding "in" e2 = multi_expr {
  let _, p, e1 = l in exp $sloc (let_in e1 p e2)
 }
| n = namespace_binding "in" e2 = multi_expr {
  match n with
  `Prefix (name, ns) ->
    exp $sloc (NamespaceIn (name, ns, e2))
  | `Keep f -> exp $sloc (KeepNsIn (f, e2))
 }
| e = expr ":" check=boption("?") p = var_pat {
  exp $sloc (if check then Check(e, p) else Forget (e, p))
 }
(*| e1 = expr ";" e2 = expr { exp $sloc (seq e1 e2)}*)
| "ref" p = var_pat e = expr { exp $sloc (Ref (e, p))}
| e1 = expr ":=" e2 = expr { exp $sloc (set_ref e1 e2) }
| e1 = expr op = binop e2 = expr { match op with
  | "||" -> exp $sloc (logical_or e1 e2)
  | "&&" -> exp $sloc (logical_and e1 e2)
  |  _ -> apply_op2 $sloc op e1 e2
 }
| e = expr SETMINUS l = ident_or_keyword { exp $sloc (RemoveField(e, label l)) }
| e = expr "/" p = var_pat {
  let tag = mk $sloc (Internal Types.Atom.any) in
	let att = mk $sloc (Internal Types.Rec.any) in
	let any = mk $sloc (Internal Types.any) in
	let re = Star(Alt(SeqCapture(noloc,id_dummy,Elem p), Elem any)) in
	let ct = mk $sloc (Regexp re) in
        let p = mk $sloc (XmlT (tag, multi_prod $sloc [att;ct])) in
	exp $sloc (Transform (e,[p, Var id_dummy]))
 }
| e = expr "/@" a = ident_or_keyword {
  let tag = mk $sloc (Internal (Types.atom AtomSet.any)) in
  let any = mk $sloc (Internal Types.any) in
  let att = mk $sloc (Record
			    (true, [(label a,
				     (mk $sloc (PatVar ([id_dummy],[])),
				      None))])) in
  let p = mk $sloc (XmlT (tag, multi_prod $sloc [att;any])) in
  let t = (p, Pair (Var id_dummy,cst_nil)) in
      exp $sloc (Transform (e,[t]))

 }
| e = expr "//" p = var_pat {
  let stk = U.mk "$stack" in
  let y = U.mk "y" in
  let x = U.mk "x" in
  let f = U.mk "f" in
  let assign =
    set_ref
      (Var stk)
      (concat (get_ref (Var stk)) (Pair (Var id_dummy,cst_nil))) in
  let tag = mknoloc (Internal (Types.atom (AtomSet.any))) in
  let att = mknoloc (Internal Types.Rec.any) in
  let any = mknoloc (Internal Types.any) in
  let re = (SeqCapture(noloc,y,Star(Elem(any)))) in
  let ct = mknoloc (Regexp re) in
  let children = mknoloc (XmlT (tag, multi_prod $sloc [att;ct])) in
  let capt = mknoloc (And (mknoloc (And (mknoloc (PatVar ([id_dummy],[])),p)),children)) in
  let assign = seq assign ( (Apply(Var(f) , Var(y) ) ) ) in
  let xt = Xtrans ((Var x),[capt,assign]) in
  let rf = Ref (cst_nil, mknoloc (Regexp (Star(Elem p)))) in
  let targ = mknoloc (Regexp(Star(Elem(any)))) in
  let tres = targ in
  let arg = mknoloc(PatVar ([x],[])) in
  let abst = {fun_name = Some ($sloc,ident "f") ;
              fun_poly=[];
              fun_iface = [(targ, tres)];
              fun_body = [(arg,xt)] } in
  let body =
    let_in rf (mknoloc (PatVar ([stk],[])))
    (let_in ((Abstraction abst)) (mknoloc (PatVar([ident "f"],[])))
            (let_in ((Apply(Var(f) , e) )  ) (mknoloc (Internal Types.any)) (get_ref (Var stk))))
  in
	exp $sloc body
 }
| "-" e = expr %prec unary_op {
    apply_op2 $sloc "-" (Integer (Intervals.V.mk "0")) e
 }
| e = app_expr { e } /* includes not */
;


%inline binop:
| "=" { "=" }
| "<=" { "<=" }
| ">=" { ">=" }
| "!=" { "!=" }
| "+" { "+" }
| "-" { "-" }
| "*" { "*" }
| "@" { "@" }
| "||" { "||" }
| "or" { "||" }
| "&&" { "&&" }
/* The four operators below use a different internal name */
| "div" { "/" }
| "mod" { "%" }
| "<<" { "<" }
| ">>" { ">" }

;

schema_ref:
s = IDENT "." typ = ident_or_keyword { (U.mk s, ident typ) }
;

from_list:
p = arrow_pat "in" e = expr { [ (p, e) ] }
| l = from_list "," p = pat "in" e = expr { (p, e) :: l }
;

where_condition:
| "where" l = and_expr_list { List.rev l }
;
and_expr_list:
| e = expr %prec AND { [ e ] }
| l = and_expr_list "and" e = expr { e :: l }
;

app_expr:
| e1=app_expr e2=no_seq_expr {
  if is_not e1 then exp $sloc (logical_not e2)
  else exp $sloc (Apply (e1, e2))
  }
| e = no_seq_expr { e }
;

no_seq_expr:
| "!" e = no_seq_expr { exp $sloc (get_ref e) }
| c = char { exp $sloc (Char (CharSet.V.mk_int c)) }
| e = simple_expr %prec BANG { e }
;

simple_expr:
| e = simple_expr "::" "{" l = nonempty_list(var_pat) "}" { exp $sloc (TyArgs(e, l)) }
| e = simple_expr "." l = ident_or_keyword { exp $sloc (Dot (e, label l)) }
| "(" e = expr  l = with_annot ")" { exp $sloc (TyArgs (e, l)) }
| "(" l = separated_nonempty_list(",", multi_expr) ")" { exp $sloc (tuple l) }
| "[" l = list(seq_elem) tl = option (";" e = expr { e })
   _le = "]" {
  let loc_end = $loc(_le) in
  let e = match tl with Some e -> e | None -> cst_nil in
  let e = exp loc_end e in
  let loc_end = snd loc_end in
  let l = List.fold_right (fun x q ->
    match x with
      `String (loc, i, j, s) -> exp loc (String (i, j, s, q))
      |`Elems ((loc_s, _), x) -> exp (loc_s, loc_end) (Pair (x, q))
      |`Explode x -> concat x q
    ) l e
  in
  exp $sloc l

 }
| "<" te = tag_expr ae = attrib_expr ">" e = no_seq_expr {
    exp $sloc (Xml (te, Pair(ae, e)))
 }
| "{" r = expr_record_spec "}" { r }
| s = STRING2 {
    let s = U.mk s in
	  exp $sloc (String (U.start_index s,
                       U.end_index s, s, cst_nil))
}
| v = IDENT { exp $sloc (Var (ident v)) }
| "`" t = ident_or_keyword { exp $sloc (Atom (ident t)) }
| i = INT { exp $sloc (Integer (Intervals.V.mk i)) }
| f = FLOAT { exp $sloc (Abstract (("float", Obj.repr f))) }
;

with_annot:
| "with" "{" l= nonempty_list(var_pat) "}" { l }
;


seq_elem:
| s = STRING1 {
  let s = U.mk s in
	`String ($sloc, U.start_index s, U.end_index s, s) }
| e = simple_expr { `Elems ($sloc, e) }
| "!" e = simple_expr { `Explode e }
;
tag_expr:
| tag = ident_or_keyword { exp $sloc (Atom (ident tag)) }
| "(" e = expr ")" { exp $sloc e }
;
attrib_expr:
| e = expr_record_spec { e }
| "(" e = expr ")" { exp $sloc e }
;
expr_record_spec:
| fields = list (l = ident_or_keyword e = option ("=" e = no_seq_expr {e} )";"?{
    label l, (
      match e with
        Some e -> e
      | None -> Var (ident l))

  }) {
  exp $sloc (RecordLitt fields)
   }
;



%%
