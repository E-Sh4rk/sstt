%{
open Ast

let parse_atom_or_builtin str =
    match str with
    | "empty" -> TBuiltin TEmpty
    | "any" -> TBuiltin TAny
    | "tuple" -> TBuiltin TAnyTuple
    | "arrow" -> TBuiltin TAnyArrow
    | "record" -> TBuiltin TAnyRecord
    | "enum" -> TBuiltin TAnyEnum
    | "tag" -> TBuiltin TAnyTag
    | "int" -> TBuiltin TAnyInt
    | str ->
      let regexp = Str.regexp {|^tuple\([0-9]*\)$|} in
      if Str.string_match regexp str 0 then
        let nb = Str.matched_group 1 str in
        TBuiltin (TAnyTupleComp (int_of_string nb))
      else
        TNamed str
%}

%token<string> STRING
%token<Z.t> INT
%token<string> ID, TAGID, VARID, MVARID
%token DEFINE TYPE WHERE AND
%token BREAK COMMA EQUAL COLON SEMICOLON
%token DPOINT OCOLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token LEQ GEQ
%token TOR TAND TNEG TDIFF TARROW
%token EOF

%start<program> program
%start<ty> ty_main
%start<command> command

%right TARROW
%left TOR
%left TAND
%left TDIFF
%nonassoc TNEG

%%

program:
| p=elt* EOF { p }

command:
| elt=elt { Elt elt }
| EOF { End }

elt:
| TYPE ids=separated_nonempty_list(SEMICOLON, ID) EQUAL e=expr_nocmp BREAK
  { DefineAlias (ids, e) }
| DEFINE defs=separated_nonempty_list(SEMICOLON, def) BREAK { Define defs }
| str=STRING? e=expr BREAK { Expr (str, e) }

def:
| id=ID { DAtom id }
| id=TAGID p=prop RPAREN { DTag (id, p) }

%inline prop:
| EQUAL { PId } | LEQ { PMono } | TAND { PAnd } | TAND TAND { PAndEx }
| TOR { POr } | TOR TOR { POrEx } | { PNone }

expr:
| e=expr_nocmp { e }
| e1=expr_nocmp op=op e2=expr_nocmp { CCmp (e1, op, e2) }

expr_nocmp:
| e=simpl_expr { e }
| e1=expr_nocmp SEMICOLON e2=simpl_expr { CCat (e1, e2) }
| e1=expr_nocmp e2=simpl_expr { CApp (e1, e2) }

simpl_expr:
| s=tsubst { CSubst s }
| t=tally { CTally t }
| ty=ty { CTy ty }
| LBRACKET e=expr_nocmp RBRACKET { e }

op:
| LEQ { LEQ } | EQUAL { EQ } | GEQ { GEQ }

tsubst:
| LBRACKET bindings=separated_list(SEMICOLON, subst_binding) RBRACKET { bindings }

%inline subst_binding:
| v=VARID COLON ty=ty { (false, v, ty) }
| v=MVARID COLON ty=ty { (true, v, ty) }

tally:
| LBRACKET cs=separated_nonempty_list(SEMICOLON, tally_binding) RBRACKET { cs }

%inline tally_binding:
| ty1=ty op=op ty2=ty { (ty1, op, ty2) }

ty_main:
| ty=ty EOF { ty }

ty:
| ty=ty_norec { ty }
| ty=ty_norec WHERE defs=separated_nonempty_list(AND, ty_def) { TWhere (ty, defs) }

%inline ty_def: name=ID EQUAL ty=ty_norec { (name, ty) }

ty_norec:
| ty=simple_ty { ty }
| hd=simple_ty COMMA tl=separated_nonempty_list(COMMA, simple_ty) { TVarop (TTuple, hd::tl) }

simple_ty:
| ty=atomic_ty { ty }
| ty1=simple_ty TOR ty2=simple_ty { TBinop (TCup, ty1, ty2) }
| ty1=simple_ty TDIFF ty2=simple_ty { TBinop (TDiff, ty1, ty2) }
| ty1=simple_ty TAND ty2=simple_ty { TBinop (TCap, ty1, ty2) }
| TNEG ty=simple_ty { TUnop (TNeg, ty) }
| ty1=simple_ty TARROW ty2=simple_ty { TBinop (TArrow, ty1, ty2) }

atomic_ty:
| id=ID { parse_atom_or_builtin id }
| id=TAGID ty=ty RPAREN { TTag (id, Some ty) }
| id=TAGID RPAREN { TTag (id, None) }
| id=VARID { TVar id }
| id=MVARID { TVarMono id }
| i=INT { TInterval (Some i, Some i) }
| LPAREN i1=INT? DPOINT i2=INT? RPAREN { TInterval (i1,i2) }
| LBRACE bindings=separated_list(SEMICOLON, record_field) t=tail RBRACE { TRecord (bindings, t) }
| LPAREN ty=ty RPAREN { ty }
| LPAREN RPAREN { TBuiltin (TAnyTupleComp 0) }

%inline tail:
| DPOINT { TOpen }
| id=VARID { TRowVar id }
| { TClosed }

record_field:
| l=ID COLON ty=ty { (l, ty, false) }
| l=ID OCOLON ty=ty { (l, ty, true) }
