{
  open Parser

  let enter_newline lexbuf =
    Lexing.new_line lexbuf;
    lexbuf

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c
}

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*
let tagid = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*'('

let varid = '\''['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
let mvarid = '\''['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*

let int = ('+'|'-')? ['0'-'9']+ ('_'+ ['0'-'9']+)*

rule token = parse
| "type" { TYPE } | "define" { DEFINE } | "where" { WHERE } | "and" { AND }
| int as i { INT (Z.of_string i) }
| '"'      { read_string (Buffer.create 17) lexbuf }
| id as s  { ID s }
| tagid as s  { TAGID (String.sub s 0 (String.length s - 1)) }
| varid as s  { VARID s }
| mvarid as s  { MVARID s }
| newline  { Lexing.new_line lexbuf ; token lexbuf }
| blank    { token lexbuf }
| ";;" { BREAK } | ',' { COMMA } | ':' { COLON } | ';' { SEMICOLON } | '=' { EQUAL }
| ".." { DPOINT } | ":?" { OCOLON }
| '(' { LPAREN } | ')' { RPAREN } | "{" { LBRACE } | "}" { RBRACE }
| "[" { LBRACKET } | "]" { RBRACKET }
| "<=" { LEQ } | ">=" { GEQ }
| '|' { TOR } | '&' { TAND } | '~' { TNEG } | '\\' { TDIFF } | "->" { TARROW }
| eof { EOF }
| _ { raise (Errors.E_Lexer ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| newline { enter_newline lexbuf |> read_string buf }
| '"' { STRING (Buffer.contents buf) }
| '\\' (backslash_escapes as c) { Buffer.add_char buf (char_for_backslash c); read_string buf lexbuf }
| [^ '"' '\\' '\010' '\013']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }
| _ { raise (Errors.E_Lexer ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (Errors.E_Lexer ("String is not terminated")) }
