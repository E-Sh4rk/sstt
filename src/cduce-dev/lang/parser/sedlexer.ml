open Parser
module L = Sedlexing

let error lexbuf s =
  let loc = Sedlexing.lexing_positions lexbuf in
  Cduce_error.(raise_err_loc ~loc Sedlexer_Error s)

let warning lexbuf msg =
  Cduce_loc.warning (L.lexing_positions lexbuf) msg

(* Buffer for string literals *)
let string_buff = Buffer.create 1024

let store_lexeme lexbuf =
  let s = L.lexeme lexbuf in
  for i = 0 to Array.length s - 1 do
    Encodings.Utf8.store string_buff (Uchar.to_int s.(i))
  done

let store_ascii = Buffer.add_char string_buff
let store_code = Encodings.Utf8.store string_buff
let clear_buff () = Buffer.clear string_buff

let get_stored_string () =
  let s = Buffer.contents string_buff in
  clear_buff ();
  Buffer.clear string_buff;
  s

(* Parse characters literals \123; \x123; *)

let hexa_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let parse_char lexbuf base i =
  let s = L.Utf8.sub_lexeme lexbuf i (L.lexeme_length lexbuf - i - 1) in
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    let c = hexa_digit s.[i] in
    if c >= base || c < 0 then
      error lexbuf "invalid digit";
    r := (!r * base) + c
  done;
  !r

let ncname_char =
  [%sedlex.regexp?
      ( xml_letter | xml_digit
      | Chars "_-"
      | xml_combining_char | xml_extender | "\\." )]

let not_ncname_letter =
  [%sedlex.regexp?
      Compl
      ( 0x0041 .. 0x005A
      | 0x0061 .. 0x007A
      | 0x00C0 .. 0x00D6
      | 0x00D8 .. 0x00F6
      | 0x00F8 .. 0x00FF
      | 0x0100 .. 0x0131
      | 0x0134 .. 0x013E
      | 0x0141 .. 0x0148
      | 0x014A .. 0x017E
      | 0x0180 .. 0x01C3
      | 0x01CD .. 0x01F0
      | 0x01F4 .. 0x01F5
      | 0x01FA .. 0x0217
      | 0x0250 .. 0x02A8
      | 0x02BB .. 0x02C1
      | 0x0386 .. 0x0386
      | 0x0388 .. 0x038A
      | 0x038C .. 0x038C
      | 0x038E .. 0x03A1
      | 0x03A3 .. 0x03CE
      | 0x03D0 .. 0x03D6
      | 0x03DA .. 0x03DA
      | 0x03DC .. 0x03DC
      | 0x03DE .. 0x03DE
      | 0x03E0 .. 0x03E0
      | 0x03E2 .. 0x03F3
      | 0x0401 .. 0x040C
      | 0x040E .. 0x044F
      | 0x0451 .. 0x045C
      | 0x045E .. 0x0481
      | 0x0490 .. 0x04C4
      | 0x04C7 .. 0x04C8
      | 0x04CB .. 0x04CC
      | 0x04D0 .. 0x04EB
      | 0x04EE .. 0x04F5
      | 0x04F8 .. 0x04F9
      | 0x0531 .. 0x0556
      | 0x0559 .. 0x0559
      | 0x0561 .. 0x0586
      | 0x05D0 .. 0x05EA
      | 0x05F0 .. 0x05F2
      | 0x0621 .. 0x063A
      | 0x0641 .. 0x064A
      | 0x0671 .. 0x06B7
      | 0x06BA .. 0x06BE
      | 0x06C0 .. 0x06CE
      | 0x06D0 .. 0x06D3
      | 0x06D5 .. 0x06D5
      | 0x06E5 .. 0x06E6
      | 0x0905 .. 0x0939
      | 0x093D .. 0x093D
      | 0x0958 .. 0x0961
      | 0x0985 .. 0x098C
      | 0x098F .. 0x0990
      | 0x0993 .. 0x09A8
      | 0x09AA .. 0x09B0
      | 0x09B2 .. 0x09B2
      | 0x09B6 .. 0x09B9
      | 0x09DC .. 0x09DD
      | 0x09DF .. 0x09E1
      | 0x09F0 .. 0x09F1
      | 0x0A05 .. 0x0A0A
      | 0x0A0F .. 0x0A10
      | 0x0A13 .. 0x0A28
      | 0x0A2A .. 0x0A30
      | 0x0A32 .. 0x0A33
      | 0x0A35 .. 0x0A36
      | 0x0A38 .. 0x0A39
      | 0x0A59 .. 0x0A5C
      | 0x0A5E .. 0x0A5E
      | 0x0A72 .. 0x0A74
      | 0x0A85 .. 0x0A8B
      | 0x0A8D .. 0x0A8D
      | 0x0A8F .. 0x0A91
      | 0x0A93 .. 0x0AA8
      | 0x0AAA .. 0x0AB0
      | 0x0AB2 .. 0x0AB3
      | 0x0AB5 .. 0x0AB9
      | 0x0ABD .. 0x0ABD
      | 0x0AE0 .. 0x0AE0
      | 0x0B05 .. 0x0B0C
      | 0x0B0F .. 0x0B10
      | 0x0B13 .. 0x0B28
      | 0x0B2A .. 0x0B30
      | 0x0B32 .. 0x0B33
      | 0x0B36 .. 0x0B39
      | 0x0B3D .. 0x0B3D
      | 0x0B5C .. 0x0B5D
      | 0x0B5F .. 0x0B61
      | 0x0B85 .. 0x0B8A
      | 0x0B8E .. 0x0B90
      | 0x0B92 .. 0x0B95
      | 0x0B99 .. 0x0B9A
      | 0x0B9C .. 0x0B9C
      | 0x0B9E .. 0x0B9F
      | 0x0BA3 .. 0x0BA4
      | 0x0BA8 .. 0x0BAA
      | 0x0BAE .. 0x0BB5
      | 0x0BB7 .. 0x0BB9
      | 0x0C05 .. 0x0C0C
      | 0x0C0E .. 0x0C10
      | 0x0C12 .. 0x0C28
      | 0x0C2A .. 0x0C33
      | 0x0C35 .. 0x0C39
      | 0x0C60 .. 0x0C61
      | 0x0C85 .. 0x0C8C
      | 0x0C8E .. 0x0C90
      | 0x0C92 .. 0x0CA8
      | 0x0CAA .. 0x0CB3
      | 0x0CB5 .. 0x0CB9
      | 0x0CDE .. 0x0CDE
      | 0x0CE0 .. 0x0CE1
      | 0x0D05 .. 0x0D0C
      | 0x0D0E .. 0x0D10
      | 0x0D12 .. 0x0D28
      | 0x0D2A .. 0x0D39
      | 0x0D60 .. 0x0D61
      | 0x0E01 .. 0x0E2E
      | 0x0E30 .. 0x0E30
      | 0x0E32 .. 0x0E33
      | 0x0E40 .. 0x0E45
      | 0x0E81 .. 0x0E82
      | 0x0E84 .. 0x0E84
      | 0x0E87 .. 0x0E88
      | 0x0E8A .. 0x0E8A
      | 0x0E8D .. 0x0E8D
      | 0x0E94 .. 0x0E97
      | 0x0E99 .. 0x0E9F
      | 0x0EA1 .. 0x0EA3
      | 0x0EA5 .. 0x0EA5
      | 0x0EA7 .. 0x0EA7
      | 0x0EAA .. 0x0EAB
      | 0x0EAD .. 0x0EAE
      | 0x0EB0 .. 0x0EB0
      | 0x0EB2 .. 0x0EB3
      | 0x0EBD .. 0x0EBD
      | 0x0EC0 .. 0x0EC4
      | 0x0F40 .. 0x0F47
      | 0x0F49 .. 0x0F69
      | 0x10A0 .. 0x10C5
      | 0x10D0 .. 0x10F6
      | 0x1100 .. 0x1100
      | 0x1102 .. 0x1103
      | 0x1105 .. 0x1107
      | 0x1109 .. 0x1109
      | 0x110B .. 0x110C
      | 0x110E .. 0x1112
      | 0x113C .. 0x113C
      | 0x113E .. 0x113E
      | 0x1140 .. 0x1140
      | 0x114C .. 0x114C
      | 0x114E .. 0x114E
      | 0x1150 .. 0x1150
      | 0x1154 .. 0x1155
      | 0x1159 .. 0x1159
      | 0x115F .. 0x1161
      | 0x1163 .. 0x1163
      | 0x1165 .. 0x1165
      | 0x1167 .. 0x1167
      | 0x1169 .. 0x1169
      | 0x116D .. 0x116E
      | 0x1172 .. 0x1173
      | 0x1175 .. 0x1175
      | 0x119E .. 0x119E
      | 0x11A8 .. 0x11A8
      | 0x11AB .. 0x11AB
      | 0x11AE .. 0x11AF
      | 0x11B7 .. 0x11B8
      | 0x11BA .. 0x11BA
      | 0x11BC .. 0x11C2
      | 0x11EB .. 0x11EB
      | 0x11F0 .. 0x11F0
      | 0x11F9 .. 0x11F9
      | 0x1E00 .. 0x1E9B
      | 0x1EA0 .. 0x1EF9
      | 0x1F00 .. 0x1F15
      | 0x1F18 .. 0x1F1D
      | 0x1F20 .. 0x1F45
      | 0x1F48 .. 0x1F4D
      | 0x1F50 .. 0x1F57
      | 0x1F59 .. 0x1F59
      | 0x1F5B .. 0x1F5B
      | 0x1F5D .. 0x1F5D
      | 0x1F5F .. 0x1F7D
      | 0x1F80 .. 0x1FB4
      | 0x1FB6 .. 0x1FBC
      | 0x1FBE .. 0x1FBE
      | 0x1FC2 .. 0x1FC4
      | 0x1FC6 .. 0x1FCC
      | 0x1FD0 .. 0x1FD3
      | 0x1FD6 .. 0x1FDB
      | 0x1FE0 .. 0x1FEC
      | 0x1FF2 .. 0x1FF4
      | 0x1FF6 .. 0x1FFC
      | 0x2126 .. 0x2126
      | 0x212A .. 0x212B
      | 0x212E .. 0x212E
      | 0x2180 .. 0x2182
      | 0x3041 .. 0x3094
      | 0x30A1 .. 0x30FA
      | 0x3105 .. 0x312C
      | 0xAC00 .. 0xD7A3
      (* ideographic *)
      | 0x3007 .. 0x3007
      | 0x3021 .. 0x3029
      | 0x4E00 .. 0x9FA5
      (* '_' *)
      | '_' )]

let ncname =
  [%sedlex.regexp? xml_letter, Star ncname_char | '_', Plus ncname_char]

let qname = [%sedlex.regexp? Opt (ncname, ':'), ncname]
let digit = [%sedlex.regexp? '0' .. '9']
let float_exp = [%sedlex.regexp? ('e' | 'E'), Opt ('+' | '-'), Plus digit]
let float_frac = [%sedlex.regexp? '.', Star digit]

let floating_point =
  [%sedlex.regexp?
      Plus digit, float_frac | Plus digit, Opt float_frac, float_exp]

let illegal lexbuf =
  error lexbuf
    (Printf.sprintf "Illegal character : %s" (L.Utf8.lexeme lexbuf))

let in_comment = ref false

(* used for the heuristic of polymorphic variables *)
let in_list = ref 0

let ident_or_keyword =
  let l =
    [
      ("and", AND);
      ("debug", DEBUG);
      ("div", DIV);
      ("else", ELSE);
      ("from", FROM);
      ("fun", FUN);
      ("if", IF);
      ("in", IN);
      ("include", INCLUDE);
      ("let", LET);
      ("map", MAP);
      ("match", MATCH);
      ("mod", MOD);
      ("namespace", NAMESPACE);
      ("off", OFF);
      ("on", ON);
      ("open", OPEN);
      ("or", OR);
      ("ref", REF);
      ("schema", SCHEMA);
      ("select", SELECT);
      ("then", THEN);
      ("transform", TRANSFORM);
      ("try", TRY);
      ("type", TYPE);
      ("using", USING);
      ("validate", VALIDATE);
      ("where", WHERE);
      ("with", WITH);
      ("xtransform", XTRANSFORM);
    ]
  in
  let hash = Hashtbl.create 17 in
  List.iter (fun (a, b) -> Hashtbl.add hash a b) l;
  function
  | s -> (
      try Hashtbl.find hash s with
      | Not_found -> IDENT s)

let rec token lexbuf =
  match%sedlex lexbuf with
  | Plus xml_blank -> token lexbuf
  | qname -> ident_or_keyword (L.Utf8.lexeme lexbuf)
  | "_" -> UNDERSCORE
  | "#print_type" -> HASH_PRINT_TYPE
  | "#dump_value" -> HASH_DUMP_VALUE
  | "#ascii" -> HASH_ASCII
  | "#latin1" -> HASH_LATIN1
  | "#utf8" -> HASH_UTF8
  | "#", qname -> HASH_DIRECTIVE (L.Utf8.lexeme lexbuf)
  | ncname, ":*" ->
    let s = L.Utf8.sub_lexeme lexbuf 0 (L.lexeme_length lexbuf - 2) in
    ANY_IN_NS s
  | ".:*" -> ANY_IN_NS ""
  | floating_point -> (
      let f = L.Utf8.lexeme lexbuf in
      try FLOAT (float_of_string f) with
      | _ ->
        error lexbuf ("invalid floating point constant `" ^ f ^ "`"))
  | Plus '0' .. '9' -> INT (L.Utf8.lexeme lexbuf)
  | "(" -> LP
  | ")" -> RP
  | "[" ->
    incr in_list;
    LSB
  | "]" ->
    decr in_list;
    RSB
  | "<" -> LT
  | ">" -> GT
  | "{" -> LCB
  | "}" -> RCB
  | ":" -> COLON
  | "," -> COMMA
  | "?" -> QMARK
  | "=" -> EQ
  | "+" -> PLUS
  | "-" -> MINUS
  | "@" -> AT
  | "|" -> BAR
  | "." -> DOT
  | "`" -> BQUOTE
  | "!" -> BANG
  | "\\" -> SETMINUS
  | "*" -> STAR
  | "&" -> AMP
  | "/" -> SLASH
  | ";" -> SEMI
  | ":=" -> COLEQ
  | "->" -> MINUSGT
  | "<=" -> LTEQ
  | "<<" -> LTLT
  | ">>" -> GTGT
  | ">=" -> GTEQ
  | "!=" -> BANGEQ
  | "&&" -> AMPAMP
  | "**" -> STARSTAR
  | "/@" -> SLASHAT
  | "//" -> SLASHSLASH
  | "::" -> COLCOL
  | ".." -> DOTDOT
  | "--" -> MINUSMINUS
  | "--*" -> MINUSMINUSSTAR
  | "??" -> QMARKQMARK
  | "+?" -> PLUSQMARK
  | "*?" -> STARQMARK
  | "*--" -> STARMINUSMINUS
  | "=?" -> EQQMARK
  | "||" -> BARBAR
  | ";;" -> SEMISEMI
  | "'" ->
    L.rollback lexbuf;
    if !in_list = 0 then single_quote_outside_list lexbuf
    else single_quote_inside_list lexbuf
  | '"'
  | "'" ->
    L.rollback lexbuf;
    do_string lexbuf
  | "(*" ->
    in_comment := true;
    comment (L.lexeme_start lexbuf) lexbuf;
    in_comment := false;
    token lexbuf
  | "/*" ->
    in_comment := true;
    tcomment (L.lexeme_start lexbuf) lexbuf;
    in_comment := false;
    token lexbuf
  | eof -> EOI
  | sm -> (
      match L.lexeme lexbuf with
      | [| uc |] -> (
          match Pretty_utf8.get_utf8_binding uc with
          | str ->
            let lexbuf = L.Latin1.from_string str in
            token lexbuf
          | exception _ -> illegal lexbuf)
      | _ -> illegal lexbuf)
  | any -> illegal lexbuf
  | _ -> assert false

and do_string lexbuf =
  match%sedlex lexbuf with
  | '"'
  | "'" ->
    let double_quote = L.Latin1.lexeme_char lexbuf 0 = '"' in
    string (L.lexeme_start lexbuf) double_quote lexbuf;
    let s = get_stored_string () in
    if double_quote then STRING2 s else STRING1 s
  | _ -> assert false

and single_quote_outside_list lexbuf =
  match%sedlex lexbuf with
  | ( "'",
      ( '\\', (Chars "\\rnt'\"" | Plus (Chars "x0123456789ABCDEFabcdef"), ';')
      | Compl (Chars "\'\\") ),
      "'" ) ->
    L.rollback lexbuf;
    do_string lexbuf
  | "'", ncname ->
    (* then try to read it as variable *)
    let s = L.Utf8.sub_lexeme lexbuf 1 (L.lexeme_length lexbuf - 1) in
    POLY s
  | any -> illegal lexbuf
  | _ -> assert false

and single_quote_inside_list lexbuf =
  (*
        lexing of single quoted string/polymorphic variables
         these rules should also be used in the comment lexer.
    *)
  match%sedlex lexbuf with
  | ( "'",
      Star (Compl (Chars "\t\n\r\'") | '\\', '\''),
      "'",
      (not_ncname_letter | eof) ) ->
    (* two single quotes not followed by an xml_letter must be a string
       we put it back call an auxiliary lexer to consume the first '
       and read it as a string. *)
    L.rollback lexbuf;
    do_string lexbuf
  | "'", ncname, "'", ncname ->
    (* Tokenize [ 'abc'abc ] as [ 'abc' abc ], to ensure backward compatibility with 0.x CDuce *)
    let s = L.Utf8.lexeme lexbuf in
    warning lexbuf
      (Printf.sprintf
         "string literal followed by an identifier ``%s'' is ambiguous. Add \
          a space after the second quote."
         s);
    L.rollback lexbuf;
    do_string lexbuf
  | "'", ncname ->
    (* then try to read it as variable *)
    let s = L.Utf8.sub_lexeme lexbuf 1 (L.lexeme_length lexbuf - 1) in
    POLY s
  | '"'
  | "'" ->
    (* otherwise we will fail for sure, but try to read it character by
       character as a string to get a decent error message *)
    L.rollback lexbuf;
    do_string lexbuf
  | _ -> assert false

and comment start lexbuf =
  match%sedlex lexbuf with
  | "(*" ->
    comment (L.lexeme_start lexbuf) lexbuf;
    comment start lexbuf
  | "*)" -> ()
  | "'" ->
    L.rollback lexbuf;
    ignore (single_quote_inside_list lexbuf);
    comment start lexbuf
  | '"'
  | "'" ->
    L.rollback lexbuf;
    ignore (do_string lexbuf);
    comment start lexbuf
  | eof -> error lexbuf "Unterminated comment"
  | any -> comment start lexbuf
  | _ -> assert false

and tcomment start lexbuf =
  match%sedlex lexbuf with
  | "*/" -> ()
  | eof -> error lexbuf "Unterminated comment"
  | any -> tcomment start lexbuf
  | _ -> assert false

and string start double lexbuf =
  match%sedlex lexbuf with
  | '"'
  | "'" ->
    let d = L.Latin1.lexeme_char lexbuf 0 = '"' in
    if d != double then (
      store_lexeme lexbuf;
      string start double lexbuf)
  | '\\', Chars "\\\"\'" ->
    store_ascii (L.Latin1.lexeme_char lexbuf 1);
    string start double lexbuf
  | "\\n" ->
    store_ascii '\n';
    string start double lexbuf
  | "\\t" ->
    store_ascii '\t';
    string start double lexbuf
  | "\\r" ->
    store_ascii '\r';
    string start double lexbuf
  | '\\', Plus '0' .. '9', ';' ->
    store_code (parse_char lexbuf 10 1);
    string start double lexbuf
  | "\\x", Plus ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F'), ';' ->
    store_code (parse_char lexbuf 16 2);
    string start double lexbuf
  | '\\' -> illegal lexbuf
  | eof -> error lexbuf "Unterminated string"
  | any ->
    store_lexeme lexbuf;
    string start double lexbuf
  | _ -> assert false

let token lexbuf =
  try token lexbuf with
  | e ->
    clear_buff ();
    in_list := 0;
    in_comment := false;
    (* reinit encoding ? *)
    raise e

let eat_shebang lexbuf =
  match%sedlex lexbuf with
  | Opt ("#!", Star (Compl '\n'), "\n") -> ()
  | _ -> ()
