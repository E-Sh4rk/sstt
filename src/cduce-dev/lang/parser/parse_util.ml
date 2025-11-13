let pp_token ?(content = false) fmt t =
  let pp = Format.fprintf fmt in
  let open Parser in
  match t with
  | AMP -> pp "%s" "AMP"
  | AMPAMP -> pp "%s" "AMPAMP"
  | AND -> pp "%s" "AND"
  | ANY_IN_NS s ->
      pp "%s" "ANY_IN_NS";
      if content then pp "(\"%s\")" s
  | AT -> pp "%s" "AT"
  | BANG -> pp "%s" "BANG"
  | BANGEQ -> pp "%s" "BANGEQ"
  | BAR -> pp "%s" "BAR"
  | BARBAR -> pp "%s" "BARBAR"
  | BQUOTE -> pp "%s" "BQUOTE"
  | COLCOL -> pp "%s" "COLCOL"
  | COLEQ -> pp "%s" "COLEQ"
  | COLON -> pp "%s" "COLON"
  | COMMA -> pp "%s" "COMMA"
  | DEBUG -> pp "%s" "DEBUG"
  | DIV -> pp "%s" "DIV"
  | DOT -> pp "%s" "DOT"
  | DOTDOT -> pp "%s" "DOTDOT"
  | ELSE -> pp "%s" "ELSE"
  | EOI -> pp "%s" "EOI"
  | EQ -> pp "%s" "EQ"
  | EQQMARK -> pp "%s" "EQQMARK"
  | FLOAT f ->
      pp "%s" "IDENT";
      if content then pp "(\"%s\")" (string_of_float f)
  | FROM -> pp "%s" "FROM"
  | FUN -> pp "%s" "FUN"
  | GT -> pp "%s" "GT"
  | GTEQ -> pp "%s" "GTEQ"
  | GTGT -> pp "%s" "GTGT"
  | HASH_ASCII -> pp "%s" "HASH_ASCII"
  | HASH_DIRECTIVE s ->
      pp "%s" "HASH_DIRECTIVE";
      if content then pp "(\"%s\")" s
  | HASH_DUMP_VALUE -> pp "%s" "HASH_DUMP_VALUE"
  | HASH_LATIN1 -> pp "%s" "HASH_LATIN1"
  | HASH_PRINT_TYPE -> pp "%s" "HASH_PRINT_TYPE"
  | HASH_UTF8 -> pp "%s" "HASH_UTF8"
  | IDENT s ->
      pp "%s" "IDENT";
      if content then pp "(\"%s\")" s
  | IF -> pp "%s" "IF"
  | IN -> pp "%s" "IN"
  | INCLUDE -> pp "%s" "INCLUDE"
  | INT s ->
      pp "%s" "INT";
      if content then pp "(\"%s\")" s
  | LCB -> pp "%s" "LCB"
  | LET -> pp "%s" "LET"
  | LP -> pp "%s" "LP"
  | LSB -> pp "%s" "LSB"
  | LT -> pp "%s" "LT"
  | LTEQ -> pp "%s" "LTEQ"
  | LTLT -> pp "%s" "LTLT"
  | MAP -> pp "%s" "MAP"
  | MATCH -> pp "%s" "MATCH"
  | MINUS -> pp "%s" "MINUS"
  | MINUSGT -> pp "%s" "MINUSGT"
  | MINUSMINUS -> pp "%s" "MINUSMINUS"
  | MINUSMINUSSTAR -> pp "%s" "MINUSMINUSSTAR"
  | MOD -> pp "%s" "MOD"
  | NAMESPACE -> pp "%s" "NAMESPACE"
  | OFF -> pp "%s" "OFF"
  | ON -> pp "%s" "ON"
  | OPEN -> pp "%s" "OPEN"
  | OR -> pp "%s" "OR"
  | POLY s ->
      pp "%s" "POLY";
      if content then pp "(\"%s\")" s
  | PLUS -> pp "%s" "PLUS"
  | PLUSQMARK -> pp "%s" "PLUSQMARK"
  | QMARK -> pp "%s" "QMARK"
  | QMARKQMARK -> pp "%s" "QMARKQMARK"
  | RCB -> pp "%s" "RCB"
  | REF -> pp "%s" "REF"
  | RESOLVED_INCLUDE _ ->
      pp "%s" "RESOLVED_INCLUDE";
      if content then pp "%s" "([ ... ])"
  | RP -> pp "%s" "RP"
  | RSB -> pp "%s" "RSB"
  | SCHEMA -> pp "%s" "SCHEMA"
  | SELECT -> pp "%s" "SELECT"
  | SEMI -> pp "%s" "SEMI"
  | SEMISEMI -> pp "%s" "SEMISEMI"
  | SETMINUS -> pp "%s" "SETMINUS"
  | SLASH -> pp "%s" "SLASH"
  | SLASHAT -> pp "%s" "SLASHAT"
  | SLASHSLASH -> pp "%s" "SLASHSLASH"
  | STAR -> pp "%s" "STAR"
  | STARMINUSMINUS -> pp "%s" "STARMINUSMINUS"
  | STARQMARK -> pp "%s" "STARQMARK"
  | STARSTAR -> pp "%s" "STARSTAR"
  | STRING1 s ->
      pp "%s" "STRING1";
      if content then pp "(\"%s\")" s
  | STRING2 s ->
      pp "%s" "STRING2";
      if content then pp "(\"%s\")" s
  | THEN -> pp "%s" "THEN"
  | TRANSFORM -> pp "%s" "TRANSFORM"
  | TRY -> pp "%s" "TRY"
  | TYPE -> pp "%s" "TYPE"
  | UNDERSCORE -> pp "%s" "UNDERSCORE"
  | USING -> pp "%s" "USING"
  | VALIDATE -> pp "%s" "VALIDATE"
  | WHERE -> pp "%s" "WHERE"
  | WITH -> pp "%s" "WITH"
  | XTRANSFORM -> pp "%s" "XTRANSFORM"

let all_tokens =
  Parser.
    [
      (AMP, "&");
      (AMPAMP, "&&");
      (AND, "and");
      (ANY_IN_NS "ns", ".:*");
      (AT, "@");
      (BANG, "!");
      (BANGEQ, "!=");
      (BAR, "|");
      (BARBAR, "||");
      (BQUOTE, "`");
      (COLCOL, "::");
      (COLEQ, ":=");
      (COLON, ":");
      (COMMA, ",");
      (DEBUG, "debug");
      (DIV, "div");
      (DOT, ".");
      (DOTDOT, "..");
      (ELSE, "else");
      (EOI, "the end of input");
      (EQ, "=");
      (EQQMARK, "=?");
      (FLOAT 42.0, "a float");
      (FROM, "from");
      (FUN, "fun");
      (GT, ">");
      (GTEQ, ">=");
      (GTGT, ">>");
      (HASH_ASCII, "#ascii");
      (HASH_DIRECTIVE "#quiet", "#quiet");
      (HASH_DUMP_VALUE, "#dump_value");
      (HASH_LATIN1, "#latin1");
      (HASH_PRINT_TYPE, "#print_type");
      (HASH_UTF8, "#utf8");
      (IDENT "x", "a variable");
      (IF, "if");
      (IN, "in");
      (INCLUDE, "include");
      (INT "42", "an integer");
      (LCB, "{");
      (LET, "let");
      (LP, "(");
      (LSB, "[");
      (LT, "<");
      (LTEQ, "<=");
      (LTLT, "<<");
      (MAP, "map");
      (MATCH, "match");
      (MINUS, "-");
      (MINUSGT, "->");
      (MINUSMINUS, "--");
      (MINUSMINUSSTAR, "--*");
      (MOD, "mod");
      (NAMESPACE, "namespace");
      (OFF, "off");
      (ON, "on");
      (OPEN, "open");
      (OR, "or");
      (POLY "a", "a type variable");
      (PLUS, "+");
      (PLUSQMARK, "+?");
      (QMARK, "?");
      (QMARKQMARK, "??");
      (RCB, "]");
      (REF, "ref");
      (* (this is a pseudo token, don't consider it a candidate)
         (RESOLVED_INCLUDE [], "");
      *)
      (RP, ")");
      (RSB, "]");
      (SCHEMA, "schema");
      (SELECT, "select");
      (SEMI, ";");
      (SEMISEMI, ";;");
      (SETMINUS, "\\");
      (SLASH, "/");
      (SLASHAT, "/@");
      (SLASHSLASH, "//");
      (STAR, "*");
      (STARMINUSMINUS, "*--");
      (STARQMARK, "?");
      (STARSTAR, "**");
      (STRING1 "hello", "a character literal");
      (STRING2 "hello", "a string literal");
      (THEN, "then");
      (TRANSFORM, "transform");
      (TRY, "try");
      (TYPE, "type");
      (UNDERSCORE, "_");
      (USING, "using");
      (VALIDATE, "validate");
      (WHERE, "where");
      (WITH, "with");
      (XTRANSFORM, "xtransform");
    ]

let escape_string s =
  let b = Buffer.create (String.length s) in
  let rec loop idx end_ us =
    if idx = end_ then Buffer.contents b
    else
      let cp, nidx = Encodings.Utf8.next us idx in
      let () =
        match cp with
        | 10 -> Buffer.add_string b "\\n"
        | 9 -> Buffer.add_string b "\\t"
        | 13 -> Buffer.add_string b "\\r"
        | _ when cp < 32 || cp > 127 ->
            Buffer.add_char b '\\';
            Buffer.add_string b (string_of_int cp)
        | _ -> Buffer.add_char b (Char.unsafe_chr cp)
      in
      loop nidx end_ us
  in
  let us = Encodings.Utf8.mk s in
  loop (Encodings.Utf8.start_index us) (Encodings.Utf8.end_index us) us

let string_of_token tok =
  let open Parser in
  match tok with
  | ANY_IN_NS n -> n ^ ":*"
  | HASH_DIRECTIVE n -> "n"
  | INT i -> i
  | IDENT i -> i
  | POLY s -> "'" ^ s
  | FLOAT f -> string_of_float f
  | STRING1 s -> "'" ^ escape_string s ^ "'"
  | STRING2 s -> "\"" ^ escape_string s ^ "\""
  | RESOLVED_INCLUDE _ -> ""
  | _ -> (
      try List.assoc tok all_tokens with
      | Not_found ->
          Format.sprintf "Unknown token %d\n" (Obj.tag (Obj.repr tok)))

let text_of_token tok =
  try List.assoc tok all_tokens with
  | Not_found -> ""

let expect_message fmt l =
  let l = List.sort compare l in
  match l with
  | [] -> ()
  | [ t ] -> Format.fprintf fmt "@\nExpecting token ``%s''" t
  | f :: rest ->
      let rec loop l =
        match l with
        | [] -> ()
        | [ t ] -> Format.fprintf fmt " or ``%s''" t
        | t :: ll ->
            Format.fprintf fmt ", ``%s''" t;
            loop ll
      in
      Format.fprintf fmt "@\nExpecting token ``%s''" f;
      loop rest
