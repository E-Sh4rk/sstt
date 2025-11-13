let dummy_from_source src = Lexing.{dummy_pos with pos_fname = src }

let seq_of_fun f =
  let rec self () =
    match f () with
    | Some c -> Seq.Cons (c, self)
    | None -> Seq.Nil
  in
  self

let seq_of_in_channel ic =
  seq_of_fun (fun () ->
      try Some (input_char ic) with
      | End_of_file -> None)

let next rseq =
  match !rseq () with
  | Seq.Cons (v, f) ->
    rseq := f;
    v
  | Seq.Nil -> Cduce_error.(raise_err Parse_Failure ())

let default_encoding = `Utf8

let str_encoding = function
  | `Ascii -> "ascii"
  | `Latin1 -> "latin-1"
  | `Utf8 -> "utf-8"

let invalid_byte_c c e =
  Cduce_error.(raise_err Parse_Invalid_byte (Format.sprintf "\\%x" (Char.code c), e))

let invalid_byte s e =
  let acc = ref "" in
  for i = 0 to String.length s - 1 do
    acc := Format.sprintf "\\%x%s" (Char.code s.[i]) !acc
  done;
  Cduce_error.(raise_err Parse_Invalid_byte (!acc, e))

(* Taken from Menhir/Lib/Convert.ml*)

let mk_lexbuf source enc cs =
  (* Workaround the sedlex functions buffer 512 characters before
     propagating them, making it unusable with interactive input *)
  let module U = Encodings.Utf8 in
  let uchars = Bytes.make 4 '\000' in
  let read_uchar () =
    let us = U.mk (Bytes.unsafe_to_string uchars) in
    let uc = U.get us (U.start_index us) in
    Uchar.unsafe_of_int uc
  in
  let set_continuation_byte i c =
    (* assumes i = 1, 2 or 3 *)
    Bytes.set uchars i c;
    (* we set it anyway, and test after *)
    let cc = Char.code c in
    if cc lsr 6 != 0b10 then
      (* throw exception with invalid byte in the buffer *)
      invalid_byte (Bytes.sub_string uchars 0 (i + 1)) `Utf8
  in
  let lexbuf =
    let closed = ref false in
    Sedlexing.create (fun arr pos _num ->
        if !closed then raise End_of_file;
        try
          Bytes.set uchars 0 '\000';
          Bytes.set uchars 1 '\000';
          Bytes.set uchars 2 '\000';
          Bytes.set uchars 3 '\000';
          let c0 = next cs in
          let codepoint =
            match (c0, !enc) with
            | '\x00' .. '\x7f', _ -> Uchar.of_char c0
            | '\x80' .. '\xff', `Latin1 -> Uchar.of_char c0
            | '\xc0' .. '\xdf', `Utf8 ->
              Bytes.set uchars 0 c0;
              set_continuation_byte 1 (next cs);
              read_uchar ()
            | '\xe0' .. '\xef', `Utf8 ->
              Bytes.set uchars 0 c0;
              set_continuation_byte 1 (next cs);
              set_continuation_byte 2 (next cs);
              read_uchar ()
            | '\xf0' .. '\xf7', `Utf8 ->
              Bytes.set uchars 0 c0;
              set_continuation_byte 1 (next cs);
              set_continuation_byte 2 (next cs);
              set_continuation_byte 3 (next cs);
              read_uchar ()
            | c, e -> invalid_byte_c c e
          in
          arr.(pos) <- codepoint;
          1
        with
        | Cduce_error.(Error (_, (Parse_Failure,_))) when not !closed ->
          closed := true;
          0)
  in
  Sedlexing.set_position lexbuf
    Lexing.{ pos_fname = source; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  Sedlexing.set_filename lexbuf source;
  let () =
    try Sedlexer.eat_shebang lexbuf with
    | Sedlexing.MalFormed -> invalid_byte (Sedlexing.Latin1.lexeme lexbuf) !enc
  in
  lexbuf

let include_stack = ref []

let close_in ic =
  try close_in ic with
  | _ -> ()

let exit_include ic =
  close_in ic;
  include_stack := List.tl !include_stack

let last_tok = ref Parser.EOI
let last_tok_pos = ref (Lexing.dummy_pos, Lexing.dummy_pos)

let rec token enc lexbuf =
  let set_enc e = enc := e in
  let f = Sedlexing.with_tokenizer Sedlexer.token lexbuf in
  let f () =
    let tok, p1, p2 = f () in
    let tok =
      match (!last_tok, tok) with
      | _, HASH_ASCII ->
        set_enc `Ascii;
        tok
      | _, HASH_LATIN1 ->
        set_enc `Latin1;
        tok
      | _, HASH_UTF8 ->
        set_enc `Utf8;
        tok
      | Parser.INCLUDE, Parser.STRING2 path -> (
          let path = Cduce_loc.resolve_filename path in
          let () = Cduce_loc.add_to_obj_path (Filename.dirname path) in
          if List.mem path !include_stack then tok
          else
            let ic =
              try open_in path with
              | Sys_error msg ->
                let last_p1, _ = !last_tok_pos in
                Cduce_error.(raise_err_loc ~loc:(last_p1, p2)
                               Ast_Parsing_error (Format.sprintf "include \"%s\" : %s" path msg))

            in
            include_stack := path :: !include_stack;
            try
              let cs = ref (seq_of_in_channel ic) in
              let newenc = ref default_encoding in
              (* or ref !enc ? *)
              let newlb = mk_lexbuf path newenc cs in
              let past = pre_prog path (token newenc newlb) in
              exit_include ic;
              Parser.RESOLVED_INCLUDE past
            with
            | e ->
              exit_include ic;
              raise e)
      | _ -> tok
    in
    last_tok := tok;
    last_tok_pos := (p1, p2);
    (tok, p1, p2)
  in
  f

and incremental source parser token =
  let open Parser.MenhirInterpreter in
  let start_pos = (dummy_from_source source) in
  let init = parser start_pos
  in
  let last_token = ref (Parser.EOI, start_pos, start_pos) in
  let last_checkpoint = ref init in
  let par_stack = ref [] in
  let rec loop checkpoint =
    match checkpoint with
    | InputNeeded _ ->
      last_checkpoint := checkpoint;
      last_token := token ();
      (match (!last_token, !par_stack) with
       | ((LP | LSB | LCB), _, _), _ -> par_stack := !last_token :: !par_stack
       | (((RP | RSB | RCB) as b), _, _), (t, _, _) :: rest when t = b ->
         par_stack := rest
       | _ -> () (* will yield an error*));
      loop (offer checkpoint !last_token)
    | Shifting _
    | AboutToReduce _ ->
      loop (resume checkpoint)
    | Accepted v -> v
    | Rejected -> raise Parser.Error
    | HandlingError env ->
      let last_token, last_spos, last_epos = !last_token in
      let has_open, candidates =
        List.fold_left
          (fun (cp, acc) (tok, stok) ->
             match
               (tok, !par_stack, acceptable !last_checkpoint tok last_spos)
             with
             | RP, (LP, _, _) :: _, true
             | RCB, (LCB, _, _) :: _, true ->
               (true, stok :: acc)
             | RSB, (LSB, _, _) :: _, true -> (true, stok :: acc)
             | _, _, true -> (cp, stok :: acc)
             | _, _, false -> (cp, acc)
             | exception _ -> (cp, acc))
          (false, []) Parse_util.all_tokens
      in

      let loc = (last_spos, last_epos) in
      let msg =
        Format.asprintf "invalid token ``%s''"
          (Parse_util.string_of_token last_token)
        ^
        if has_open then
          let op, i, j = List.hd !par_stack in
          let loc = i, j in
          Format.asprintf
            "%a: The opening parenthesis ``%s'' might be unmatched"
            (fun fmt l -> Cduce_loc.print_loc fmt (l, `Full))
            loc
            (Parse_util.string_of_token op)
        else Format.asprintf "%a" Parse_util.expect_message candidates
      in
      Cduce_error.(raise_err_loc ~loc  Ast_Parsing_error msg) 
  in
  loop init

and pre_prog source lb = incremental source Parser.Incremental.prog lb

let rec sync f =
  match !last_tok with
  | Parser.EOI
  | Parser.SEMISEMI ->
    ()
  | t ->
    let tok, p1, p2 = f () in
    last_tok := tok;
    last_tok_pos := (p1, p2);
    sync f

let get_loc lexbuf = Sedlexing.lexing_positions lexbuf

let protect_parser ?global_enc source do_sync gram stream =
  let enc =
    match global_enc with
    | Some e -> e
    | None -> ref default_encoding
  in
  let b = mk_lexbuf source enc stream in
  try
    let f = token enc b in
    try gram f with
    | e ->
      if do_sync then sync f;
      raise e
  with
  | Parser.Error ->
    let loc = get_loc b in
    Cduce_error.(raise_err_loc ~loc Ast_Parsing_error "")
  | Cduce_error.Error (_, (Parse_Invalid_byte, (c, e))) ->
    let loc = get_loc b in
    let msg = if String.length c > 1 then " sequence" else "" in
    Cduce_error.(raise_err_loc ~loc Ast_Parsing_error
                   (Format.sprintf "Invalid byte%s %s for %s encoding" msg c (str_encoding e)))
  | Sedlexing.MalFormed ->
    let loc = get_loc b in
    Cduce_error.(raise_err_loc ~loc Sedlexer_Error "MalFormed")

let prog ?(source="") cs = protect_parser source false (pre_prog source) (ref cs)

let top_phrases ?(source="") cs =
  protect_parser ~global_enc:(ref default_encoding) source true
    (incremental source Parser.Incremental.top_phrases)
    (ref cs)

let simple_parser source p cs =
  let enc = ref default_encoding in
  let supplier = token enc (mk_lexbuf source enc (ref cs)) in
  Parser.(MenhirInterpreter.loop supplier (p (dummy_from_source source)))

let pat ?(source="") cs = simple_parser source Parser.Incremental.parse_pat cs
let expr ?(source="") cs = simple_parser source Parser.Incremental.parse_expr cs

let protect_exn f g =
  try
    let x = f () in
    g ();
    x
  with
  | e ->
    g ();
    raise e

let sync () = ()
