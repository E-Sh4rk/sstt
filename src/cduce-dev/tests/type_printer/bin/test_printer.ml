let parse_type_defs env cs =
  let open Cduce_lib in
  let open Cduce_types in
  let ast = Parse.prog cs in
  let ast_types =
    List.fold_left
      (fun acc d ->
        match d.Cduce_loc.descr with
        | Ast.TypeDecl ((c, l, []), n) -> (c, l, [], n) :: acc
        | _ -> acc)
      [] ast
  in
  let env = Typer.type_defs env ast_types in
  let orig_types =
    List.fold_left
      (fun acc (_, l, _, _) ->
        ( l,
          Types.descr
            (Typer.typ env (Cduce_loc.mknoloc (Ast.PatVar ([ l ], [])))) )
        :: acc)
      [] ast_types
  in
  (env, orig_types)

let () =
  if Array.length Sys.argv != 2 then exit 1
  else
    try
      let ic = open_in Sys.argv.(1) in
      let cs = Cduce_lib.Parse.seq_of_in_channel ic in
      let open Cduce_types in
      let open Cduce_lib in
      let env, orig_types = parse_type_defs Builtin.env cs in
      let new_types_txt =
        let open Format in
        asprintf "%a"
          (pp_print_list (fun ppf (u, t) ->
               fprintf ppf "type %a_new = %a;;\n" Encodings.Utf8.print u
                 Types.Print.print_noname t))
          orig_types
      in
      let cs2 = String.to_seq new_types_txt [@@alert "-deprecated"] in
      let _, new_types = parse_type_defs env cs2 in
      List.iter2
        (fun (n, ta) (_, tb) ->
          if not (Types.equiv ta tb) then begin
            Format.printf
              "ERROR:\noriginal type %a:\n%a\n-----\nprinted as:\n%a\n"
              Encodings.Utf8.print n Types.Print.print_noname ta
              Types.Print.print_noname tb
          end
          else begin
            Format.printf "OK:\ntype %a\n-----printed and reparsed correctly\n"
              Types.Print.print_noname tb
          end)
        orig_types new_types;
      exit 0
    with
    | Cduce_core.Cduce_error.Error _ as e ->
        Cduce_core.Cduce_error.print_exn Format.err_formatter e;
        (* Format.eprintf "%s" (Printexc.to_string e); *)
        exit 1
