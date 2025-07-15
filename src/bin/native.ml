open Sstt_repl.Main
open Sstt_repl.Parsing
open Output

let () =
    Printexc.record_backtrace true;
    if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
    try
        let fn = if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None in      
        let run () =
            match fn with
            | Some fn -> IO.parse_program_file fn |>
                List.fold_left Repl.treat_elt Repl.empty_env |> ignore
            | None ->
                Format.fprintf Format.std_formatter "Simple Set-Theoretic Types (SSTT) - REPL@." ;
                Format.fprintf Format.std_formatter "Version %s (commit %s, compiler %s)@."
                    Version.version Version.commit Version.compiler ;
                let buf = Lexing.from_channel stdin in
                let rec repl env =
                    begin try
                        Format.fprintf Format.std_formatter "> @?" ;
                        match IO.parse_command buf with
                        | End -> ()
                        | Elt elt -> Repl.treat_elt env elt |> repl
                    with
                    | e -> print Error "%s" (Printexc.to_string e) ; repl env
                    end
                in          
                repl Repl.empty_env
        in
        with_rich_output Format.std_formatter run ()
    with
    | IO.LexicalError (p, msg)
    | IO.SyntaxError (p, msg) ->
        Format.printf "@.%s: %s@." (Position.string_of_pos p) msg
    | e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
        Format.printf "@.Uncaught exception: %s@.%s@." msg stack
