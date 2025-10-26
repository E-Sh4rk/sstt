open Sstt_repl
(* open Output
open Yojson.Safe *)

let () =
    if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
    failwith "TODO"
