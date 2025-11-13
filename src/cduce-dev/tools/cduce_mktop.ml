let normalize_path =
  if Sys.win32 then fun s ->
    String.concat Filename.dir_sep (String.split_on_char '/' s)
  else fun s -> s

let run_cmd c =
  let first, rest =
    let l = String.split_on_char ' ' c in
    (List.hd l, List.tl l)
  in
  let c = String.concat " " (Filename.quote (normalize_path first) :: rest) in
  let stdout, _, stderr = Unix.open_process_full c (Unix.environment ()) in
  let rec loop chan acc =
    match input_line chan with
    | s -> loop chan (s :: acc)
    | exception End_of_file -> acc
  in
  List.rev (loop stderr (loop stdout []))

let link_flags : [ `I of string | `P of string | `L of string ] list ref =
  ref []

let set_i s = link_flags := `I s :: !link_flags
let set_p s = link_flags := `P s :: !link_flags
let set_l s = link_flags := `L s :: !link_flags
let native = ref true
let target = ref ""
let prim_file = ref ""
let ext = if Sys.win32 then ".exe" else ""
let ocamlfind_prog = ref ("ocamlfind" ^ ext)
let cduce_prog = ref ("cduce" ^ ext)

let set_args s =
  if !target = "" then target := s
  else if !prim_file = "" then prim_file := s
  else raise (Arg.Bad "too many arguments")

let specs =
  Arg.(
    align
      [
        ( "-byte",
          Unit (fun () -> native := false),
          "  Compile with ocamlc (default to ocamlopt)" );
        ( "-l",
          String set_l,
          "<file.{cmo,cma,cmx,cmxa}>  Link with the specified object" );
        ( "-p",
          String set_p,
          "<package>  Link with the specified ocamlfind package" );
        ( "-I",
          String set_i,
          "<dir>  Add <dir> to the list of include directories" );
        ( "-cduce",
          Set_string cduce_prog,
          "<prog>  Use <prog> as the cduce compiler" );
        ( "-ocamlfind",
          Set_string ocamlfind_prog,
          "<prog>  Use <prog> as the ocamlfind command" );
      ])

let usage =
  Printf.sprintf
    "%s [OPTION...] <target> <prims>\n\n\
     Create a CDuce toplevel <target> with the OCaml primitives listed in \
     <prims> included.\n\n\
     Options:"
    Sys.argv.(0)

exception Error of string

let error fmt = Format.ksprintf (fun s -> raise (Error s)) fmt

exception Syntax of string

let compatible_ext s =
  if !native then
    Filename.check_suffix s ".cmx" || Filename.check_suffix s ".cmxa"
  else Filename.check_suffix s ".cmo" || Filename.check_suffix s ".cma"

let gen_objects is_native base =
  let exts = ".cmi" :: (if is_native then [ ".cmx"; ".o" ] else [ ".cmo" ]) in
  List.map (fun ext -> base ^ ext) exts

let basename s =
  if String.contains s '.' then Filename.remove_extension s else s

let main () =
  let () = Arg.parse specs set_args usage in
  if !target = "" then raise (Syntax "missing target toplevel name");
  if !prim_file = "" then raise (Syntax "missing primitive file name");
  let compiler, predicate, pstr =
    if !native then ("ocamlopt", "native", "native")
    else ("ocamlc", "byte", "bytecode")
  in
  let ocamlfind =
    Printf.sprintf "%s query -predicates %s -format " !ocamlfind_prog predicate
  in
  let include_flags =
    List.flatten
    @@ List.map
         (function
           | `I s -> [ "-I"; s ]
           | `P s ->
               List.flatten
               @@ List.map
                    (fun s -> [ "-I"; s ])
                    (run_cmd @@ ocamlfind ^ "%d " ^ s)
           | _ -> [])
         !link_flags
  in
  let has_ocamliface =
    List.exists
      (fun s ->
        String.length s >= 7 && String.equal "- ocaml" (String.sub s 0 7))
      (run_cmd @@ !cduce_prog ^ " -v")
  in
  let () =
    if not has_ocamliface then
      error "CDuce compiler `%s' was compiled without the OCaml/CDuce interface"
        !cduce_prog
  in
  let packages_flags =
    List.flatten
    @@ List.map
         (function
           | `P s -> [ s ]
           | _ -> [])
         ([
            `P "cduce.lib";
            `P "cduce.lib.ocamliface";
            `P "ocaml-compiler-libs.common";
          ]
         @ !link_flags)
  in
  let objects =
    List.flatten
    @@ List.map
         (function
           | `L s ->
               if compatible_ext s then [ s ]
               else
                 raise
                   (Error
                      (Printf.sprintf "file '%s' is incompatible in %s mode" s
                         pstr))
           | _ -> [])
         !link_flags
  in
  let base = basename !prim_file in
  let generated_objects = gen_objects !native base in
  let () =
    match List.find Sys.file_exists (!target :: generated_objects) with
    | s -> error "file '%s' already exists, please remove it" s
    | exception Not_found -> ()
  in
  let str_i_flags = String.concat " " include_flags in
  let cmd_argv =
    Array.concat
      [
        [| !ocamlfind_prog; compiler; "-o"; !target; "-linkpkg" |];
        Array.of_list include_flags;
        [| "-package"; String.concat "," packages_flags |];
        Array.of_list objects;
        [|
          "-pp";
          !cduce_prog ^ " " ^ str_i_flags ^ " --topstub";
          "-impl";
          !prim_file;
        |];
      ]
  in
  let pid =
    Unix.create_process_env !ocamlfind_prog cmd_argv (Unix.environment ())
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, result = Unix.waitpid [] pid in
  let () =
    List.iter
      (fun f -> if Sys.file_exists f then Sys.remove f)
      generated_objects
  in
  match result with
  | Unix.WEXITED code -> exit code
  | Unix.WSIGNALED s
  | Unix.WSTOPPED s ->
      error "`%s' was abborted with signal %d" !ocamlfind_prog s

let () =
  try main () with
  | Syntax msg ->
      Printf.eprintf "Error: %s\nusage: " msg;
      Arg.usage specs usage;
      exit 1
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 2
