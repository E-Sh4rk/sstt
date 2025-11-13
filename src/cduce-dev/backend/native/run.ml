open Cduce_core

let () = Stats.gettimeofday := Unix.gettimeofday
let external_init = ref None
let out_dir = ref [] (* directory of the output file *)
let src = ref []
let args = ref []
let compile = ref false
let run = ref false
let script = ref false
let mlstub = ref false
let topstub = ref false
let binarystub = ref false
let color = ref true
let prettify = ref false
let ppf = Format.std_formatter
let ppf_err = Format.err_formatter

let version () =
  Format.fprintf ppf "CDuce, version %s@." Version.cduce_version;
  Format.fprintf ppf "Using OCaml %s compiler@." Version.ocaml_compiler;
  Format.fprintf ppf "Supported features: @.";
  List.iter
    (fun (n, d) -> Format.fprintf ppf "- %s: %s@." n d)
    (Cduce_config.descrs ());
  exit 0

let parse_argv () =
  let usage_msg =
    "Usage:\ncduce [OPTIONS ...] [FILE ...] [--arg argument ...]\n\nOptions:"
  in
  let help = ref (fun () -> assert false) in
  let specs =
    Arg.align
      ([
        ("--compile", Arg.Set compile, "  compile the given CDuce file");
        ("-c", Arg.Set compile, "  same as --compile");
        ("--run", Arg.Set run, "  execute the given .cdo files");
        ( "--verbose",
          Arg.Set Cduce_driver.verbose,
          "  show types of exported values (for --compile)" );
        ( "--obj-dir",
          Arg.String (fun s -> out_dir := s :: !out_dir),
          "  directory for the compiled .cdo file (for --compile)" );
        ( "-I",
          Arg.String (fun s -> let s = if Filename.check_suffix s ".cmi" then Filename.dirname s else s in 
                       Cduce_loc.add_to_obj_path s),
          "  add one directory to the lookup path for .cdo/.cmi and include \
           files" );
        ( "--stdin",
          Arg.Unit (fun () -> src := "" :: !src),
          "  read CDuce script on standard input" );
        ( "--arg",
          Arg.Rest (fun s -> args := s :: !args),
          "  following arguments are passed to the CDuce program" );
        ( "--script",
          Arg.Rest
            (fun s ->
               if not !script then (
                 script := true;
                 src := s :: !src)
               else args := s :: !args),
          "  the first argument after is the source, then the arguments" );
        ( "--no",
          Arg.String Cduce_config.inhibit,
          "  disable a feature (cduce -v to get a list of features)" );
        ( "--debug",
          Arg.Unit (fun () -> Stats.set_verbosity Stats.Summary),
          "  print profiling/debugging information" );
        ( "--no-color",
          Arg.Clear color,
          "  disable ansi colors for cduce messages" );
        ( "--prettify",
          Arg.Set prettify,
          "  enable prettifying for cduce messages. If your output doesn't \
           support utf-8 encoding you are strongly advised to not use this \
           flag." );
        ( "-v",
          Arg.Unit version,
          "  print CDuce version, and list built-in optional features" );
        ( "--version",
          Arg.Unit version,
          "  print CDuce version, and list built-in optional features" );
        ( "--mlstub",
          Arg.Set mlstub,
          " produce stub ML code from a compiled unit" );
        ( "--topstub",
          Arg.Set topstub,
          "  produce stub ML code for a toplevel from a primitive file" );
        ( "--binarystub",
          Arg.Set binarystub,
          "  output stub ML code in binary format (default text format)" );
        ( "-help",
          Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'")),
          "" );
      ]
        @ !Cduce_driver.extra_specs
        @ [
          ("-h", Arg.Unit (fun () -> !help ()), "  display this list of options");
          ( "--help",
            Arg.Unit (fun () -> !help ()),
            "  display this list of options" );
        ])
  in
  (help :=
     fun () ->
       Arg.usage specs usage_msg;
       exit 0);
  Arg.parse specs (fun s -> src := s :: !src) usage_msg

let err s =
  Format.fprintf ppf_err "@{<fg_red; prettify>%s@}@." s;
  exit 1

let update_terminal_width () =
  try
    let cols =
      if Unix.isatty Unix.stdout && Sys.file_exists "/dev/tty" then
        let ic = Unix.open_process_in "stty -F /dev/tty size" in
        try
          int_of_string (List.nth (String.split_on_char ' ' (input_line ic)) 1)
        with
        | _ -> 80
      else 8192
      (* output is a file, set large lines *)
    in
    Format.set_margin cols;
    Format.set_max_indent (cols - 10)
  with
  | _ -> ()

let ansi_term () =
  match Sys.getenv "TERM" with
  | exception Not_found -> false
  | "dumb"
  | _ ->
    true

let ansi_terminal l = if !color && ansi_term () then `ANSI :: l else l
let prettify l = if !prettify && ansi_term () then `UTF8 :: l else []

let setup_term_watcher () =
  if Sys.os_type <> "Unix" && Sys.os_type <> "Cygwin" then ()
  else
    let restore old_h = Sys.set_signal 28 old_h in
    try
      let old =
        Sys.signal 28 (Signal_handle (fun _ -> update_terminal_width ()))
      in
      at_exit (fun () -> restore old)
    with
    | _ -> ()

let mode () =
  if !mlstub then
    match !src with
    | [ x ] -> `Mlstub x
    | _ -> err "Please specify one .cdo file"
  else if !topstub then
    match !src with
    | [ x ] -> `Topstub x
    | _ -> err "Please specify one primitive file"
  else
    match (!compile, !out_dir, !run, !src, !args) with
    | false, _ :: _, _, _, _ ->
      err "--obj-dir option can be used only with --compile"
    | false, [], false, [], args -> `Toplevel args
    | false, [], false, [ x ], args -> `Script (x, args)
    | false, [], false, _, _ ->
      err "Only one CDuce program can be executed at a time"
    | true, [ o ], false, [ x ], [] -> `Compile (x, Some o)
    | true, [], false, [ x ], [] -> `Compile (x, None)
    | true, [], false, [], [] ->
      err "Please specify the CDuce program to be compiled"
    | true, [], false, _, [] ->
      err "Only one CDuce program can be compiled at a time"
    | true, _, false, _, [] -> err "Please specify only one output directory"
    | true, _, false, _, _ ->
      err "No argument can be passed to programs at compile time"
    | false, _, true, [ x ], args -> `Run (x, args)
    | false, _, true, [], _ ->
      err "Please specifiy the CDuce program to be executed"
    | false, _, true, _, _ ->
      err "Only one CDuce program can be executed at a time"
    | true, _, true, _, _ ->
      err "The options --compile and --run are incompatible"

let bol = ref true

let outflush s =
  output_string stdout s;
  flush stdout

let has_newline b =
  let rec loop i found =
    if i >= 1 then
      let c = Buffer.nth b i in
      if c == ';' && Buffer.nth b (i - 1) == ';' then found
      else loop (i - 1) (c == '\n')
    else false
  in
  loop (Buffer.length b - 1) false

let toploop () =
  let restore =
    if Sys.win32 then fun () -> ()
    else
      try
        let tcio = Unix.tcgetattr Unix.stdin in
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { tcio with Unix.c_vquit = '\004' };
        fun () -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio
      with
      | Unix.Unix_error (_, _, _) -> fun () -> ()
  in
  let quit () =
    outflush "\n";

    restore ();
    exit 0
  in
  Format.fprintf ppf "        CDuce version %s\n@." Version.cduce_version;
  if not Sys.win32 then
    Sys.set_signal Sys.sigquit (Sys.Signal_handle (fun _ -> quit ()));
  Sys.catch_break true;
  Cduce_driver.toplevel := true;
  Librarian.run_loaded := true;
  let buf_in = Buffer.create 1024 in
  let read _i =
    if !bol then if !Sedlexer.in_comment then outflush "* " else outflush "> ";
    try
      let c = input_char stdin in
      Buffer.add_char buf_in c;
      bol := c = '\n';
      Some c
    with
    | Sys.Break -> quit ()
  in
  let input = Parse.seq_of_fun read in
  let rec loop () =
    outflush "# ";
    bol := false;
    Buffer.clear buf_in;
    ignore (Cduce_driver.topinput ppf ppf_err input);
    if not (has_newline buf_in) then
      (* ";;\n" was eaten by a regular expression in the lexer *)
      while input_char stdin != '\n' do
        ()
      done;
    loop ()
  in
  (try loop () with
   | End_of_file -> ());
  restore ()

let main () =
  parse_argv ();
  let assoc =
    [
      (* Euclidian Products *)
      ("->", Pretty_utf8.create 0x2192 (* → *));
      ("Empty", Pretty_utf8.create 0x01D7D8 (* 𝟘 *));
      ("Any", Pretty_utf8.create 0x01D7D9 (* 𝟙 *));
      ("&", Pretty_utf8.create 0x0022C2 (* ⋂ *));
      ("|", Pretty_utf8.create 0x0022C3 (* ⋃ *));
      (* Type Variables *)
      ("'a", Pretty_utf8.create 0x0003B1 (* α *));
      ("'b", Pretty_utf8.create 0x0003B2 (* β *));
      ("'c", Pretty_utf8.create 0x0003B3 (* γ *));
      ("'d", Pretty_utf8.create 0x0003B4 (* δ *));
      ("'e", Pretty_utf8.create 0x0003B5 (* ε *));
      ("'f", Pretty_utf8.create 0x0003B6 (* ζ *));
      ("'g", Pretty_utf8.create 0x0003B7 (* η *));
      ("'h", Pretty_utf8.create 0x0003B7 (* η *));
      ("'i", Pretty_utf8.create 0x0003B8 (* θ *));
      ("'j", Pretty_utf8.create 0x0003B9 (* ι *));
      ("'k", Pretty_utf8.create 0x0003BA (* κ *));
      ("'l", Pretty_utf8.create 0x0003BB (* λ *));
    ]
  in

  List.iter (fun (s, sym) -> Pretty_utf8.register_utf8_binding s sym) assoc;
  let stylings = [] |> ansi_terminal |> prettify in
  Terminal_styling.set_formatter stylings Format.std_formatter;
  Terminal_styling.set_formatter stylings Format.err_formatter;
  at_exit (fun () -> Stats.dump Format.std_formatter);
  let m = mode () in
  (* May call Cduce_config.inhibit while parsing the command line *)
  let () =
    match !external_init with
    | Some f
      when List.exists (fun (n, _) -> n = "ocaml") (Cduce_config.descrs ()) ->
      f () (* calls Cduce_config.init_all ()*)
    | _ -> Cduce_config.init_all ()
  in
  let () =
    update_terminal_width ();
    setup_term_watcher ()
  in
  try
    match m with
    | `Toplevel args ->
      Cduce_driver.set_argv args;
      toploop ()
    | `Script (f, args) ->
      Cduce_driver.set_argv args;
      Cduce_driver.compile_run f
    | `Compile (f, o) -> Cduce_driver.compile f o
    | `Run (f, args) ->
      Cduce_driver.set_argv args;
      Cduce_driver.run f
    (* TODO: handle exceptions raised by mlstub *)
    | `Mlstub f -> Librarian.prepare_stub !binarystub f
    | `Topstub f -> !Librarian.make_wrapper !binarystub f
  with
    Cduce_error.Error (loc, e) ->
    Cduce_error.print_error_loc Format.err_formatter loc e;
    Format.pp_print_flush Format.err_formatter ();
    exit 1
  | e ->
    let bt = Printexc.get_backtrace () in
    Format.eprintf "Internal error: %s\nBacktrace:\n%s\n%!"
      (Printexc.to_string e) bt;
    exit 2