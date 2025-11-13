open Printf

let time_cmd = "/usr/bin/time"
let cduce_cmd = "../cduce"
let xduce_cmd = "/home/frisch/xduce-0.4.0/xduce.opt"
let xduce_024_cmd = "/home/frisch/xduce-0.2.4/xduce.opt"
let xslt_cmd = "xsltproc"

let rec split c s =
  try
    let i = String.index s c in
    String.sub s 0 i :: split c (String.sub s (i + 1) (String.length s - i - 1))
  with
  | Not_found -> [ s ]

let has_prefix pre s =
  let ls = String.length s in
  let lpre = String.length pre in
  ls >= lpre && String.sub s 0 lpre = pre

let extract_prefix pre s =
  let ls = String.length s in
  let lpre = String.length pre in
  if ls >= lpre && String.sub s 0 lpre = pre then String.sub s lpre (ls - lpre)
  else failwith "Invalid string"

let has_suffix pre s =
  let ls = String.length s in
  let lpre = String.length pre in
  ls >= lpre && String.sub s (ls - lpre) lpre = pre

let name, args, scripts =
  match Array.to_list Sys.argv with
  | _ :: name :: args :: scripts -> (name, args, scripts)
  | _ ->
      Printf.eprintf "Please specify bench name and size list";
      exit 1

let scripts ext = List.filter (has_suffix ext) scripts
let gen = name ^ ".ml"
let xml = name
let args = List.map int_of_string (split ',' args)
let sp = sprintf

let langs =
  [
    (* "CDuce PXP", ".cd",
       (fun script xml ->
          sp "%s --pxp --quiet %s --arg %s" cduce_cmd script xml); *)
    ( "CDuce",
      ".cd",
      fun script xml -> sp "%s %s --no ocaml --arg %s" cduce_cmd script xml );
    (* "CDuce.old", ".cd",
       (fun script xml ->
          sp "%s --quiet %s --arg %s" (cduce_cmd^".old") script xml);*)
    ("XDuce 0.4.0", ".q", fun script xml -> sp "%s %s %s" xduce_cmd script xml);
    ( "XDuce 0.2.4",
      ".q",
      fun script xml -> sp "%s %s %s" xduce_024_cmd script xml );
    ( "XDuce 0.2.4 - patopt",
      ".q",
      fun script xml -> sp "%s %s -patopt %s" xduce_024_cmd script xml );
    ("XSLT", ".xsl", fun script xml -> sp "%s --noout %s %s" xslt_cmd script xml);
  ]

let pr = printf
let rep = 3

let time s =
  let s = sp "%s -p %s 2>&1" time_cmd s in
  (*  pr "Running: %s\n" s;  *)
  flush stdout;
  for i = 1 to rep do
    let ic = Unix.open_process_in s in
    let real = input_line ic in
    let user = input_line ic in
    let sys = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 ->
        pr "%s,%s " (extract_prefix "user " user) (extract_prefix "real " real);
        flush stdout
    | _ -> pr "err"
  done;
  pr "\n"

let run s =
  flush stdout;
  ignore (Sys.command s)

let () =
  List.iter
    (fun s ->
      let fn = sp "%s.%i.xml" xml s in
      if not (Sys.file_exists fn) then run (sp "ocaml %s %i > %s" gen s fn);
      let ic = open_in fn in
      let size = in_channel_length ic in
      close_in ic;
      pr "XML size = %i; records = %i\n" size s;
      List.iter
        (fun (lang, ext, cmd) ->
          List.iter
            (fun file ->
              pr "%20s[%20s] " lang file;
              time (cmd file fn))
            (scripts ext))
        langs;
      pr "====================================\n")
    args
