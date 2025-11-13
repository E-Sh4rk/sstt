(* TODO: handle encodings of the input for pretty printing
   fragments of code *)

type pos = Lexing.position

type loc = pos * pos

let nopos = Lexing.dummy_pos
let noloc = (nopos, nopos)

let source_name (pos_start, pos_end) =
  let open Lexing in
  assert (pos_start.pos_fname = pos_end.pos_fname);
  pos_start.pos_fname

(* \x00 is an invalid character in a filename, both for Posix and Windows paths *)
let stdin_source = "\x00\x00"
let toplevel_source = "\x00\x01"
let jsoo_source = "\x00\x02"

let is_dummy_source s =
  s = "" || s.[0] = '\x00'

type precise =
  [ `Full
  | `Char of int
  ]
let min_pos pos1 pos2 =
  let open Lexing in
  if pos1.pos_cnum <= pos2.pos_cnum then pos1 else pos2
let max_pos pos1 pos2 =
  let open Lexing in
  if pos1.pos_cnum >= pos2.pos_cnum then pos1 else pos2
let merge_loc ((p1s,p1e) as loc1) ((p2s, p2e) as loc2) =
  let open Lexing in
  if p1s.pos_fname = p2s.pos_fname then
    if p1s = nopos || p1e = nopos then loc2
    else if p2s = nopos || p2e = nopos then loc1
    else 
      (min_pos p1s p2s, max_pos p1e p2e)
  else loc1

(* Note: this is incorrect. Directives #utf8,... should
   not be recognized inside comments and strings !
   The clean solution is probably to have the real lexer
   count the lines. *)

let print_precise ppf = function
  | `Full -> ()
  | `Char i -> Format.fprintf ppf " (character %i of the string)" i

let line_pos pos =
  Lexing.(pos.pos_cnum - pos.pos_bol)

let print_loc ppf ((ps, pe), w) =
  let open Format in
  let open Lexing in
  let n = ps.pos_fname in
  if n = "" then ()
  else if n.[0] = '\x00' then
    fprintf ppf "Characters %i-%i%a" ps.pos_cnum pe.pos_cnum print_precise w
  else begin
    fprintf ppf "File \"%s\", line %i, " n ps.pos_lnum;
    if ps.pos_lnum = pe.pos_lnum then
      fprintf ppf "characters %i-%i" (line_pos ps) (line_pos pe)
    else
      fprintf ppf "character %i, to line %i, character %i"
        (line_pos ps) pe.pos_lnum (line_pos pe);
    fprintf ppf "%a" print_precise w
  end
type 'a located = {
  loc : loc;
  descr : 'a;
}
let mk_loc loc x = { loc; descr = x }
let mknoloc x = { loc = noloc; descr = x }
let obj_path = ref [ "" ]

let get_obj_path () = !obj_path

let add_to_obj_path s =
  if Sys.file_exists s && Sys.is_directory s &&
     not (List.mem s !obj_path)
  then
    obj_path := s :: !obj_path

let resolve_filename s =
  if Filename.is_relative s then
    try
      let p =
        List.find
          (fun p -> Sys.file_exists (Filename.concat p s))
          !obj_path
      in
      Filename.concat p s
    with
    | Not_found -> s
  else s

let warning loc msg =
  let ppf =  Format.err_formatter in
  print_loc ppf (loc, `Full);
  Format.fprintf ppf "Warning: %s@." msg
