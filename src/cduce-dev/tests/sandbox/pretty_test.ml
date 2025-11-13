(* Experiments *)
open Cduce_types
open Cduce_lib

let () = Format.set_margin 200

(** Typing of records: *)
let parse_type str =
  match Parse.pat (String.to_seq str) with
  | exception _ ->
      Format.eprintf "@{<fg_yellow>Warning:@} Could not parse %s@." str;
      Types.empty
  | p -> Types.descr (Typer.typ Builtin.env p)

let () =
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
  Terminal_styling.set_formatter [ `UTF8 ] Format.str_formatter;
  Terminal_styling.set_formatter [ `UTF8 ] Format.err_formatter

let test s =
  let t1 = parse_type s in
  let st =
    Format.fprintf Format.str_formatter "%a" Types.Print.print t1;
    Format.flush_str_formatter ()
  in
  let t2 = parse_type st in
  if Types.equal t1 t2 then
    Format.eprintf "@{<fg_green>Passed:@} %s to %s@." s st
  else (
    Format.eprintf "@{<fg_red>Failed:@} %a and %a are different@."
      Types.Print.print t1 Types.Print.print t2;
    raise Exit)

(* -> to → *)
let () = test "Int -> Bool"

(* Empty to 𝟘 *)
let () = test "Empty"

(* Any to 𝟙 *)
let () = test "Any"

(* \\\\ to \ *)
let () = test "((Int | Bool))"

(* let t_map = parse_type "('a -> 'b) -> [ 'a * ] -> [ 'b * ]" in *)
(* let f1 = parse_type "('a -> 'a)" in *)

(* & to ⋂ *)
let () = test "Int & Bool"

(* | to ⋃ *)
let () = test "Int | Bool"

(* 'a to α *)
let () = test "'a"

(* 'b to β *)
let () = test "'b"

(* 'c to γ *)
let () = test "'c"

(* 'd to δ *)
let () = test "'d"

(* 'e to ε *)
let () = test "'e"

(* 'f to ζ *)
let () = test "'f"

(* 'g to η *)
let () = test "'g"

(* 'h to η *)
let () = test "'h"

(* 'i to θ *)
let () = test "'i"

(* 'j to ι *)
let () = test "'j"

(* 'k to κ *)
let () = test "'k"

(* 'l to λ *)
let () = test "'l"
