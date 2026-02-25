open Js_of_ocaml
open Sstt_repl
open Output

module Html = Dom_html

let buf = Buffer.create 256
let env = ref Repl.empty_env

let treat_elt elt =
  let env' = Repl.treat_elt !env elt in
  env := env'
let treat str =
  try IO.parse_program str |> List.iter treat_elt
  with e -> print Error "%s" (Printexc.to_string e)

let send line =
  let line = Js.to_string line in
  Buffer.add_string buf line ;
  let str = Buffer.contents buf |> String.trim in
  if String.ends_with ~suffix:";;" str then begin
    Buffer.clear buf ;
    let out = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer out in
    with_basic_output fmt treat str ;
    Format.fprintf fmt "@?" ;
    Js.string (Buffer.contents out) |> Js.some
  end else begin
    Buffer.add_string buf "\n" ;
    Js.null
  end

let _ =
  Js.export "sstt"
    (object%js
       method send line = send line
       method commit = Version.commit |> Js.string
       method version = Version.version |> Js.string
       method compiler = Version.compiler |> Js.string
     end)
