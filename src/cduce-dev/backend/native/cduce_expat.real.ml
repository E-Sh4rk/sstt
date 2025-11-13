open Cduce_core

let buflen = 1024
let buf = Bytes.create buflen

let load_from_file p s =
  let ic =
    try open_in s with
    | exn ->
        let msg =
          Printf.sprintf "load_xml, file \"%s\": %s" s (Printexc.to_string exn)
        in
        Value.failwith' msg
  in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n > 0 then (
      Expat.parse_sub_bytes p buf 0 n;
      loop ())
  in
  try
    loop ();
    Expat.final p;
    close_in ic
  with
  | exn ->
      close_in ic;
      raise exn

let rec push p s =
  Expat.set_external_entity_ref_handler p (fun ctx _base sys _pub ->
      let s = Url.local s sys in
      let p = Expat.external_entity_parser_create p ctx None in
      push p s);
  try
    if Url.is_url s then begin
      let content = Url.load_url s in
      Expat.parse p content
    end
    else load_from_file p s
  with
  | Expat.Expat_error e ->
      let msg =
        Printf.sprintf "load_xml,%s at line %i, column %i: %s" s
          (Expat.get_current_line_number p)
          (Expat.get_current_column_number p)
          (Expat.xml_error_to_string e)
      in
      Value.failwith' msg

let load_expat se ee txt s =
  let p = Expat.parser_create ~encoding:None in
  Expat.set_start_element_handler p se;
  Expat.set_end_element_handler p ee;
  Expat.set_character_data_handler p txt;
  ignore (Expat.set_param_entity_parsing p Expat.NEVER);
  push p s

let use () =
  Load_xml.xml_parser :=
    load_expat Load_xml.start_element_handler Load_xml.end_element_handler
      Load_xml.text_handler;
  Schema_xml.xml_parser :=
    fun uri f g -> load_expat f (fun _ -> g ()) (fun _ -> ()) uri

let () = Cduce_config.register "expat" "Expat XML parser" use
