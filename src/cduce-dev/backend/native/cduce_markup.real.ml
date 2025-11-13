open Markup
open Cduce_core

let ends_with s pat =
  let ls = String.length s in
  let lpat = String.length pat in
  ls >= lpat && pat = String.sub s (ls - lpat) lpat

let markup_load_xml otag ctag text s =
  let stream, close =
    if Url.is_url s then (string (Url.load_url s), ignore) else file s
  in
  let resolver = ref (fun _ -> None) in
  let () = if ends_with s ".xhtml" then resolver := xhtml_entity in
  let parser =
    parse_xml
      ~entity:(fun s ->
          match !resolver s with
          | None -> Some ""
          | x -> x)
      ~report:(fun location e ->
          Value.failwith'
            (Format.sprintf "load_xml: '%s': %s" s (Error.to_string ~location e)))
      ~context:`Document stream
  in
  iter
    (fun signal ->
       match signal with
       | `Start_element ((_, tag) as name, atts) ->
         if tag = "xhtml" then resolver := xhtml_entity;
         otag name atts
       | `End_element -> ctag ""
       | `Text ls -> List.iter text ls
       | _ -> ())
    (signals parser);
  close ()

let markup_load_html otag ctag text s =
  let stream, close =
    if Url.is_url s then (string (Url.load_url s), ignore) else file s
  in
  let parser = parse_html ~report:(fun _ _ -> ()) ~context:`Document stream in
  iter
    (fun signal ->
       match signal with
       | `Start_element ((_, tag), atts) ->
         otag tag (List.map (fun ((_, tag), v) -> (tag, v)) atts)
       | `End_element -> ctag ""
       | `Text ls -> List.iter text ls
       | _ -> ())
    (signals parser);
  close ()

let use () =
  let open Load_xml in
  xml_parser :=
    markup_load_xml start_element_handler_resolved_ns end_element_handler text_handler;
  html_loader :=
    fun s ->
      Value.sequence
        [
          (mk_load_xml
             (markup_load_html start_element_handler end_element_handler
                text_handler)
             ~ns:true)
            s;
        ]

let () =
  Cduce_config.register ~priority:2 "markup" "Markup.ml XML and HTML parser" use
