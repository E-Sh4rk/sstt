open Cduce_types
open Cduce_core
open Value
open Ident
open Load_xml
module U = Encodings.Utf8

let load_html s =
  let rec val_of_doc q = function
    | Nethtml.Data data ->
        if only_ws (Bytes.unsafe_of_string data) (String.length data) then q
        else string data q
    | Nethtml.Element (tag, att, child) ->
        let att =
          List.map (fun (n, v) -> (Label.mk (Ns.empty, U.mk n), U.mk v)) att
        in
        pair
          (elem Ns.empty_table
             (AtomSet.V.mk (Ns.empty, U.mk tag))
             att (val_of_docs child))
          q
  and val_of_docs = function
    | [] -> nil
    | h :: t -> val_of_doc (val_of_docs t) h
  in

  let parse src = Nethtml.parse_document ~dtd:Nethtml.relaxed_html40_dtd src in
  let doc =
    if Url.is_url s then parse (Lexing.from_string (Url.load_url s))
    else
      let ic = open_in s in
      let doc =
        try parse (Lexing.from_channel ic) with
        | exn ->
            close_in ic;
            raise exn
      in
      close_in ic;
      doc
  in
  let doc = Nethtml.decode ~subst:(fun _ -> "???") doc in
  let doc =
    Nethtml.map_list
      (Netconversion.convert ~in_enc:`Enc_iso88591 ~out_enc:`Enc_utf8)
      doc
  in
  val_of_docs doc

let use () = Load_xml.html_loader := load_html

let () =
  Cduce_config.register ~priority:~-1 "netstring"
    "Load HTML document with netstring" use
