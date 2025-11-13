(* Loading XML documents *)

open Value
open Ident
open Encodings

let keep_ns = ref true

module H = Hashtbl.Make (Ns.Uri)

let subst_ns = H.create 10

let prefix_table = H.create 16
let () = 
  H.add prefix_table Ns.xml_ns (Utf8.mk "xml");
  H.add prefix_table Ns.xmlns_ns (Utf8.mk "xmlns")

let pref_id = ref 0

let get_prefix_uri uri =
  try
    H.find prefix_table uri
  with Not_found ->
    incr pref_id;
    let id = !pref_id in
    let pref = Utf8.mk (Format.sprintf "ns%d" id) in
    H.add prefix_table uri pref;
    pref


type buf = {
  mutable buffer : Bytes.t;
  mutable pos : int;
  mutable length : int;
}

let txt = { buffer = Bytes.create 1024; pos = 0; length = 1024 }

let resize txt n =
  let new_len = (txt.length * 2) + n in
  let new_buf = Bytes.create new_len in
  Bytes.unsafe_blit txt.buffer 0 new_buf 0 txt.pos;
  txt.buffer <- new_buf;
  txt.length <- new_len

let add_string txt s =
  let len = String.length s in
  let new_pos = txt.pos + len in
  if new_pos > txt.length then resize txt len;
  Bytes.unsafe_blit (Bytes.unsafe_of_string s) 0 txt.buffer txt.pos len;
  txt.pos <- new_pos

let rec only_ws s i =
  i = 0
  ||
  let i = pred i in
  match Bytes.unsafe_get s i with
  | ' '
  | '\t'
  | '\n'
  | '\r' ->
    only_ws s i
  | _ -> false

let string s q =
  let s = Utf8.mk s in
  String_utf8 { i = Utf8.start_index s; j = Utf8.end_index s; str = s; tl = q }

let attrib att =
  (* TODO: better error message *)
  let att = List.map (fun (n, v) -> (Upool.int n, string_utf8 v)) att in
  Imap.create (Array.of_list att)

let elem ns tag att child =
  if !keep_ns then XmlNs (Atom tag, Record (attrib att), child, ns)
  else Xml (Atom tag, Record (attrib att), child)

type stack =
  | Element of Value.t * stack
  | Start of
      Ns.table * AtomSet.V.t * (Ns.Label.t * Utf8.t) list * Ns.table * stack
  | String of string * stack
  | Empty

let stack = ref Empty
let ns_table = ref Ns.empty_table

let rec create_elt accu = function
  | String (s, st) -> create_elt (string s accu) st
  | Element (x, st) -> create_elt (pair x accu) st
  | Start (ns, name, att, old_table, st) ->
    stack := Element (elem ns name att accu, st);
    ns_table := old_table
  | Empty -> assert false

let start_element_handler name att =
  if not (only_ws txt.buffer txt.pos) then
    stack := String (Bytes.sub_string txt.buffer 0 txt.pos, !stack);
  txt.pos <- 0;

  let table, name, att =
    Ns.process_start_tag_subst !ns_table name att subst_ns
  in
  stack := Start (table, AtomSet.V.mk name, att, !ns_table, !stack);
  ns_table := table

let mk_qname (uri, name) = 
  let uri = Ns.Uri.mk uri in
  let pref = get_prefix_uri uri in
  pref, uri, (Utf8.to_string pref) ^ ":" ^ name

let start_element_handler_resolved_ns (uri, name) att =
  if not (only_ws txt.buffer txt.pos) then
    stack := String (Bytes.sub_string txt.buffer 0 txt.pos, !stack);
  txt.pos <- 0;
  let uri = Utf8.mk uri in
  let pref, uri, qname = mk_qname (uri, name) in
  let local_table = Ns.add_prefix pref uri !ns_table in
  let local_table, att = List.fold_left (fun (at, aa) ((uri, a), v) ->
      let pref, uri', qname =  mk_qname (Utf8.mk uri, a) in
      Ns.add_prefix pref uri' at,
      ((qname,v)::aa)
    ) (local_table, []) att
  in
  let table, name, att =
    Ns.process_start_tag_subst local_table qname (List.rev att) subst_ns
  in
  stack := Start (local_table, AtomSet.V.mk name, att, !ns_table, !stack);
  ns_table := table

let end_element_handler _ =
  let accu =
    if only_ws txt.buffer txt.pos then nil
    else string (Bytes.sub_string txt.buffer 0 txt.pos) nil
  in
  txt.pos <- 0;
  create_elt accu !stack

let text_handler = add_string txt
let xml_parser = ref (fun s -> failwith "No XML parser available")

let mk_load_xml parser ?(ns = false) s =
  try
    H.clear subst_ns;
    keep_ns := ns;
    parser s;
    match !stack with
    | Element (x, Empty) ->
      stack := Empty;
      x
    | _ -> Value.failwith' "No XML stream to parse"
  with
  | e -> (
      stack := Empty;
      txt.pos <- 0;
      match e with
      | Ns.UnknownPrefix n ->
        Value.failwith' ("Unknown namespace prefix: " ^ Utf8.get_str n)
      | e -> raise e)

let load_xml ?(ns = false) s = mk_load_xml !xml_parser ~ns s

let load_xml_subst ?(ns = false) s subst =
  H.clear subst_ns;
  List.iter (fun (k, v) -> H.replace subst_ns k v) subst;
  try
    keep_ns := ns;
    !xml_parser s;
    match !stack with
    | Element (x, Empty) ->
      stack := Empty;
      x
    | _ -> Value.failwith' "No XML stream to parse"
  with
  | e -> (
      stack := Empty;
      txt.pos <- 0;
      match e with
      | Ns.UnknownPrefix n ->
        Value.failwith' ("Unknown namespace prefix: " ^ Utf8.get_str n)
      | e -> raise e)

let html_loader =
  ref (fun _ -> Cduce_error.(raise_err Generic "load_html not implemented"))

let load_html s = !html_loader s
(*
let load_html s =
  let rec val_of_doc q = function
    | Nethtml.Data data ->
	if (only_ws (Bytes.unsafe_of_string data) (String.length data)) then q else string data q
    | Nethtml.Element (tag, att, child) ->
	let att = List.map (fun (n,v) -> (Label.mk (Ns.empty, Utf8.mk n), Utf8.mk v)) att in
	pair (elem Ns.empty_table (Atoms.V.mk (Ns.empty,Utf8.mk tag) )
		att (val_of_docs child)) q
  and val_of_docs = function
    | [] -> nil
    | h::t -> val_of_doc (val_of_docs t) h
  in

  Cduce_loc.protect_op "load_html";
  let parse src = Nethtml.parse_document ~dtd:Nethtml.relaxed_html40_dtd src in
  let doc =
    if Url.is_url s then
      parse (Lexing.from_string (Url.load_url s))
    else
      let ic = open_in s in
      let doc =
	try parse (Lexing.from_channel ic)
	with exn -> close_in ic; raise exn in
      close_in ic;
      doc
  in
  let doc = Nethtml.decode ~subst:(fun _ -> "???") doc in
  let doc = Nethtml.map_list
	      (Netconversion.convert ~in_enc:`Enc_iso88591
		 ~out_enc:`Enc_utf8) doc in
  val_of_docs doc
*)
