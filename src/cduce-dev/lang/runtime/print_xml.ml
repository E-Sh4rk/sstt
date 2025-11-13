(* Print XML documents *)

(* The write_*_function are inspired from Pxp_aux.ml *)
(*
open Netconversion

net conversion does really fast string copying, but does not
do anything fancy Unicode-wise. We only use it for
utf-8 → iso8859-1

it reads the code point in utf-8 and writes it as
iso8859-1 if <= 255, otherwise calls subst.

*)
let convert
    ~(in_enc : [ `Enc_utf8 ])
    ~(out_enc : [ `Enc_iso88591 | `Enc_ascii ])
    ~(subst : int -> string)
    ~(range_pos : int)
    ~(range_len : int)
    s =
  let buff = Buffer.create (range_len lsl 2) in
  let open Encodings in
  let in_s = Utf8.mk s in
  let last = Utf8.mk_idx (range_pos + range_len) in
  let rec loop idx =
    if idx >= last then Buffer.contents buff
    else
      let code_point, nidx = Utf8.next in_s idx in
      let () =
        if code_point > 127 then Buffer.add_string buff (subst code_point)
        else Buffer.add_char buff (Char.unsafe_chr code_point)
      in
      loop nidx
  in
  loop (Utf8.mk_idx range_pos)

let write_markup_string ~to_enc buf s =
  let s' =
    match to_enc with
    | `Enc_utf8 -> s
    | `Enc_iso88591 as to_enc ->
      convert ~in_enc:`Enc_utf8 ~out_enc:to_enc ~range_pos:0
        ~range_len:(String.length s)
        ~subst:(fun n ->
            failwith ("Cannot represent code point " ^ string_of_int n))
        s
  in
  buf s'

let write_data_string ~to_enc buf s =
  let write_part i len =
    if len > 0 then
      match to_enc with
      | `Enc_utf8 -> buf (String.sub s i len)
      | `Enc_iso88591 ->
        let s' =
          convert ~in_enc:`Enc_utf8 ~out_enc:`Enc_ascii
            ~subst:(fun n -> "&#" ^ string_of_int n ^ ";")
            ~range_pos:i ~range_len:len s
        in
        buf s'
  in
  let i = ref 0 in
  for k = 0 to String.length s - 1 do
    match s.[k] with
    | ('&' | '<' | '>' | '"' | '%') as c ->
      write_part !i (k - !i);
      begin
        match c with
        | '&' -> buf "&amp;"
        | '<' -> buf "&lt;"
        | '>' -> buf "&gt;"
        | '"' -> buf "&quot;"
        | '%' -> buf "&#37;" (* reserved in DTDs *)
        | _ -> assert false
      end;
      i := k + 1
    | _ -> ()
  done;
  write_part !i (String.length s - !i)

(*************)

open Value
open Ident
module U = Encodings.Utf8
module H = Hashtbl.Make (Ns.Uri)

let exn_print_xml =
  CDuceExn
    (pair
       (Atom (AtomSet.V.mk_ascii "Invalid_argument"))
       (string_latin1 "print_xml"))

let blank = U.mk " "
let true_literal = U.mk "true"
let false_literal = U.mk "false"

(* @raise exn_print_xml in case of failure. Rationale: schema printing is
 * the last attempt to print a value, others have already failed *)
let rec schema_value ?(recurs = true) ~wds ~wcs v =
  match v with
  | Abstract ("float", o) -> wds (U.mk (string_of_float (Obj.magic o : float)))
  | Abstract ("cdata", o) ->
    wcs (U.mk "<![CDATA[");
    wcs (U.mk (U.get_str (Obj.magic o : U.t)));
    wcs (U.mk "]]>")
  | Record _ as v -> (
      try wds (Schema_builtin.string_of_time_type (Value.get_fields v)) with
      | Cduce_error.Error (_, (Schema_builtin_Error, b)) -> raise exn_print_xml)
  | Integer i -> wds (U.mk (Intervals.V.to_string i))
  | v when Value.equal v Value.vtrue -> wds true_literal
  | v when Value.equal v Value.vfalse -> wds false_literal
  | Pair _ as v when recurs -> schema_values ~wds ~wcs v
  | (String_utf8 _ | String_latin1 _) as v -> wds (fst (get_string_utf8 v))
  | _ -> raise exn_print_xml

and schema_values ~wds ~wcs v =
  match v with
  | Pair { fst = hd; snd = Atom a; concat = false }
    when a = Types.Sequence.nil_atom ->
    schema_value ~recurs:false ~wds ~wcs hd
  | Pair { fst = hd; snd = tl; concat = false } ->
    schema_value ~recurs:false ~wds ~wcs hd;
    wds blank;
    schema_values ~wds ~wcs tl
  | _ -> raise exn_print_xml

let to_buf ~utf8 buffer ns_table v subst =
  let to_enc = if utf8 then `Enc_utf8 else `Enc_iso88591 in

  let printer = Ns.Printer.printer ns_table in

  let wms = write_markup_string ~to_enc buffer
  and wds s = write_data_string ~to_enc buffer (U.get_str s)
  and wcs s = buffer (U.get_str s) in
  let write_att (n, v) =
    wms (" " ^ Ns.Printer.attr printer (Label.value n) ^ "=\"");
    wds v;
    wms "\""
  in
  let write_xmlns (pr, ns) =
    let pr = U.get_str pr in
    if pr = "" then wms " xmlns"
    else (
      wms " xmlns:";
      wms pr);
    wms "=\"";
    wds (Ns.Uri.value ns);
    wms "\""
  in

  let element_start q xmlns attrs =
    wms ("<" ^ Ns.Printer.tag printer (AtomSet.V.value q));
    List.iter write_xmlns xmlns;
    List.iter write_att attrs;
    wms ">"
  and empty_element q xmlns attrs =
    wms ("<" ^ Ns.Printer.tag printer (AtomSet.V.value q));
    List.iter write_xmlns xmlns;
    List.iter write_att attrs;
    wms "/>"
  and element_end q =
    wms ("</" ^ Ns.Printer.tag printer (AtomSet.V.value q) ^ ">")
  and document_start () =
    ()
    (*wms ("<?xml version='1.0' encoding='" ^
       (match to_enc with `Enc_utf8 -> "UTF-8" | `Enc_iso88591 -> "ISO-8859-1")^
       "'?>\n")*)
  in

  let rec register_elt = function
    | Xml (Atom q, Record attrs, content)
    | XmlNs (Atom q, Record attrs, content, _) ->
      Imap.iter
        (fun n _ ->
           Ns.Printer.register_qname printer (Label.value (Label.from_int n)))
        attrs;
      Ns.Printer.register_qname printer (AtomSet.V.value q);
      register_content content
    | _ -> ()
  and register_content = function
    | String_utf8 { tl = q; _ }
    | String_latin1 { tl = q; _ } ->
      register_content q
    | Pair { fst = x; snd = q; concat = false } ->
      register_elt x;
      register_content q
    | Pair { fst = x; snd = y; concat = true } ->
      register_content x;
      register_content y
    | _ -> ()
  in
  register_elt v;

  let rec print_elt xmlns = function
    | Xml (Atom tag, Record attrs, content)
    | XmlNs (Atom tag, Record attrs, content, _) -> (
        let attrs =
          Imap.map_elements
            (fun n v ->
               if is_str v then
                 let s, q = get_string_utf8 v in
                 match q with
                 | Atom a when a = Types.Sequence.nil_atom ->
                   (Label.from_int n, s)
                 | _ -> raise exn_print_xml
               else
                 let buf = Buffer.create 20 in
                 let wds s = Buffer.add_string buf (U.get_str s) in
                 schema_value ~wds ~wcs:wds v;
                 (Label.from_int n, U.mk (Buffer.contents buf)))
            attrs
        in
        match content with
        | Atom a when a = Types.Sequence.nil_atom ->
          empty_element tag xmlns attrs
        | _ ->
          element_start tag xmlns attrs;
          print_content content;
          element_end tag)
    | _ -> raise exn_print_xml
  and print_content v =
    let s, q = get_string_utf8 v in
    wds s;
    match q with
    | Pair { fst = (Xml _ | XmlNs _) as x; snd = q; concat = false } ->
      print_elt [] x;
      print_content q
    | Atom a when a = Types.Sequence.nil_atom -> ()
    | v -> schema_value ~wds ~wcs v
  in
  let uri_subst prefixes replace =
    let h = H.create 10 in
    List.iter (fun (k, v) -> H.replace h k v) replace;
    List.map
      (fun (pr, ns) -> if H.mem h ns then (pr, H.find h ns) else (pr, ns))
      prefixes
  in
  document_start ();
  match subst with
  | [] -> print_elt (Ns.Printer.prefixes printer) v
  | _ -> print_elt (uri_subst (Ns.Printer.prefixes printer) subst) v

let print_xml ~utf8 ns_table s =
  let buf = Buffer.create 32 in
  to_buf ~utf8 (Buffer.add_string buf) ns_table s [];
  let s = Buffer.contents buf in
  if utf8 then string_utf8 (U.mk s) else string_latin1 s

let print_xml_subst ~utf8 ns_table s subst =
  let buf = Buffer.create 32 in
  to_buf ~utf8 (Buffer.add_string buf) ns_table s subst;
  let s = Buffer.contents buf in
  if utf8 then string_utf8 (U.mk s) else string_latin1 s

let dump_xml ~utf8 ns_table s =
  to_buf ~utf8 print_string ns_table s [];
  Value.nil
