open Js_of_ocaml
open Cduce_core

let load_xml_html mimetype open_cb close_cb text_cb txt =
  let _DOMParser : (unit -> < .. > Js.t) Js.constr Js.optdef =
    Js.Unsafe.(get global (Js.string "DOMParser"))
  in
  let _DOMParser =
    Js.Optdef.get _DOMParser (fun () ->
        Value.failwith' "DOMParser not available")
  in
  let parser = new%js _DOMParser () in
  let txt = Js.string txt in
  let mime_type = Js.string mimetype in
  let dom : Dom.element Dom.document Js.t =
    Js.Unsafe.(
      meth_call parser "parseFromString" [| inject txt; inject mime_type |])
  in
  let root = dom##.documentElement in
  let name = Js.to_string root##.tagName in
  if name = "parsererror" then Value.failwith' "Invalid document element"
  else
    let rec loop node =
      match Js.Opt.to_option node with
      | None -> ()
      | Some n -> (
          let fs = n##.firstChild in
          let ns = n##.nextSibling in
          match Dom.nodeType n with
          | Dom.Text text_node ->
              text_cb (Js.to_string text_node##.data);
              loop fs;
              loop ns
          | Dom.Element elem_node ->
              let alist = ref [] in
              for i = 0 to elem_node##.attributes##.length - 1 do
                let an =
                  Js.Opt.get
                    (elem_node##.attributes##item i)
                    (fun () -> assert false)
                in
                alist :=
                  (Js.to_string an##.name, Js.to_string an##.value) :: !alist
              done;
              open_cb (Js.to_string elem_node##.tagName) !alist;
              loop fs;
              loop ns;
              close_cb (Js.to_string elem_node##.tagName)
          | _ ->
              loop fs;
              loop ns)
    in
    loop (Js.Opt.return (root :> Dom.node Js.t))

let get_fun = function
  | Value.Abstraction (_, f, _) -> f
  | _ -> assert false

let load_url async url cb_ok cb_err =
  let xhr = XmlHttpRequest.create () in
  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        if xhr##.readyState == DONE then
          let text =
            match Js.Opt.to_option xhr##.responseText with
            | None -> ""
            | Some s -> Js.to_string s
          in
          if xhr##.status == 200 then cb_ok text else cb_err text);
  xhr##_open (Js.string "get") (Js.string url) (Js.bool async);
  xhr##send Js.null

let use () =
  (Stats.gettimeofday := fun () -> Js.to_float (Js.date##now) /. 1000.);

  (* Url loading *)
  (Url.url_loader :=
     fun url ->
       let content = ref "" in
       load_url false url
         (fun s -> content := s)
         (fun _ -> Value.failwith' ("Cannot load URL: " ^ url));
       !content);

  Load_xml.xml_parser :=
    fun url ->
      load_xml_html "application/xml" Load_xml.start_element_handler
        Load_xml.end_element_handler Load_xml.text_handler (Url.load_url url);

      Load_xml.html_loader :=
        Load_xml.mk_load_xml (fun url ->
            load_xml_html "text/html" Load_xml.start_element_handler
              Load_xml.end_element_handler Load_xml.text_handler
              (Url.load_url url));

      let open Cduce_types in
      let tstr = Builtin_defs.string in
      let nil = Builtin_defs.nil in
      Operators.register_fun3 "load_url_async" Builtin_defs.string
        (Types.arrow (Types.cons tstr) (Types.cons nil))
        (Types.arrow (Types.cons tstr) (Types.cons nil))
        nil
        (fun curl cok cerr ->
          let url = Encodings.Utf8.get_str (fst (Value.get_string_utf8 curl)) in
          let ok s =
            ignore ((get_fun cok) (Value.string_utf8 (Encodings.Utf8.mk s)))
          in
          let err s =
            ignore ((get_fun cerr) (Value.string_utf8 (Encodings.Utf8.mk s)))
          in
          load_url true url ok err;
          Value.nil)

let () = Cduce_config.register "jsoo" "Js_of_ocaml bindings" use
