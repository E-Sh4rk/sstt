open Js_of_ocaml

type history = {
  mutable after : string list;
  mutable before : string list;
}

let add_substring buff s i l =
  for k = 0 to l - 1 do
    match s.[i + k] with
    | '<' -> Buffer.add_string buff "&lt;"
    | '>' -> Buffer.add_string buff "&gt;"
    | c -> Buffer.add_char buff c
  done

let dump_buffer kind (div : Dom_html.divElement Js.t) buff =
  let s = Buffer.contents buff in
  let () = Buffer.reset buff in
  if s <> "" then
    let otag = "<span class='cduce-top-" ^ kind ^ "'>" in
    div##.innerHTML :=
      div##.innerHTML##concat_3
        (Js.string otag) (Js.string s) (Js.string "</span>")

let relt = new%js Js.regExp_withFlags (Js.string "[&]lt;") (Js.string "g")
let regt = new%js Js.regExp_withFlags (Js.string "[&]gt;") (Js.string "g")
let retag = new%js Js.regExp_withFlags (Js.string "<[^>]*>") (Js.string "g")
let rebr = new%js Js.regExp_withFlags (Js.string "<[bB][rR]/?>") (Js.string "g")

let display_divs
    (console : Dom_html.divElement Js.t)
    (input : Dom_html.divElement Js.t)
    (container : Dom_html.divElement Js.t) =
  if container##.clientHeight <= input##.offsetHeight + console##.scrollHeight
  then begin
    console##.style##.bottom
    := Js.string (Format.sprintf "%dpx" input##.offsetHeight);
    input##.style##.bottom := Js.string "0";
    console##.style##.top := Js.string "";
    input##.style##.top := Js.string ""
  end
  else begin
    console##.style##.top := Js.string "0";
    input##.style##.top
    := Js.string (Format.sprintf "%dpx" console##.scrollHeight);
    console##.style##.bottom := Js.string "";
    input##.style##.bottom := Js.string ""
  end

let install id =
  let id = Js.to_string id in
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.div with
  | None -> failwith ("Expecting a div element with id : " ^ id)
  | Some div ->
      let history = { after = []; before = [] } in
      let lineHeight = ref 0 in
      let container = Dom_html.createDiv Dom_html.document in
      container##.id := Js.string "cduce-top-container";
      let console = Dom_html.createDiv Dom_html.document in
      console##.id := Js.string "cduce-top-console";
      let input = Dom_html.createDiv Dom_html.document in
      input##.id := Js.string "cduce-top-input";
      input##setAttribute (Js.string "tabindex") (Js.string "0");
      input##.innerHTML := Js.string " ";
      input##setAttribute (Js.string "contenteditable") (Js.string "true");
      input##setAttribute (Js.string "role") (Js.string "textbox");
      let out_buff = Buffer.create 16 in
      let err_buff = Buffer.create 16 in
      let out_fmt = Format.make_formatter (add_substring out_buff) ignore in
      let err_fmt = Format.make_formatter (add_substring err_buff) ignore in
      (input :> Dom_html.eventTarget Js.t)##.onkeydown
      := Dom.handler (fun e ->
             let res =
               match Js.Optdef.to_option e##.key with
               | None -> Js._true
               | Some s -> (
                   let key = Js.to_string s in
                   match key with
                   | "Enter" ->
                       let s = input##.innerHTML in
                       let fields = Js.str_array (s##split (Js.string ";;")) in
                       if fields##.length > 1 || Js.to_bool e##.ctrlKey then begin
                         input##.textContent := Js.null;
                         input##.innerHTML := Js.string " ";
                         input##.style##.height
                         := (Js.string (string_of_int !lineHeight))##concat
                              (Js.string "px");
                         history.before <-
                           List.rev_append history.after history.before;
                         history.after <- [];
                         let phrase = Js.array_get fields 0 in
                         match Js.Optdef.to_option phrase with
                         | None -> Js._true
                         | Some orig_phrase ->
                             let phrase =
                               ((((orig_phrase##replace rebr (Js.string "\n"))##replace
                                    retag (Js.string ""))##replace
                                   relt (Js.string "<"))##replace
                                  regt (Js.string ">"))##trim
                             in
                             let phrase = Js.to_string phrase ^ ";;" in
                             if phrase <> ";;" then begin
                               Format.fprintf out_fmt "%s\n%!" phrase;
                               history.before <-
                                 Js.to_string orig_phrase :: history.before;
                               Cduce_lib_js.Toplevel.eval_top out_fmt err_fmt
                                 phrase;
                               dump_buffer "out" console out_buff;
                               dump_buffer "err" console err_buff;
                               Cduce_js_compat.set_scroll_top console (console##.scrollHeight)
                             end;
                             Js._false
                       end
                       else begin
                         input##.style##.height
                         := Js.string
                              (Format.sprintf "%dpx"
                                 (input##.clientHeight + !lineHeight));
                         Js._true
                       end
                   | "ArrowUp"
                   | "Up"
                     when Js.to_bool e##.ctrlKey && history.before <> [] ->
                       let l = List.hd history.before in
                       history.after <- l :: history.after;
                       history.before <- List.tl history.before;
                       input##.innerHTML := Js.string l;
                       Js._false
                   | "ArrowDown"
                   | "Down"
                     when Js.to_bool e##.ctrlKey && history.after <> [] ->
                       let l = List.hd history.after in
                       history.before <- l :: history.before;
                       history.after <- List.tl history.after;
                       input##.innerHTML := Js.string l;
                       Js._false
                   | _ -> Js._true)
             in
             display_divs console input container;
             res);
      Cduce_lib_js.Toplevel.init_top out_fmt;
      dump_buffer "out" console out_buff;
      ignore (container##appendChild (console :> Dom.node Js.t));
      ignore (container##appendChild (input :> Dom.node Js.t));
      ignore (div##appendChild (container :> Dom.node Js.t));
      display_divs console input container;
      lineHeight := input##.clientHeight;
      input##focus

let () =
  let install = Js.wrap_callback install in
  Js.export "cduce_top_install" install
