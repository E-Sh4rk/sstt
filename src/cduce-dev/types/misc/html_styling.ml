open TextStyling

(* See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters for some values *)
let pp_html_value ppf s =
  let fg_color c =
    Format.fprintf ppf {|@[<v 0>@[<v 2><span class="color_%s;">@,|} c
  in
  let bg_color c =
    Format.fprintf ppf {|@[<v 0>@[<v 2><span class="background-color_%s;">@,|} c
  in
  let close_span () = Format.fprintf ppf "@]@,</span>@]" in
  let tag = Format.fprintf ppf in
  match s with
  | Styling.Reset -> close_span ()
  | FG Default -> close_span ()
  | FG c ->
      fg_color
        (match c with
        | Black -> "black"
        | Red -> "red"
        | Green -> "green"
        | Yellow -> "yellow"
        | Blue -> "blue"
        | White -> "white"
        | _ -> assert false)
  | BG Default -> close_span ()
  | BG c ->
      bg_color
        (match c with
        | Black -> "black"
        | Red -> "red"
        | Green -> "green"
        | Yellow -> "yellow"
        | Blue -> "blue"
        | White -> "white"
        | _ -> assert false)
  | Styling (Bold, true) -> tag "<b>"
  | Styling (Bold, false) -> tag "</b>"
  | Styling (Italic, true) -> tag "<i>"
  | Styling (Italic, false) -> tag "</i>"
  | Styling (Underlined, true) -> tag "<u>"
  | Styling (Underlined, false) -> tag "</u>"
  | No_print _ -> assert false

let open_enclosing = { enc_open = ""; enc_close = "" }
let close_enclosing = { enc_open = ""; enc_close = "" }

let marker =
  {
    open_enclosing;
    close_enclosing;
    pp_stag = pp_html_value;
    pp_sep = (fun _ _ -> ());
  }
