open TextStyling

(* See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters for some values *)
let pp_ansi_value ppf s =
  Format.fprintf ppf "%s"
    (match s with
    | Styling.Reset -> "0"
    | FG Black -> "30"
    | FG Red -> "31"
    | FG Green -> "32"
    | FG Yellow -> "33"
    | FG Blue -> "34"
    | FG White -> "37"
    | FG Default -> "39"
    | BG Black -> "40"
    | BG Red -> "41"
    | BG Green -> "42"
    | BG Yellow -> "43"
    | BG Blue -> "44"
    | BG White -> "47"
    | BG Default -> "49"
    | Styling (Bold, true) -> "1"
    | Styling (Bold, false) -> "21"
    | Styling (Italic, true) -> "3"
    | Styling (Italic, false) -> "23"
    | Styling (Underlined, true) -> "4"
    | Styling (Underlined, false) -> "24"
    | No_print _ -> assert false)

let enclosing = { enc_open = "\x1B["; enc_close = "m" }

let marker =
  {
    open_enclosing = enclosing;
    close_enclosing = enclosing;
    pp_stag = pp_ansi_value;
    pp_sep = (fun ppf () -> Format.fprintf ppf ";");
  }
