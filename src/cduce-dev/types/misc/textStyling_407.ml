module Styling = struct
  type color =
    | Default
    | Red
    | Blue
    | Green
    | Black
    | Yellow
    | White

  type styling =
    | Italic
    | Bold
    | Underlined

  type no_print = Prettify

  type style =
    | Reset
    | FG of color
    | BG of color
    | Styling of styling * bool
    | No_print of no_print * bool
end

type enclosing = {
  enc_open : string;
  enc_close : string;
}

type marker = {
  open_enclosing : enclosing;
  close_enclosing : enclosing;
  pp_stag : Format.formatter -> Styling.style -> unit;
  pp_sep : Format.formatter -> unit -> unit;
}

type stag_marker =
  | Mark
  | Print

let prettify_formatter _ = ()
let add_stag_handlers _ _ _ = ()
