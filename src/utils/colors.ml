(* Semantic tags for console output *)
(* Thanks OCamlPro https://ocamlpro.com/blog/2020_06_01_fr_tutoriel_format/ *)

open Format

type style =
  | Normal        (*  0 *)
  | Bold          (*  1 *)
  | Italic        (*  3 *)
  | Underline     (*  4 *)
  | Bold_off      (* 22 *)
  | Italic_off    (* 23 *)
  | Underline_off (* 24 *)
  | Black         (* 30 *)
  | Red           (* 31 *)
  | Green         (* 32 *)
  | Yellow        (* 33 *)
  | Blue          (* 34 *)
  | Purple        (* 35 *)
  | Cyan          (* 36 *)
  | White         (* 37 *)
  | Default       (* 39 *)
  | BgBlack       (* 40 *)
  | BgRed         (* 41 *)
  | BgGreen       (* 42 *)
  | BgYellow      (* 43 *)
  | BgBlue        (* 44 *)
  | BgPurple      (* 45 *)
  | BgCyan        (* 46 *)
  | BgWhite       (* 47 *)
  | BgDefault     (* 49 *)

let close_tag = function
  | Bold -> Bold_off
  | Italic -> Italic_off
  | Underline -> Underline_off
  | Black | Red | Green | Yellow | Blue | Purple | Cyan | White | Default
    -> Default
  | BgBlack | BgRed | BgGreen | BgYellow | BgBlue | BgPurple | BgCyan | BgWhite
    | BgDefault -> BgDefault
  | _ -> Normal

let close_tags = List.map close_tag

let style_of_tag = function
  | "n"          -> Normal
  | "bold"       -> Bold
  | "italic"     -> Italic
  | "uline"      -> Underline
  | "/bold"      -> Bold_off
  | "/italic"    -> Italic_off
  | "/uline"     -> Underline_off
  | "black"      -> Black
  | "red"        -> Red
  | "green"      -> Green
  | "yellow"     -> Yellow
  | "blue"       -> Blue
  | "purple"     -> Purple
  | "cyan"       -> Cyan
  | "white"      -> White
  | "default"    -> Default
  | "bg_black"   -> BgBlack
  | "bg_red"     -> BgRed
  | "bg_green"   -> BgGreen
  | "bg_yellow"  -> BgYellow
  | "bg_blue"    -> BgBlue
  | "bg_purple"  -> BgPurple
  | "bg_cyan"    -> BgCyan
  | "bg_white"   -> BgWhite
  | "bg_default" -> BgDefault
  | _ -> raise Not_found

let styles_of_tag = function
  | String_tag s ->
     Str.split (Str.regexp ";") s
     |> List.map style_of_tag
  | _ -> raise Not_found

let to_ansi_value = function
  | Normal        -> "0"
  | Bold          -> "1"
  | Italic        -> "3"
  | Underline     -> "4"
  | Bold_off      -> "22"
  | Italic_off    -> "23"
  | Underline_off -> "24"
  | Black         -> "30"
  | Red           -> "31"
  | Green         -> "32"
  | Yellow        -> "33"
  | Blue          -> "34"
  | Purple        -> "35"
  | Cyan          -> "36"
  | White         -> "37"
  | Default       -> "39"
  | BgBlack       -> "40"
  | BgRed         -> "41"
  | BgGreen       -> "42"
  | BgYellow      -> "43"
  | BgBlue        -> "44"
  | BgPurple      -> "45"
  | BgCyan        -> "46"
  | BgWhite       -> "47"
  | BgDefault     -> "49"

let to_ansi_values = function
  | [] -> ""
  | x :: xs ->
     List.fold_left (fun a s -> a ^ ";" ^ (to_ansi_value s))
       (to_ansi_value x)
       xs

let ansi_tag = Printf.sprintf "\x1B[%sm"

let start_mark_ansi_stag t =
  styles_of_tag t |> to_ansi_values |> ansi_tag

let stop_mark_ansi_stag t =
  styles_of_tag t |> close_tags |> to_ansi_values |> ansi_tag

let add_ansi_marking fmt =
  let open Format in
  pp_set_mark_tags fmt true;
  let old_fs = pp_get_formatter_stag_functions fmt () in
  pp_set_formatter_stag_functions fmt
    { old_fs with
      mark_open_stag = start_mark_ansi_stag;
      mark_close_stag = stop_mark_ansi_stag }
