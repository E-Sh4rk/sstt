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
  (* the boolean correspond to the enabling/disabling of the styling *)

  let prettify = ref false

  let close_tag = function
    | FG _ -> FG Default
    | BG _ -> BG Default
    | Styling (s, true) -> Styling (s, false)
    | Styling (s, false) -> Styling (s, true)
    | Reset -> Reset
    | No_print _ -> assert false

  let style_of_string = function
    | "n"
    | "r" ->
        Reset
    | "fg_black" -> FG Black
    | "fg_blue" -> FG Blue
    | "fg_red" -> FG Red
    | "fg_green" -> FG Green
    | "fg_yellow" -> FG Yellow
    | "fg_white" -> FG White
    | "fg_default" -> FG Default
    | "bg_black" -> BG Black
    | "bg_blue" -> BG Blue
    | "bg_red" -> BG Red
    | "bg_green" -> BG Green
    | "bg_yellow" -> BG Yellow
    | "bg_white" -> BG White
    | "bg_default" -> BG Default
    | "italic" -> Styling (Italic, true)
    | "italic_off" -> Styling (Italic, false)
    | "bold" -> Styling (Bold, true)
    | "bold_off" -> Styling (Bold, false)
    | "underlined" -> Styling (Underlined, true)
    | "underlined_off" -> Styling (Underlined, false)
    | "prettify" -> No_print (Prettify, true)
    | "prettify_off" -> No_print (Prettify, false)
    | s -> failwith ("style_of_string: '" ^ s ^ "' is an unknown style")

  let styles_of_stag =
    (* Partition a stag list in two style lists:
       - a list of styles that need to be printed
       - a list of styles that just have side effects
    *)
    function
    | Format.String_tag s ->
        String.split_on_char ';' s
        |> List.map (fun s -> String.trim s |> style_of_string)
        |> List.partition (function
             | No_print _ -> true
             | _ -> false)
    | _ -> raise Not_found

  let side_effects ~close l =
    (* There is currently one possible side effect, prettifying
       If we're in a style opening, set prettify to true else set it to false
    *)
    List.iter
      (function
        | No_print (Prettify, b) ->
            if close then prettify := not b else prettify := b
        | _ -> assert false)
      l

  (** Creates a printer for a list of styles *)
  let pp_styles ~close ~enc_open ~pp_stag ~enc_close ~pp_sep ppf l =
    Format.fprintf ppf "%a"
      (Format.pp_print_list ~pp_sep (fun ppf stag ->
           Format.fprintf ppf "%s%a%s" enc_open pp_stag
             (if close then close_tag stag else stag)
             enc_close))
      l

  (* Steps:
     - Parse the semantic tags
     - Split them between the printable ones (color, style etc) and the side effect ones (prettifying)
     - If functions have been provided to print the printables for the current formatter, use it
     - Otherwise, print an empty string
     - Handle the side effects
     - Close the side effects
     - Close the printables with either the provided function or with an empty string
  *)
  let create_mark ?(close = false) pp_styles ppf stag =
    (* Making sure that no print stags are the last opened ones and the
       first closed ones. Otherwise, it could change the output of other stags *)
    (* Parsing and Splitting *)
    let se, pr = styles_of_stag stag in
    if close then (
      side_effects ~close se;
      pp_styles ppf pr)
    else Format.kfprintf (fun _ -> side_effects ~close se) ppf "%a" pp_styles pr

  let prettify () = !prettify
end

(* Function that ensures the handling of side effect stags and doesn't print anything *)
let empty_string_of_styles ?(close = false) stag =
  let empty ppf stag = Styling.create_mark ~close (fun _ _ -> ()) ppf stag in
  Format.asprintf "%a" empty stag

let prettify_formatter formatter =
  let open Format in
  pp_set_mark_tags formatter true;
  let old_of = pp_get_formatter_out_functions formatter () in
  let out_string string pos nb_chars =
    let new_string, new_nb_chars =
      if Styling.prettify () then Pretty_utf8.prettify string pos nb_chars
      else (string, nb_chars)
    in
    old_of.out_string new_string pos new_nb_chars
  in
  let old_fs = Format.pp_get_formatter_stag_functions formatter () in
  pp_set_formatter_stag_functions formatter
    {
      old_fs with
      mark_open_stag = empty_string_of_styles;
      mark_close_stag = empty_string_of_styles ~close:true;
    };
  pp_set_formatter_out_functions formatter { old_of with out_string }

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

let add_stag_handlers m_type marker formatter =
  let open Format in
  (* Handle stags as marks (0 length) or prints (normal length) *)
  (match m_type with
  | Mark -> pp_set_mark_tags formatter true
  | Print -> pp_set_print_tags formatter true);
  let old_fs = pp_get_formatter_stag_functions formatter () in
  let open_stag =
    let close = false in
    Styling.(
      create_mark ~close
        (pp_styles ~close ~enc_open:marker.open_enclosing.enc_open
           ~pp_stag:marker.pp_stag ~enc_close:marker.open_enclosing.enc_close
           ~pp_sep:marker.pp_sep))
  in
  let close_stag =
    let close = true in
    Styling.(
      create_mark ~close
        (pp_styles ~close ~enc_open:marker.close_enclosing.enc_open
           ~pp_stag:marker.pp_stag ~enc_close:marker.close_enclosing.enc_close
           ~pp_sep:marker.pp_sep))
  in
  match m_type with
  | Mark ->
      let mark_open_stag = Format.asprintf "%a" open_stag in
      let mark_close_stag = Format.asprintf "%a" close_stag in
      pp_set_formatter_stag_functions formatter
        { old_fs with mark_open_stag; mark_close_stag }
  | Print ->
      let print_open_stag = Format.fprintf formatter "%a" open_stag in
      let print_close_stag = Format.fprintf formatter "%a" close_stag in
      pp_set_formatter_stag_functions formatter
        { old_fs with print_open_stag; print_close_stag }
