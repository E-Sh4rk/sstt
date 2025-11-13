let set_formatter l formatter =
  let sides, prints =
    List.partition
      (function
        | `UTF8 -> true
        | _ -> false)
      l
  in
  match sides with
  | [] -> ()
  | [ `UTF8 ] -> (
      TextStyling.prettify_formatter formatter;
      match prints with
      | [ `ANSI ] ->
          TextStyling.add_stag_handlers Mark Ansi_terminal.marker formatter
      | [ `HTML ] ->
          TextStyling.add_stag_handlers Print Html_styling.marker formatter
      | [] -> ()
      | _ ->
          failwith
            "You're trying to combine different stag handlers or to handle a \
             styler that was not fully configured.")
  | _ -> assert false

let reset_formatter formatter =
  Format.pp_set_mark_tags formatter false;
  Format.pp_set_print_tags formatter false
