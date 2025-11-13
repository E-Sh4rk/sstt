(** Base module for text styling and pretty printing *)

(** {2 Styling} *)
module Styling : sig
  (** {3 Colors} *)

  type color =
    | Default
    | Red
    | Blue
    | Green
    | Black
    | Yellow
    | White

  (** {3 Styles} *)

  type styling =
    | Italic
    | Bold
    | Underlined

  (** {3 No printing action} *)

  (** These actions don't print anything but are supposed to change the internal
      state of the formatter *)
  type no_print = Prettify

  (** {3 Semantic Styles} *)

  type style =
    | Reset
    | FG of color
    | BG of color
    | Styling of styling * bool
    | No_print of no_print * bool
end

(** {2 Formatter modification} *)

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

val prettify_formatter : Format.formatter -> unit
(** [prettify_formatter formatter] adds UTF8 prettifying to [formatter] *)

val add_stag_handlers : stag_marker -> marker -> Format.formatter -> unit
(** [add_marking ppf ~mark_open ~pp_stag ~mark_close ~pp_sep] will redefine semantic tag operations for mark opening and closing (print opening and closing are left untouched).

    @param ppf the formatter that will handle semantic tags
    @param pp_stag the semantic tag printer (i.e. for ANSI markings, this will be a function associating styles to their ANSI values, [FG Red -> "31"] etc.)
    @param open_mark_open the string to print before opening the semantic tag (i.e. for ANSI markings, this will correspond to ["\x1B["])
    @param open_mark_close the string to print after opening the semantic tag (i.e. for ANSI markings, this will correspond to ["m"])
    @param close_mark_open the string to print before closing the semantic tag (i.e. for ANSI markings, this will correspond to ["\x1B["])
    @param close_mark_close the string to print after closing the semantic tag (i.e. for ANSI markings, this will correspond to ["m"])
    @param pp_sep the separator function that should be used when printing a list of semantic tags
*)
