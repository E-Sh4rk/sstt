(** Styler signature *)

(** If you want to handle text styling, your module should have the following signature *)

(** Semantic tags are introduced in a formatting string with the two following forms:
    - [Format.fprintf ppf "@{<semantic_tag1>@{<semantic_tag2>...@}@}" ...]
    - [Format.fprintf ppf "@{<semantic_tag1; semantic_tag2>...@}" ...]
*)

module type S = sig
  val marker : TextStyling.marker
  (** [marker] will allow {!TextStyling.add_stag_handlers} to add styling to a formatter.

    The current handled tags are the following:
    - [n | r]          : Reset all the previous tags
    - [fg_black]   : Black foreground
    - [fg_blue]    : Blue foreground
    - [fg_green]   : Green foreground
    - [fg_red]     : Red foreground
    - [fg_white]   : White foreground
    - [fg_yellow]  : Yellow foreground
    - [fg_default] : Default background
    - [bg_black]   : Black background
    - [bg_blue]    : Blue background
    - [bg_green]   : Green background
    - [bg_red]     : Red background
    - [bg_white]   : White background
    - [bg_yellow]  : Yellow background
    - [bg_default] : Default background
    - [italic]     : Italic text
    - [bold]       : Bold text
    - [underlined] : Underlined text
    - [prettify]   : Replace some symbols by their unicode equivalent
*)
end

(** {2 Contributors} *)

(** If you want to add a new style you'll need to define it and add this possibility to {!Terminal_styling} *)
