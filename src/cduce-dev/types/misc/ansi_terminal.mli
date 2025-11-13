(** ANSI terminal handling *)

include Styler.S
(** See {!Styler} for more informations *)

(** {2 Contributors} *)

(** If you want to add a tag (you can find a list of all possible semantic tags {{: https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters}here}) you need to:
    - Add a constructor to {!TextStyling.style}
    - If needed, add the inverse constructor to {!TextStyling.style}
    - Add your first constructor to the {!TextStyling.close_tag} function linking it to the corresponding inverse constructor
    - Add the value associated to your constructor(s) to {!pp_ansi_value}
 *)
