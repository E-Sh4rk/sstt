(** HTML styling handling *)

include Styler.S
(** See {!Styler} for more informations *)

(** {2 Contributors} *)

(** If you want to add an html tag:
    - Add a constructor to {!TextStyling.style}
    - If needed, add the inverse constructor to {!TextStyling.style}
    - Add your first constructor to the {!TextStyling.close_tag} function linking it to the corresponding inverse constructor
    - Add the value associated to your constructor(s) to {!pp_html_value}
 *)
