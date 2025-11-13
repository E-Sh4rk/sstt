(** Base module for applying transformations to formatters. *)

(** {2 Setting formatter} *)

val set_formatter : [> `ANSI | `HTML | `UTF8 ] list -> Format.formatter -> unit
(** [set_formatter l fmt] takes a list of options and transforms the formatter accordingly to handle semantic tags.

    The options are:
    - [`ANSI]: Adds ansi markings to the formatter according to {!TextStyling.Styling}
    - [`HTML]: Outputs the styles defined in {!TextStyling.Styling} in html syntax
    - [`UTF8]: Prettifies some strings
*)

val reset_formatter : Format.formatter -> unit
(** [reset_formatter fmt] will disable semantic tags handling for [fmt]  *)
