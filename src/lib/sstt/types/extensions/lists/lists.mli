(** The type of heterogenous lists.

    Lists are encoded {i à la} Lisp with as either a constant (the empty list [nil])
    or a pair of a value (the {i head}) and a list (the {i tail}).

    Lists are printed using a regular expression types whenever possible.
*)

open Core

val tag : Tag.t
(** The tag used for list type.
*)

val cons : Ty.t -> Ty.t -> Ty.t
(** [cons hd tl] returns the type of lists formed by the head [hd] and the tail [tl].
    The function does not check whether [tl] is a list.
*)

val nil : Ty.t
(** The empty list. *)

val any : Ty.t
(** The type of all lists. *)

val any_non_empty : Ty.t
(** The type of all non-empty lists. *)

val destruct : Ty.t -> (Ty.t * Ty.t) list
(** [destruct t] returns au union of lists [[ (hd1, tl1); …; (hdn, tln) ]] such
    that [Ty.cap t any] is equivalent to
    {math

    \bigcup_{i=1\ldots n}\texttt{cons} ~~\texttt{hd}_i ~~\texttt{tl}_i
    }
*)

val proj : Ty.t -> Ty.t * Ty.t
(** [proj t] returns the approximation
    {math

    \bigcup_{i=1\ldots n} \texttt{hd}_i ~~~\times~~~ 
    \bigcup_{i=1\ldots n} \texttt{tl}_i
    }
    where [destruct t] = [[ (hd1, tl1); …; (hdn, tln) ]]
*)

type 'a regexp =
  | Epsilon
  | Symbol of 'a
  | Concat of 'a regexp list
  | Union of 'a regexp list
  | Star of 'a regexp
  | Plus of 'a regexp
  | Option of 'a regexp

type basic = Nil | Cons of Printer.descr * Printer.descr

type t =
  | Regexp of Printer.descr regexp
  | Basic of basic list

val to_t : (Printer.ctx -> Ty.t -> Printer.descr) -> Printer.ctx
            -> TagComp.t -> t option
val map : ((Printer.descr -> Printer.descr) -> t -> t)

val printer_builder : Printer.extension_builder
val printer_params : Printer.params

val build : Ty.t regexp -> Ty.t
