open Sstt_core

val tag : TagComp.Tag.t
val add_tag : Ty.t -> Ty.t

val cons : Ty.t -> Ty.t -> Ty.t
val nil : Ty.t
val any : Ty.t
val any_non_empty : Ty.t
val destruct : Ty.t -> (Ty.t * Ty.t) list
val destruct' : Ty.t -> Ty.t * Ty.t

val basic_printer_params : Printer.params

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

type printer = Format.formatter -> t -> unit

val print : printer

val printer_params : printer -> Printer.params
val printer_params' : Printer.params

val build : Ty.t regexp -> Ty.t
