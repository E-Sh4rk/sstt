(** Some CDuce predefined types

 Most of these types are used for XML Schema built-in types.
*)

val pos_int : Types.t
(** Positive integers: [1--*]. *)

val non_neg_int : Types.t
(** Positive or null integers: [0--*]. *)

val neg_int : Types.t
(** Negative integers: [*--(-1)]. *)

val non_pos_int : Types.t
(** Negative or null integers: [*--0]. *)

val long_int : Types.t
(** Signed 64 bit integers: [-9223372036854775808 -- 9223372036854775807]. *)

val int_int : Types.t
(** Signed 32 bit integers: [-2147483648--2147483647]. *)

val short_int : Types.t
(** Signed 16 bit integers: [-32768--32767]. *)

val byte_int : Types.t
(** Signed 8 bit integers: [-128--127]. *)

val caml_int : Types.t
(** OCaml integers: [min_int--max_int]. *)

val unsigned_byte_int : Types.t
(** Unsigned 8 bit integers: [0--255]. *)

val non_zero_int : Types.t
(** Non zero integers: [Int \ 0]. *)

val pos_intstr : Types.t
(** String representing positive integers in base 2, 8, 10, 16.
    Bases other than 10 are prefixed with ["0b"] or ["0B"] (binary),
    ["0o"] or ["0O"] (octal), ["0x"] or ["0X"] (hexadecimal).
*)

val neg_intstr : Types.t
(** String accepted by [pos_intstr] prefixed by a mandatory ["-"].
*)

val intstr : Types.t
(** Either positive or negative integer strings. *)

val true_atom : AtomSet.V.t
(** The constant [`true] as a constant. *)

val false_atom : AtomSet.V.t
(** The constant [`false] as a constant. *)

val true_type : Types.t
(** The singleton type [`true]. *)

val false_type : Types.t
(** The singleton type [`false]. *)

val any : Types.t
(** The top type [Any]. *)

val atom : Types.t
(** An alias for [Types.Atom.any]. *)

val nil : Types.t
(** The singleton type [`nil] (also written [[]]). *)

val bool : Types.t
(** The type [`false | `true ]. *)

val int : Types.t
(** An alias for [Types.Int.any]. *)

val char : Types.t
(** An alias for [Types.Char.any]. *)

val string : Types.t
(** An alias for [[Char*]]. *)

val char_latin1 : Types.t
(** An alias for [Byte]. *)

val string_latin1 : Types.t
(** An alias for [[Byte*]]. *)

val time_kind : Types.t
(** The union of atoms [`duration | `dateTime | `time | `date | `gYearMonth |
  `gYear | `gMonthDay | `gDay | `gMonth]. *)

val mk_ref : get:'a -> set:'a -> 'a Ident.label_map
(** [mk_ref ~get ~set] returns a record with two fields [get] and [set]
    bound to the values of [~get] and [~set].
*)

val ref_type : Types.Node.t -> Types.t
(** [ref_type n] constructs a reference type for the type node [n].
    A reference type is a record [{ get: [] -> n; set: n -> [] }].
*)

val float_abs : AbstractSet.elem
(** OCaml floats, as an abstract type with label ["float"]. *)

val float : Types.t
(** OCaml floats, as a ℂDuce type. *)

val number : Types.t
(** The union type [Int | Float]. *)

val any_xml : Types.t
(** The recursive type representing any XML document: [ AnyXml = <_ ..>[
  (Char|AnyXml)*]].
*)

val any_xml_with_tag : AtomSet.t -> Types.t
(** [any_xml_with_tag t] returns the type of an XML document with tag [t]
 for its root element.
*)

val seq_type : Types.Node.t -> Types.t
(** [seq_type n] returns the type [[n*]]. *)
