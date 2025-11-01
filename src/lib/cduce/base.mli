
module CD = Cduce_types

type typ = CD.Types.t
type node = CD.Types.Node.t

val dump_typ : Format.formatter -> typ -> unit
val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string

val register : string -> typ -> unit
val pp : Format.formatter -> typ -> unit
val string_of_type : typ -> string
val pp_node : Format.formatter -> node -> unit
val descr : node -> typ
val cons : typ -> node

val any : typ
val empty : typ
val any_node : node
val empty_node : node

val true_typ : typ
val false_typ : typ
val bool_typ : typ
val int_typ : typ
val float_typ : typ
val char_typ : typ
val unit_typ : typ
val nil_typ : typ
val string_typ : typ
val list_typ : typ
val interval : Z.t option -> Z.t option -> typ
val single_char : char -> typ
val single_string : string -> typ

val neg : typ -> typ
val cup : typ -> typ -> typ
val cap : typ -> typ -> typ
val diff : typ -> typ -> typ
(* val cup_o : typ -> typ -> typ
val cap_o : typ -> typ -> typ
val diff_o : typ -> typ -> typ *)

val mk_atom : string -> typ
val atom_any : typ
val mk_new_typ: unit -> node
val define_typ: node -> typ -> unit
(*val normalize_typ: typ -> typ*)

module Iter = CD.Types.Iter
module type Kind = CD.Types.Kind

val mk_times : node -> node -> typ
val pair_any : typ
val pi1 : typ -> typ
val pi2 : typ -> typ
val pair_dnf : typ -> (typ * typ) list

val to_label : string -> Ns.Label.t
val from_label : Ns.Label.t -> string
val mk_record : bool (* is_open *) -> (string * node) list -> typ
val record_dnf : typ -> ((string * (bool * typ)) list * bool) list
val record_any : typ
val absent : typ
val has_absent : typ -> bool
val any_or_absent : typ
val absent_node : node
val any_or_absent_node : node
val or_absent : typ -> typ
val empty_closed_record : typ
val empty_open_record : typ
val get_field : typ -> string -> typ (* Can raise Not_found *)
val get_field_assuming_not_absent : typ -> string -> typ
val merge_records : typ -> typ -> typ
val remove_field : typ -> string -> typ

val mk_arrow : node -> node -> typ
val arrow_any : typ
val domain : typ -> typ
val apply : typ -> typ -> typ
val dnf : typ -> (typ * typ) list list

val is_empty : typ -> bool
val non_empty: typ -> bool
val subtype  : typ -> typ -> bool
val disjoint : typ -> typ -> bool
val equiv : typ -> typ -> bool
