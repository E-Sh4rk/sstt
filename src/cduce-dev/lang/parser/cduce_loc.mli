(* Locations in source file,
   and presentation of results and errors *)

type pos = Lexing.position
type loc = pos * pos

val source_name : loc -> string

val stdin_source : string
val toplevel_source : string
val jsoo_source : string

val is_dummy_source : string -> bool

type precise =
  [ `Full
  | `Char of int
  ]

val nopos : Lexing.position
val noloc : loc

val merge_loc : loc -> loc -> loc

val print_loc : Format.formatter -> loc * precise -> unit

type 'a located = {
  loc : loc;
  descr : 'a;
}

val mk_loc : loc -> 'a -> 'a located
val mknoloc : 'a -> 'a located

val add_to_obj_path : string -> unit
val get_obj_path : unit -> string list
val resolve_filename : string -> string
val warning : loc -> string -> unit
