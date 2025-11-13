(** Pretty-printing of types. *)

val register_global :
  string -> Ns.QName.t -> ?params:Types.t list -> Types.t -> unit
(** [register_global cu name t] registers type [t] under [name] for compilation
  unit [cu]. The name is then used by [print] and [print_node] in place of
  the type expansion. The optional [params] parameters allows one to pass
  type parameters in case [t] is a parametric type.
*)

val print_const : Format.formatter -> Types.const -> unit
(** [print_const fmt c] prints the type constant [c] to the specified formatter.
  *)

val print : Format.formatter -> Types.t -> unit
(** [print fmt t] prints the type [t] to the specified formatter. 
  Heuristics are used to decompile recursive types that ar subtypes of
  [[Any*]] into regular expressions, and to display the name of builtin
  and already registered type names.
*)

val to_string : Types.t -> string
(** [to_string t] is a convenience function to generate a string from a type,
  using a string formatter. 
*)

val print_node : Format.formatter -> Types.Node.t -> unit
(** [print_node fmt n] is an alias for [print fmt (Types.descr n)]. *)

val print_noname : Format.formatter -> Types.t -> unit
(** [print_noname fmt t] behaves like [print fmt t] except that name are not used,
except for : [Any], [Int], [Char], [Atoms] and the special case [Bool].
*)

(**/**)

type service_params =
  | TProd of service_params * service_params
  | TOption of service_params
  | TList of string * service_params
  | TSet of service_params
  | TSum of service_params * service_params
  | TString of string
  | TInt of string
  | TInt32 of string
  | TInt64 of string
  | TFloat of string
  | TBool of string
  | TFile of string
  (* | TUserType of string * (string -> 'a) * ('a -> string) *)
  | TCoord of string
  | TCoordv of service_params * string
  | TESuffix of string
  | TESuffixs of string
  (*  | TESuffixu of (string * (string -> 'a) * ('a -> string)) *)
  | TSuffix of (bool * service_params)
  | TUnit
  | TAny
  | TConst of string

module Service : sig
  val to_service_params : Types.t -> service_params
  val to_string : service_params -> string
end
