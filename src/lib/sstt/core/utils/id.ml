module type NamedIdentifier =
sig
  type t

  (** [mk name] makes a new identifier of name [name].
  This will generate a fresh identifier even if another identifier
  has name [name]. *)
  val mk : string -> t

  (** [name t] returns the name of the identifier [t]. *)
  val name : t -> string

  val hash : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool

  (** [pp fmt t] prints the name of [t] using the formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** [pp_unique fmt t] prints the name of [t] followed by a unique
  integer to disambiguate it from other identifiers with the same name. *)
  val pp_unique : Format.formatter -> t -> unit
end

module NamedIdentifier () : NamedIdentifier = struct
  type t = int * string

    let next_id =
      let c = ref 0 in
      fun () -> c := !c + 1 ; !c

  let mk name =  (next_id (), name)
  let name (_, name) = name
  let hash (i,_) = Hashtbl.hash i
  let compare (i1,_) (i2,_) = Int.compare i1 i2
  let equal (i1,_) (i2,_) = (i1 == i2)
  let pp fmt (_,name) = Format.fprintf fmt "%s" name
  let pp_unique fmt (id,name) = Format.fprintf fmt "%s__%i" name id
end
