
module type E = sig
  include Set.OrderedType
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type t
  type e

  val any : unit -> t
  val empty : unit -> t

  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t

  val is_empty : t -> bool

  val mk : e list -> t
  val mk_singl : e -> t
  val get : t -> bool * e list

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
end

module Make(E:E) : S with type e = E.t = struct
  module ESet = Set.Make(E)
  type t = Pos of ESet.t | Neg of ESet.t
  type e = E.t

  let any () = Neg ESet.empty
  let empty () = Pos ESet.empty

  let mk es = Pos (ESet.of_list es)
  let mk_singl e = Pos (ESet.singleton e)
  let get t = match t with
  | Pos s -> true, ESet.elements s
  | Neg s -> false, ESet.elements s

  let cap t1 t2 =
    match t1, t2 with
    | Pos p1, Pos p2 -> Pos (ESet.inter p1 p2)
    | Neg n1, Neg n2 -> Neg (ESet.union n1 n2)
    | Pos p, Neg n | Neg n, Pos p -> Pos (ESet.diff p n)
  let cup t1 t2 =
    match t1, t2 with
    | Pos p1, Pos p2 -> Pos (ESet.union p1 p2)
    | Neg n1, Neg n2 -> Neg (ESet.inter n1 n2)
    | Pos p, Neg n | Neg n, Pos p -> Neg (ESet.diff n p)
  let neg = function
  | Pos s -> Neg s
  | Neg s -> Pos s
  let diff t1 t2 = cap t1 (neg t2)

  let is_any = function
  | Pos _ -> false
  | Neg n -> ESet.is_empty n

  let is_empty = function
  | Pos p -> ESet.is_empty p
  | Neg _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | Pos _, Neg _ -> 1
    | Neg _, Pos _ -> -1
    | Pos s1, Pos s2 | Neg s1, Neg s2 -> ESet.compare s1 s2
  let equal t1 t2 =
    match t1, t2 with
    | Pos _, Neg _ | Neg _, Pos _ -> false
    | Pos s1, Pos s2 | Neg s1, Neg s2 -> ESet.equal s1 s2

  let pp fmt t =
    if is_any t
    then Format.fprintf fmt "⊤"
    else if is_empty t
    then Format.fprintf fmt "⊥"
    else
      match t with
      | Pos p -> Format.fprintf fmt "(%a)" (Utils.print_seq E.pp "|") (ESet.elements p)
      | Neg n -> Format.fprintf fmt "~(%a)" (Utils.print_seq E.pp "|") (ESet.elements n)
end
