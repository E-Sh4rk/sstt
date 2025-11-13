module type S = sig
  module Elem : Custom.T
  include Custom.T with type t = private Elem.t list

  external get : t -> Elem.t list = "%identity"
  external unsafe_cast : Elem.t list -> t = "%identity"
  val singleton : Elem.t -> t
  val iter : (Elem.t -> unit) -> t -> unit
  val filter : (Elem.t -> bool) -> t -> t
  val exists : (Elem.t -> bool) -> t -> bool
  val fold : ('a -> Elem.t -> 'a) -> 'a -> t -> 'a
  val pick : t -> Elem.t option
  val choose : t -> Elem.t
  val length : t -> int
  val empty : t
  val is_empty : t -> bool
  val from_list : Elem.t list -> t
  val add : Elem.t -> t -> t
  val remove : Elem.t -> t -> t
  val disjoint : t -> t -> bool
  val cup : t -> t -> t
  val split : t -> t -> t * t * t

  (* split l1 l2 = (l1 \ l2, l1 & l2, l2 \ l1) *)
  val cap : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val map : (Elem.t -> Elem.t) -> t -> t
  val mem : t -> Elem.t -> bool

  module Map : sig
    type 'a map

    external get : 'a map -> (Elem.t * 'a) list = "%identity"
    external unsafe_cast : (Elem.t * 'a) list -> 'a map = "%identity"
    val add : Elem.t -> 'a -> 'a map -> 'a map
    val length : 'a map -> int
    val domain : 'a map -> t
    val restrict : 'a map -> t -> 'a map
    val empty : 'a map
    val iter : ('a -> unit) -> 'a map -> unit
    val iteri : (Elem.t -> 'a -> unit) -> 'a map -> unit
    val filter : (Elem.t -> 'a -> bool) -> 'a map -> 'a map
    val split : (Elem.t -> 'a -> bool) -> 'a map -> 'a map * 'a map
    val is_empty : 'a map -> bool
    val singleton : Elem.t -> 'a -> 'a map
    val assoc_remove : Elem.t -> 'a map -> 'a * 'a map
    val remove : Elem.t -> 'a map -> 'a map
    val subset_keys : 'a map -> 'a map -> bool
    val merge : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val merge_set : ('a -> 'a -> 'a) -> 'a map -> t -> 'a -> 'a map

    val combine :
      ('a -> 'c) -> ('b -> 'c) -> ('a -> 'b -> 'c) -> 'a map -> 'b map -> 'c map

    val cap : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val sub : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val merge_elem : 'a -> 'a map -> 'a map -> 'a map
    val union_disj : 'a map -> 'a map -> 'a map
    val diff : 'a map -> t -> 'a map
    val from_list : ('a -> 'a -> 'a) -> (Elem.t * 'a) list -> 'a map
    val from_list_disj : (Elem.t * 'a) list -> 'a map
    val map_from_slist : (Elem.t -> 'a) -> t -> 'a map
    val collide : ('a -> 'b -> unit) -> 'a map -> 'b map -> unit
    val may_collide : ('a -> 'b -> unit) -> exn -> 'a map -> 'b map -> unit
    val map : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (Elem.t -> 'a -> 'b) -> 'a map -> 'b map
    val fold : (Elem.t -> 'a -> 'b -> 'b) -> 'a map -> 'b -> 'b
    val constant : 'a -> t -> 'a map
    val num : int -> t -> int map
    val map_to_list : ('a -> 'b) -> 'a map -> 'b list
    val mapi_to_list : (Elem.t -> 'a -> 'b) -> 'a map -> 'b list
    val assoc : Elem.t -> 'a map -> 'a
    val assoc_present : Elem.t -> 'a map -> 'a
    val replace : Elem.t -> 'a -> 'a map -> 'a map
    val update : ('a -> 'a -> 'a) -> Elem.t -> 'a -> 'a map -> 'a map
    val remove_min : 'a map -> (Elem.t * 'a) * 'a map
    val compare : ('a -> 'a -> int) -> 'a map -> 'a map -> int
    val hash : ('a -> int) -> 'a map -> int
    val equal : ('a -> 'a -> bool) -> 'a map -> 'a map -> bool
  end

  module MakeMap (Y : Custom.T) : sig
    include Custom.T with type t = Y.t Map.map
  end
end

module Make (X : Custom.T) : S with module Elem = X

module type FiniteCofinite = sig
  type elem

  type s = private
    | Finite of elem list
    | Cofinite of elem list

  include Custom.T with type t = s

  val empty : t
  val any : t
  val atom : elem -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val contains : elem -> t -> bool
  val disjoint : t -> t -> bool
  val is_empty : t -> bool
  val sample : t -> elem option
end

module FiniteCofinite (X : Custom.T) : FiniteCofinite with type elem = X.t

module FiniteCofiniteMap (X : Custom.T) (SymbolSet : FiniteCofinite) : sig
  include Custom.T

  val empty : t
  val any : t
  val any_in_ns : X.t -> t
  val atom : X.t * SymbolSet.elem -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val is_empty : t -> bool
  val symbol_set : X.t -> t -> SymbolSet.t
  val contains : X.t * SymbolSet.elem -> t -> bool
  val disjoint : t -> t -> bool

  val get :
    t ->
    [ `Finite of (X.t * SymbolSet.t) list
    | `Cofinite of (X.t * SymbolSet.t) list
    ]

  val sample : t -> (X.t * SymbolSet.elem option) option
end
