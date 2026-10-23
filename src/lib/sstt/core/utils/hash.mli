val const0 : int
val const1 : int
val const2 : int
val const3 : int
val mix : int -> int -> int
val mix3 : int -> int -> int -> int
val int_of_bool : bool -> int
val int : int -> int
val bool : bool -> int
val list : ('a -> int) -> 'a list -> int

module type Comparable =
sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module List (X : Comparable) : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val empty : t
  val is_empty : t -> bool
  val of_list : X.t list -> t
  val ( $:: ) : X.t -> t -> t
  val map : (X.t -> X.t) -> t -> t
  val to_list : t -> X.t list
  val filter : (X.t -> bool) -> t -> t
  val for_all : (X.t -> bool) -> t -> bool
  val length : t -> int
  type 's hash
  type 's hd = private X.t
  type 's lst = private X.t list
  type 's view = [] | (::) of 's hd * 's lst
  type 'r cop = { cop : 's. 's hash -> 'r } [@@unboxed]
  type 'r uop = { uop : 's. 's hash -> 's lst -> 'r } [@@unboxed]

  val create : 'r cop -> 'r
  val update : t -> 'r uop -> 'r
  val view : 's lst -> 's view
  val empty_lst : 's lst
  val to_lst : 's hash -> X.t list -> 's lst
  val cons : 's hash -> X.t -> 's lst -> 's lst
  val tail : 's hash -> 's lst -> 's lst
  val ( @:: ) : 's hd -> 's lst -> 's lst
  val make : 's hash -> 's lst -> t
end


module type Set =
sig
  type elt
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val subset : t -> t -> bool
  val elements : t -> elt list
  val filter : (elt -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t
  val mem : elt -> t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val cardinal : t -> int
end
module type Map =
sig
  type key
  type value
  type t
  module Set : Set with type elt = key
  val add : key -> value -> t -> t
  val dom : t -> Set.t
  val bindings : t -> (key * value) list
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val empty : t
  val exists : (key -> value -> bool) -> t -> bool
  val filter : (key -> value -> bool) -> t -> t
  val find_opt : key -> t -> value option
  val map : (key -> value -> value) -> t -> t
  val filter_map : (key -> value -> value option) -> t -> t
  val of_list : (key * value) list -> t
  val singleton : key -> value -> t
  val to_list : t -> key list * value list
  val merge_disjoint : t -> t -> t
  val merge :
    (key -> value option -> value option -> value option) -> t -> t -> t
  val values_for_domain : Set.t -> value -> t -> value list
  val values : t -> value list
  val constant : Set.t -> value -> t
  val init : Set.t -> (key -> value) -> t
  val fold : ('a -> key -> value -> 'a) -> 'a -> t -> 'a
  val is_singleton_opt : t -> (key * value) option
  val combine : Set.t -> value list -> t
  val remove : t -> Set.t -> t
  val for_all : (key -> value -> bool) -> t -> bool
end
module SetList (X : Comparable) : Set with type elt = X.t
module MapList (K : Comparable) (V : Comparable) :
  Map with type key = K.t and type value = V.t
