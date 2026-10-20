(** Utility module with hash related function and hash-consed lists, sets and
      maps.

    All hashes (constants or computed) are positive integers less than {m 2^30}.
*)

val const0 : int
val const1 : int
val const2 : int
val const3 : int
(** Integer constants that can be used as initial hash
    values for an incremental hash algorithm.
*)

val mix : int -> int -> int
(** [mix i1 i2] combines integer [i1] and [i2] to form a hash.
*)

val mix3 : int -> int -> int -> int
(** [mix i1 i2 i3] combines integer [i1], [i2] and [i3] to form a hash.
*)

val int_of_bool : bool -> int
(** Converts [true] to [1] and [false] to [0]. *)

val int : int -> int
(** Hashes the given integer. *)

val bool : bool -> int
(** Hashes the given Boolean. *)

val list : ('a -> int) -> 'a list -> int
(** [list hash l] computes the hash of a list of elements. *)

module type Comparable =
sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end
(** The signature of comparable and hashable elements. *)

(** Hashed list of hashable elements *)
module List (X : Comparable) :
sig
  type t
  include Comparable with type t := t

  val empty : t
  (** The empty list. *)

  val is_empty : t -> bool
  (** Emptiness test. *)

  val ( $:: ) : X.t -> t -> t
  (** List constructor. *)

  val uncons : t -> (X.t * t) option
  (** List destructor. *)

  val of_list : X.t list -> t
  (** Build from a list of elements *)

  val map : (X.t -> X.t) -> t -> t
  (** [map f l] returns the list of the images of the elements of [l] by [f]. *)

  val to_list : t -> X.t list
  (** Converts to a list. Runs in constant time. *)

  val filter : (X.t -> bool) -> t -> t
  (** [filter f l] returns the list of elements of [l] for which [f] returns [true]. *)

  val for_all : (X.t -> bool) -> t -> bool
  (** [for_all f l] returns [true] iff [f] returns [true] for all elements of [l]. *)

  val length : t -> int
  (** Returns the length of the list. *)
end
module type Set = sig


  type elt
  (** The type of elements in the set. *)

  type t
  (** Sets of comparable and hashable elements which support hash in constant time.
      We call the view of a set is the list [(x0,h0) :: (x1, h1) :: … :: []]
      where the [xi] are the ordered elements of the set and [hi] is
      the has computed between the has of element [xi] and the tail of
      the list starting with element [xi+1].
  *)

  val compare : t -> t -> int
  (** Total order between two sets implemented as lexicographic order over the views. *)

  val equal : t -> t -> bool
  (** Equal is structural equality between the views. *)

  val hash : t -> int
  (** Compute the hash of the set, in constant time. *)

  val empty : t
  (** The empty set. *)

  val is_empty : t -> bool

  val add : elt -> t -> t

  val subset : t -> t -> bool

  val elements : t -> elt list
  (** Return the elements of the set as a sorted list. *)

  val filter : (elt -> bool) -> t -> t
  (** Filtering, [filter f s] is the set of elements of [s] for which [f] returns [true]. *)

  val map : (elt -> elt) -> t -> t

  val mem : elt -> t -> bool
  (** Membership test. *)

  val of_list : elt list -> t
  (** Convert from a list of elements which may contain duplicates w.r.t to the
      comparison function. (which element is kept in this case is unspecified).
  *)

  val to_list : t -> elt list
  (** An alias for elements. *)

  val union : t -> t -> t
  (** The union of two sets. If an element compares equal in both set, the
      element of the first set is kept.*)

  val inter : t -> t -> t
  (** The intersection of two sets. If an element compares equal in both set, the
      element of the first set is kept. *)

  val diff : t -> t -> t
  (** The difference of two sets. *)

  val cardinal : t -> int
  (** Return the number of elements in the set. *)

end

module type Map = sig
  type key
  (** The type of keys *)

  type value
  (** The type of values *)

  type t
  (** Maps of hashable and comparable keys and values which support hash in
      constant time.
  *)

  module Set : Set with type elt = key
  (** Represent sets of keys *)

  val add : key -> value -> t -> t
  (** Add a binding to a map. If a binding already exists, the old value is replaced. *)

  val dom : t -> Set.t
  (** The domain of the map (in constant time). *)

  val bindings : t -> (key * value) list
  (** Return the list of bindings. *)

  val compare : t -> t -> int
  (** Total ordering between maps. *)

  val equal : t -> t -> bool
  (** Equality between two maps. *)

  val hash : t -> int
  (** Hash value of a map (in constant time). *)

  val empty : t
  (** The empty map. *)

  val exists : (key -> value -> bool) -> t -> bool
  (** Test whether a binding verifies a predicate. *)

  val filter : (key -> value -> bool) -> t -> t
  (** Return the map of bindings which verify a predicate. *)

  val find_opt : key -> t -> value option
  (** Find a value in the map. Runs in linear time in the number of bindings. *)

  val map : (key -> value -> value) -> t -> t
  (** Return a map with the same keys and updated values *)

  val filter_map : (key -> value -> value option) -> t -> t

  val of_list : (key * value) list -> t
  (** Build from a list of bindings *)

  val singleton : key -> value -> t
  (** Return the map with one binding. *)

  val to_list : t -> (key * value) list
  (** An alias for bindings. *)

  val merge : (key -> value option -> value option -> value option) -> t -> t -> t
  (** Merge two maps. For each key the merge function is called. The optional
      value given as first (resp. second) arguments determines the presence of the
      binding in the first (resp. second) map.
      If the merge function returns [None], no binding is created for that key.
  *)

  val values_for_domain : Set.t -> value -> t -> value list
  (** [values_for_domain dom def map] returns the values of a map whose domain is [dom] and whose
      values are:
      - taken from [map] if the binding exists there
      - otherwise [def]
  *)

  val values : t -> value list
  (** Return the values of a map. *)

  val constant : Set.t -> value -> t
  (** Return a constant map from the given domain. *)

  val init : Set.t -> (key -> value) -> t
  (** Initializes a map with from the given domain. *)

  val fold : ('a -> key -> value -> 'a) -> 'a -> t -> 'a
  (** Folding over the bindings of the map. *)

  val is_singleton_opt : t -> (key * value) option
  (** If the map is a singleton, returns its single binding as [Some (k, v)].
      Otherwise returns [None]. *)

  val combine : Set.t -> value list -> t
  (** Create a map from the given domain and values, taken in the order of the
        list.

      @raise Invalid_argument if the domain and list of values do not
      have the same length.
  *)

  val remove : t -> Set.t -> t

  val for_all : (key -> value -> bool) -> t -> bool
end

module SetList (X : Comparable) : Set with type elt = X.t
module MapList (K : Comparable) (V : Comparable) : Map with type key = K.t and type value = V.t
