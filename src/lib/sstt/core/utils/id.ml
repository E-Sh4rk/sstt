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

module NamedIdentifier() : NamedIdentifier = struct
  type t = int * string

    let next_id =
      let c = ref 0 in
      fun () -> c := !c + 1 ; !c

  let mk name =  (next_id (), name)
  let name (_, name) = name
  let hash (i,_) = Hash.int i
  let compare (i1,_) (i2,_) = Int.compare i1 i2
  let equal (i1,_) (i2,_) = Int.equal i1 i2
  let pp fmt (_,name) = Format.fprintf fmt "%s" name
  let pp_unique fmt (id,name) = Format.fprintf fmt "%s__%i" name id
end

module type MixSet =  sig
  type a
  type b
  module ASet : Set.S with type elt=a
  module BSet : Set.S with type elt=b
  type t
  val empty : t
  val of_set : ASet.t -> BSet.t -> t
  val of_set1 : ASet.t -> t
  val of_set2 : BSet.t -> t
  val of_list : a list -> b list -> t
  val of_list1 : a list -> t
  val of_list2 : b list -> t
  val singleton1 : a -> t
  val singleton2 : b -> t
  val add1 : a -> t -> t
  val add2 : b -> t -> t
  val proj1 : t -> ASet.t
  val proj2 : t -> BSet.t
  val elements : t -> a list * b list
  val elements1 : t -> a list
  val elements2 : t -> b list
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val filter : (a -> bool) -> (b -> bool) -> t -> t
  val filter1 : (a -> bool) -> t -> t
  val filter2 : (b -> bool) -> t -> t
  val is_empty : t -> bool
  val mem1 : a -> t -> bool
  val mem2 : b -> t -> bool
  val subset : t -> t -> bool
  val disjoint : t -> t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module MixSet(ASet:Set.S)(BSet:Set.S) :
  MixSet with type a=ASet.elt and type b=BSet.elt
  and module ASet=ASet and module BSet=BSet = struct
  module ASet = ASet
  module BSet = BSet
  type a = ASet.elt
  type b = BSet.elt
  type t = ASet.t * BSet.t
  let empty = ASet.empty, BSet.empty
  let of_set a b = a, b
  let of_set1 a = a, BSet.empty
  let of_set2 b = ASet.empty, b
  let of_list lst1 lst2 = ASet.of_list lst1, BSet.of_list lst2
  let of_list1 lst1 = ASet.of_list lst1, BSet.empty
  let of_list2 lst2 = ASet.empty, BSet.of_list lst2
  let singleton1 a = ASet.singleton a, BSet.empty
  let singleton2 b = ASet.empty, BSet.singleton b
  let add1 e (a,b) = ASet.add e a, b
  let add2 e (a,b) = a, BSet.add e b
  let proj1 (a,_) = a
  let proj2 (_,b) = b
  let elements (a,b) = ASet.elements a, BSet.elements b
  let elements1 (a,_) = ASet.elements a
  let elements2 (_,b) = BSet.elements b
  let union (a,b) (a',b') = ASet.union a a', BSet.union b b'
  let inter (a,b) (a',b') = ASet.inter a a', BSet.inter b b'
  let diff (a,b) (a',b') = ASet.diff a a', BSet.diff b b'
  let filter fa fb (a,b) = ASet.filter fa a, BSet.filter fb b
  let filter1 fa (a,b) = ASet.filter fa a, b
  let filter2 fb (a,b) = a, BSet.filter fb b
  let is_empty (a,b) = ASet.is_empty a && BSet.is_empty b
  let mem1 e (a,_) = ASet.mem e a
  let mem2 e (_,b) = BSet.mem e b
  let subset (a,b) (a',b') = ASet.subset a a' && BSet.subset b b'
  let disjoint (a,b) (a',b') = ASet.disjoint a a' && BSet.disjoint b b'
  let compare (a,b) (a',b') = ASet.compare a a' |> Sstt_utils.ccmp  BSet.compare b b'
  let equal (a,b) (a',b') = ASet.equal a a' && BSet.equal b b'
end

module type MixMap =  sig
  type a
  type b
  module AMap : Map.S with type key=a
  module BMap : Map.S with type key=b
  type ('va,'vb) t
  val empty : ('va,'vb) t
  val of_map : 'va AMap.t -> 'vb BMap.t -> ('va,'vb) t
  val of_map1 : 'va AMap.t -> ('va,'vb) t
  val of_map2 : 'vb BMap.t -> ('va,'vb) t
  val of_list : (a * 'va) list -> (b * 'vb) list -> ('va,'vb) t
  val of_list1 : (a * 'va) list -> ('va, 'vb) t
  val of_list2 : (b * 'vb) list -> ('va, 'vb) t
  val singleton1 : a -> 'va -> ('va, 'vb) t
  val singleton2 : b -> 'vb -> ('va, 'vb) t
  val proj1 : ('va, 'vb) t -> 'va AMap.t
  val proj2 : ('va, 'vb) t -> 'vb BMap.t
  val bindings : ('va, 'vb) t -> (a * 'va) list * (b * 'vb) list
  val bindings1 : ('va, 'vb) t -> (a * 'va) list
  val bindings2 : ('va, 'vb) t -> (b * 'vb) list
  val mem1 : a -> ('va, 'vb) t -> bool
  val mem2 : b -> ('va, 'vb) t -> bool
  val find1 : a -> ('va, 'vb) t -> 'va
  val find2 : b -> ('va, 'vb) t -> 'vb
  val find_opt1 : a -> ('va, 'vb) t -> 'va option
  val find_opt2 : b -> ('va, 'vb) t -> 'vb option
  val add1 : a -> 'va -> ('va, 'vb) t -> ('va, 'vb) t
  val add2 : b -> 'vb -> ('va, 'vb) t -> ('va, 'vb) t
  val remove1 : a -> ('va, 'vb) t -> ('va, 'vb) t
  val remove2 : b -> ('va, 'vb) t -> ('va, 'vb) t
  val fold : (a -> 'va -> 'a -> 'a) -> (b -> 'vb -> 'a -> 'a) -> ('va, 'vb) t -> 'a -> 'a
  val fold1 : (a -> 'va -> 'a -> 'a) -> ('va, 'vb) t -> 'a -> 'a
  val fold2 : (b -> 'vb -> 'a -> 'a) -> ('va, 'vb) t -> 'a -> 'a
  val map : ('va -> 'va') -> ('vb -> 'vb') -> ('va, 'vb) t -> ('va', 'vb') t
  val map1 : ('va -> 'va') -> ('va, 'vb) t -> ('va', 'vb) t
  val map2 : ('vb -> 'vb') -> ('va, 'vb) t -> ('va, 'vb') t
  val filter : (a -> 'va -> bool) -> (b -> 'vb -> bool) -> ('va, 'vb) t -> ('va, 'vb) t
  val filter1 : (a -> 'va -> bool) -> ('va, 'vb) t -> ('va, 'vb) t
  val filter2 : (b -> 'vb -> bool) -> ('va, 'vb) t -> ('va, 'vb) t
  val union : (a -> 'va -> 'va -> 'va option) -> (b -> 'vb -> 'vb -> 'vb option)
    -> ('va, 'vb) t -> ('va, 'vb) t -> ('va, 'vb) t
  val is_empty : ('va, 'vb) t -> bool
  val equal : ('va -> 'va -> bool) -> ('vb -> 'vb -> bool)
    -> ('va, 'vb) t -> ('va, 'vb) t -> bool
  val compare : ('va -> 'va -> int) -> ('vb -> 'vb -> int)
    -> ('va, 'vb) t -> ('va, 'vb) t -> int
end

module MixMap(AMap:Map.S)(BMap:Map.S) :
  MixMap with type a=AMap.key and type b=BMap.key
  and module AMap=AMap and module BMap=BMap = struct
  module AMap = AMap
  module BMap = BMap
  type a = AMap.key
  type b = BMap.key
  type ('va,'vb) t = 'va AMap.t * 'vb BMap.t
  let empty = AMap.empty, BMap.empty
  let of_map a b = a, b
  let of_map1 a = a, BMap.empty
  let of_map2 b = AMap.empty, b
  let of_list lst1 lst2 = AMap.of_list lst1, BMap.of_list lst2
  let of_list1 lst1 = AMap.of_list lst1, BMap.empty
  let of_list2 lst2 = AMap.empty, BMap.of_list lst2
  let singleton1 a v = AMap.singleton a v, BMap.empty
  let singleton2 b v = AMap.empty, BMap.singleton b v
  let proj1 (a,_) = a
  let proj2 (_,b) = b
  let bindings (a,b) = AMap.bindings a, BMap.bindings b
  let bindings1 (a,_) = AMap.bindings a
  let bindings2 (_,b) = BMap.bindings b
  let mem1 k (a,_) = AMap.mem k a
  let mem2 k (_,b) = BMap.mem k b
  let find1 k (a,_) = AMap.find k a
  let find2 k (_,b) = BMap.find k b
  let find_opt1 k (a,_) = AMap.find_opt k a
  let find_opt2 k (_,b) = BMap.find_opt k b
  let add1 k v (a,b) = AMap.add k v a, b
  let add2 k v (a,b) = a, BMap.add k v b
  let remove1 k (a,b) = AMap.remove k a, b
  let remove2 k (a,b) = a, BMap.remove k b
  let fold fa fb (a,b) acc = AMap.fold fa a acc |> BMap.fold fb b
  let fold1 f (a,_) acc = AMap.fold f a acc
  let fold2 f (_,b) acc = BMap.fold f b acc
  let map fa fb (a,b) = AMap.map fa a, BMap.map fb b
  let map1 fa (a,b) = AMap.map fa a, b
  let map2 fb (a,b) = a, BMap.map fb b
  let filter fa fb (a,b) = AMap.filter fa a, BMap.filter fb b
  let filter1 fa (a,b) = AMap.filter fa a, b
  let filter2 fb (a,b) = a, BMap.filter fb b
  let union ua ub (a1,b1) (a2,b2) = AMap.union ua a1 a2, BMap.union ub b1 b2
  let is_empty (a,b) = AMap.is_empty a && BMap.is_empty b
  let equal fa fb (a1,b1) (a2,b2) = AMap.equal fa a1 a2 && BMap.equal fb b1 b2
  let compare fa fb (a1,b1) (a2,b2) = AMap.compare fa a1 a2
    |> Sstt_utils.ccmp (BMap.compare fb) b1 b2
end
