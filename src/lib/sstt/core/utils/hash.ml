let const0 = 0x278dde6d (* 2^32 * golden ratio, mod 2^30 *)
let const1 = 0x3f4a7c15 (* from SplitMix64 *)
let const2 = 0x27d4eb2f (* from XXHash *)
let const3 = 0x165667b1 (* from Murmur3 *)


(* From Boost standard library, mod 2^30 *)
let[@inline always] mix h1 h2 =
  (h1 lxor (h2 + const0 + (h1 lsl 6) + (h1 lsr 2))) land 0x3fffffff

let[@inline always] mix3 h1 h2 h3 =
  mix h1 (mix h2 h3)
let[@inline always] int_of_bool = (* OCaml compiles this to nop *) 
  function false -> 0
         | true -> 1
let int i = mix const0 i
let[@inline always] bool b = int (int_of_bool b)

let list h l = (* use for small lists *)
  let rec loop acc l =
    match l with
      [] -> acc
    | e :: ll -> loop (mix acc (h e)) ll
  in
  loop const3 l

(* hash.ml cannot depend on sigs.ml, it would introduce a cyclic dependency 
*)
module type Comparable =
sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t
end

module List(X : Comparable) : sig
  type t = (X.t * int) list
  include Comparable with type t := t
  val ($::) : X.t -> t -> t
  val of_list : X.t list -> t
  val map : (X.t -> X.t) -> t -> t
  val to_list : t -> X.t list
  val filter : (X.t -> bool) -> t -> t
  val for_all : (X.t -> bool) -> t -> bool
end = struct

  type t = (X.t * int) list

  let compare_hash (x1, h1) (x2, h2) =
    let c = Int.compare h1 h2 in
    if c <> 0 then c else X.compare x1 x2

  let equal_hash (x1, h1) (x2, h2) =
    Int.equal h1 h2 && X.equal x1 x2
  let compare = List.compare compare_hash
  let equal = List.equal equal_hash

  let[@inline always] hash = function
      [] -> const0
    | (_, h) :: _ -> h

  let[@inline always] ($::) x l = (x, mix (hash l) (X.hash x))::l

  let of_list = List.fold_left (fun acc e -> e $:: acc) []
  let map f l =
    let rec loop l =
      match l with
        [] -> []
      | (e, _) :: ll -> (f e) $:: loop ll
    in loop l

  let to_list = List.map fst

  let rec filter f = function 
    | [] -> []
    | (e, _) :: ll -> if f e then e $:: filter f ll else filter f ll

  let rec for_all f = function
    | [] -> true
    | (e, _) :: ll -> (f e) && for_all f ll
end


module type Set = sig
  type elt
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val empty : t
  val elements : t -> elt list
  val filter : (elt -> bool) -> t -> t
  val mem : elt -> t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  val union : t -> t -> t
  val cardinal : t -> int
end

module type Map = sig 
  type key
  type value 
  type t 
  module Set : Set with type elt = key
  val add : key -> value -> t -> t
  val dom : t -> Set.t
  val bindings : t -> (key * value) list
  val compare : t -> t -> int
  val empty : t
  val equal : t -> t -> bool
  val hash : t -> int
  val exists : (key -> value -> bool) -> t -> bool
  val filter : (key -> value -> bool) -> t -> t
  val find_opt : key -> t -> value option
  val map : (value -> value) -> t -> t
  val of_list : (key * value) list -> t
  val singleton : key -> value -> t
  val to_list : t -> (key * value) list
end
include (
struct


  module SetList ( X : Comparable ) = 
  struct
    type elt = X.t
    include List(X)
    let elements = to_list
    let empty = []
    let rec mem x l =
      match l with
        [] -> false
      | (y, _) :: ll -> 
        let c = X.compare y x in
        if c < 0 then mem x ll
        else c = 0

    let rec add x l =
      match l with
        []  -> x $:: []
      | (y, _) :: ll ->
        let c = X.compare x y in
        if c < 0 then x $:: l
        else if c = 0 then l
        else y $:: add x ll

    let of_list l = Stdlib.List.fold_left (fun acc x -> add x acc) empty l
    let rec union l1 l2 =
      match l1, l2 with
        ([], l) | (l, []) -> l
      | (x1, _) :: ll1, (x2, _) :: ll2 ->
        let c = X.compare x1 x2 in
        if c < 0 then x1 $:: union ll1 l2
        else if c = 0 then x1 $:: union ll1 ll2
        else x2 $:: union l1 ll2
    let cardinal t = Stdlib.List.length t

  end

  module MapList (K : Comparable) (V : Comparable) 
  = 
  struct
    type key = K.t
    type value = V.t
    module KV = struct type t = K.t * V.t 
      let compare (k1, v1) (k2, v2) =
        let c = K.compare k1 k2 in 
        if c <> 0 then c else V.compare v1 v2

      let equal (k1, v1) (k2, v2) =
        K.equal k1 k2 && V.equal v1 v2

      let hash (k, v) = 
        mix3 const3 (K.hash k) (V.hash v)
    end
    module Set = SetList(K)
    module SL = List(KV)

    type t = SL.t

    let[@inline always] hash x = SL.hash x
    let[@inline always] ($::) x l = SL.($::) x l 
    let rec add k v l =
      match l with
        [] -> (k, v) $:: []
      | (((x, _) as b), _) :: ll ->
        let c = K.compare k x in 
        if c < 0 then (k, v) $:: l
        else if c = 0 then (x, v)$:: ll
        else  b $::(add k v ll)

    let bindings l = Stdlib.List.map fst l
    let compare = SL.compare
    let empty = []
    let equal = SL.equal
    let exists f l = Stdlib.List.exists (fun ((k, v),_) -> f k v) l 
    let rec filter f = function 
      | [] -> []
      | ((k, v) as b, _) :: ll -> if f k v then b $:: (filter f ll) else filter f ll

    let rec map f = function 
      | [] -> []
      | ((k, v), _) :: ll ->  (k, f v) $:: (map f ll)

    let rec find_opt k l =
      match l with
        [] -> None
      | ((k', v), _) :: ll ->
        let c = K.compare k k' in
        if c < 0 then None
        else if c = 0 then Some v
        else find_opt k ll 


    let of_list l = Stdlib.List.fold_left (fun acc (k, v) -> add k v acc) empty l 

    let singleton k v = add k v []

    let to_list = bindings

    let rec dom l =
      match l with
        [] -> []
      | ((k, _), _) :: ll -> 
        Set.($::) k (dom ll)
  end
end : sig 
  module SetList (X : Comparable) : Set with type elt = X.t
  module MapList (K : Comparable) (V : Comparable) : Map with type key = K.t and type value = V.t
end)