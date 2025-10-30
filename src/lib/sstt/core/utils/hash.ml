open Sstt_utils
let enable_hashconsing = true
let print_hashconsing_stats = true

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

module type MEMO =
sig
  type key 
  type 'a t
  val create : string -> 'a t
  val find_opt : 'a t -> key -> 'a option
  val add : 'a t -> key -> 'a -> 'a
end

module Memo1 (K : Hashtbl.HashedType) : MEMO with type key = K.t = 
struct
  type key = K.t
  module T = Hashtbl.Make(K)
  type 'a t = { 
    name : string;
    mutable count_access : int;
    mutable count_uniq : int;
    table : 'a T.t
  }
  let create name = 
    let t = { name; count_access = 0; count_uniq = 0; table = T.create 16 } in
    if enable_hashconsing && print_hashconsing_stats then at_exit (fun () -> 
        Format.eprintf "%s: access: %d, uniq: %d, uniq_ratio: %f\n%!"
          t.name
          t.count_access
          t.count_uniq
          ((float t.count_uniq /. float t.count_access))
      );
    t
  let find_opt =
    if enable_hashconsing then 
      fun t k ->
        t.count_access <- t.count_access + 1;
        match T.find_opt t.table k with
          None -> t.count_uniq <- t.count_uniq + 1; None
        | o -> o
    else 
      fun _ _ -> None
  let add =
    if enable_hashconsing then  
      fun  t k v -> T.add t.table k v; v
    else 
      fun _ _ v -> v

end
module Memo2(K1 : Hashtbl.HashedType)(K2 : Hashtbl.HashedType) : MEMO with type key = K1.t * K2.t =
  Memo1(
  struct
    type t = K1.t * K2.t
    let equal (a1,b1) (a2, b2) = K1.equal a1 a2 && K2.equal b1 b2
    let hash (a, b) = mix (K1.hash a) (K2.hash b)
  end)


(* hash.ml cannot depend on sigs.ml, it would introduce a cyclic dependency *)
module type Comparable =
sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t
  val tname : string
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
  val length : t -> int
end = struct


  type t = (X.t * int) list

  let tname = Printf.sprintf "Hash.List(%s)" X.tname
  let _compare_hash (x1, h1) (x2, h2) =
    let c = Int.compare h1 h2 in
    if c <> 0 then c else X.compare x1 x2

  let _equal_hash (x1, h1) (x2, h2) =
    Int.equal h1 h2 && X.equal x1 x2
  let rec compare l1 l2 = match l1, l2 with
      [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | (x1, h1)::ll1, (x2, h2)::ll2 ->
      Int.compare h1 h2 |> ccmp X.compare x1 x2 |> ccmp compare ll1 ll2
  let rec equal l1 l2 = 
    l1 == l2 (* still implement structural equality in case hashconsing is disabled *)
    || match l1, l2 with
      [], [] -> true
    | _, [] | [], _ -> false
    | (x1, h1)::ll1, (x2, h2)::ll2 -> 
      Int.equal h1 h2 && X.equal x1 x2 && equal ll1 ll2

  let[@inline always] hash = function
      [] -> const0
    | (_, h) :: _ -> h

  module Memo = Memo1(
    struct
      type nonrec t = t
      let equal = equal
      let hash = hash
    end
    )
  let memo = Memo.create (tname ^ ".memo")
  let[@inline always] ($::) x l =
    let ll = (x, mix (hash l) (X.hash x))::l in
    match Memo.find_opt memo ll with
      Some ll -> ll
    | None -> Memo.add memo ll ll

  let of_list l = List.fold_left (fun acc e -> e $:: acc) [] (List.rev l)
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

  let length = List.length
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

  val subset : t -> t -> bool

  val elements : t -> elt list
  (** Return the elements of the set as a sorted list. *)

  val filter : (elt -> bool) -> t -> t
  (** Filtering, [filter f s] is the set of elements of [s] for which [f] returns [true]. *)

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

  val map : (value -> value) -> t -> t
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
  (** [values_for_domain dom def map] returns a map whose domain is [dom] and whose
      value is:
      - taken from [map] if the binding exists there
      - otherwise [def]
  *)

  val values : t -> value list
  (** Return the values of a map. *)

  val constant : Set.t -> value -> t
  (** Return a constant map with from the given domain. *)

  val fold : ('a -> key -> value -> 'a) -> 'a -> t -> 'a
  (** Folding over the bindings of the map. *)

  val is_singleton_opt : t -> (key * value) option
  (** If the map is a singleton, returns its single binding as [Some (k, v)].
      Otherwise returns [None]. *)

  val combine : Set.t -> value list -> t
  (** Create a map from the given domain and values, taken in the order of the
        list.

      @raise Invalid_arugment if the domain and list of values do not
      have the same length.
  *)
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
    let rec inter l1 l2 =
      match l1, l2 with
        ([], _) | (_, []) -> []
      | (x1, _) :: ll1, (x2, _) :: ll2 ->
        let c = X.compare x1 x2 in
        if c < 0 then inter ll1 l2
        else if c = 0 then x1 $:: inter ll1 ll2
        else inter l1 ll2
    let rec diff l1 l2 =
      match l1, l2 with
        ([], _) -> []
      | (l, []) -> l
      | (x1, _) :: ll1, (x2, _) :: ll2 ->
        let c = X.compare x1 x2 in
        if c < 0 then x1 $:: diff ll1 l2
        else if c = 0 then diff ll1 ll2
        else diff l1 ll2
    let cardinal t = Stdlib.List.length t
    let is_empty = Stdlib.List.is_empty
    let subset t1 t2 = diff t1 t2 |> is_empty
  end

  module MapList (K : Comparable) (V : Comparable)
  =
  struct
    type key = K.t
    type value = V.t

    module Set = SetList(K)
    module VL = List(V)
    type t = Set.t * VL.t


    let[@inline always] hash (lk, lv) =
      mix (Set.hash lk) (VL.hash lv)

    let[@inline always] uncons = function
      | [] -> assert false
      | e::l -> e, l

    let add k v (lk, lv) =
      let rec loop lk lv =
        match lk with
          [] -> Set.(k $:: []), VL.(v $:: [])
        | (k',_) :: llk ->
          let c = K.compare k k' in
          if c < 0 then  Set.(k $:: lk), VL.(v $:: lv)
          else
            let (v', _), llv = uncons lv in
            if c = 0 then lk, VL.(v $:: llv)
            else
              let rk, rv = loop llk llv in
              Set.(k' $:: rk), VL.(v' $:: rv)
      in loop lk lv

    let bindings (lk, lv) = Stdlib.List.map2 (fun (k, _) (v, _) -> (k, v)) lk lv

    let compare (lk1, lv1) (lk2, lv2) =
      let c = Set.compare lk1 lk2 in
      if c <> 0 then c else VL.compare lv1 lv2

    let equal ((lk1, lv1) as m1) ((lk2, lv2) as m2) =
      m1 == m2 || (Set.equal lk1 lk2 && VL.equal lv1 lv2)

    let empty = [], []

    let exists f (lk, lv) =
      Stdlib.List.exists2 (fun (k,_) (v, _) -> f k v) lk lv

    let filter f (lk, lv) =
      let rec loop lk lv =
        match lk with
        | [] -> empty
        | (k, _)::llk ->
          let (v, _), llv = uncons lv in
          if f k v then
            let rk, rv = loop  llk llv in
            Set.(k $:: rk), VL.(v $:: rv)
          else
            loop llk llv
      in
      loop lk lv

    let map f (lk, lv) = (lk, VL.map f lv)

    let find_opt k (lk, lv) =
      let rec loop lk lv =
        match lk with
          [] -> None
        | (k', _) :: llk ->
          let c = K.compare k k' in
          if c < 0 then None
          else
            let (v, _), llv = uncons lv in
            if c = 0 then Some v
            else loop llk llv
      in
      loop lk lv

    let of_list l = Stdlib.List.fold_left (fun acc (k, v) -> add k v acc) empty l

    let singleton k v = add k v empty

    let to_list = bindings

    let dom (lk, _) = lk

    let filter_map f (lk, lv) =
      let rec loop lk lv =
        match lk with
          [] -> empty
        | (k, _)::llk ->
          let (v, _), llv = uncons lv in
          match f k v with
            None -> loop llk llv
          | Some v' ->
            let rk, rv = loop llk llv in
            Set.(k $:: rk), VL.(v' $:: rv)
      in loop lk lv

    let cons_opt k vo ((lk, lv) as m) =
      match vo with
        None -> m
      | Some v -> Set.(k $:: lk), VL.(v $:: lv)

    let merge f (lk1, lv1) (lk2, lv2) =
      let rec loop lk1 lv1 lk2 lv2 =
        match lk1 with
          [] -> filter_map (fun k v -> f k None (Some v)) (lk2, lv2)
        | (k1, _)::llk1 ->
          match lk2 with
            [] -> filter_map (fun k v -> f k (Some v) None) (lk1, lv1)
          | (k2, _)::llk2 ->
            let (v1, _), llv1 = uncons lv1 in
            let (v2, _), llv2 = uncons lv2 in
            let c = K.compare k1 k2 in
            if c < 0 then
              let ov1 = f k1 (Some v1) None in
              let r = loop llk1 llv1 lk2 lv2 in
              cons_opt k1 ov1 r
            else if c = 0 then
              let ov = f k1 (Some v1) (Some v2) in
              let r = loop llk1 llv1 llk2 llv2 in
              cons_opt k1 ov r
            else let ov2 = f k2 None (Some v2) in
              let r = loop lk1 lv1 llk2 llv2  in
              cons_opt k2 ov2 r
      in
      loop lk1 lv1 lk2 lv2

    let values (_, lv) = VL.to_list lv

    let values_for_domain s def (lk, lv)=
      let rec loop s lk lv =
        match s with
          []  -> VL.to_list lv
        | (x, _) :: ss ->
          match lk with
          | [] -> Stdlib.List.map (fun _ -> def) s
          | (k, _)::llk ->
            let c = K.compare x k in
            if c < 0 then def :: loop ss lk lv
            else
              let (v, _), llv = uncons lv in
              if c > 0 then
                v :: loop s llk llv
              else
                v :: loop ss llk llv
      in
      loop s lk lv

    let constant dom def =
      (dom, Stdlib.List.fold_left (fun acc _ -> VL.(def $:: acc)) [] dom)

    let fold f acc (lk, lv) =
      Stdlib.List.fold_left2 (fun acc (k, _) (v, _) -> f acc k v) acc lk lv

    let is_singleton_opt (lk, lv) =
      match lk with
        (k, _) :: [] -> let (v, _), _ = uncons lv in Some (k, v)
      | _ -> None

    let combine dom values =
      if (Set.cardinal dom <> Stdlib.List.length values) then invalid_arg "Map.combine";
      dom,VL.of_list values
  end
end : sig
  module SetList (X : Comparable) : Set with type elt = X.t
  module MapList (K : Comparable) (V : Comparable) : Map with type key = K.t and type value = V.t
end)
