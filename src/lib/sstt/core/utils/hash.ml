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

(* Mixes a hash values so that commutative hashes can be computed from it.

*)
let[@inline always] mix_hash h =
  let z = h * 0x1E3779B97F4A7C15 in
  let z = z lxor (z lsr 29) in
  let z = z * 0x3F58476D1CE4E5B9 in
  z lxor (z lsr 32)


(* hash.ml cannot depend on sigs.ml, it would introduce a cyclic dependency
*)
module type Comparable =
sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t
end

(*
: sig 
  type t
  type (_, 'b) hashed = private 'b
  type hash_counter

  type h = private H

  type hashed_list = (h, X.t list) hashed


  val with_counter : (hash_counter -> 'a -> 'b) -> 'a -> hash_counter * 'b
  val (@::) : X.t * hash_counter -> hashed_list -> hashed_list
  val empty_hashed : hashed_list
  val hashed_of_list : hash_counter -> X.t list -> hashed_list 
  val make : hash_counter  -> hashed_list -> t
  val open_ : t -> hash_counter * hashed_list

  include Comparable with type t := t

  val empty : t
  val is_empty : t -> bool
  val of_list : X.t list -> t
  val map : (X.t -> X.t) -> t -> t
  val to_list : t -> X.t list
  val filter : (X.t -> bool) -> t -> t
  val for_all : (X.t -> bool) -> t -> bool
  val length : t -> int

end
*)
module type Set =
sig
  type elt
  type t
  include Comparable with type t:=t

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
  val to_list : t -> (key * value) list
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

include (struct 
  module L (X : Comparable)  = struct

    type t = { hash : int; lst : X.t list }

    type (_, 'b) hashed = 'b
    type hash_counter =  int ref

    type h = private H

    type hashed_list = (h, X.t list) hashed


    let with_counter f l =
      let h = ref const0 in
      let r = f h l in
      h, r


    let hash s = s.hash land 0x3fffffff
    let[@inline] add_sum sum x = sum + mix_hash (X.hash x)
    let[@inline] rem_sum sum x = sum - mix_hash (X.hash x)

    let (@::) (x, h) l =
      h := add_sum !h x;
      x :: l

    let empty_hashed = []

    let open_ l = ref l.hash, l.lst
    let update h x = h := add_sum !h x


    let hashed_of_list c l = 
      List.iter (update c) l |> ignore; l

    let make hash lst = { hash = !hash; lst }
    let mk (h, l) = make h l

    let empty = with_counter (fun _ x -> x) [] |> mk

    let is_empty = function { lst = []; _ } -> true | _ -> false

    let compare l1 l2  =
      let c = Int.compare l1.hash l2.hash in
      if c <> 0 then c else List.compare X.compare l1.lst l2.lst

    let equal l1 l2 = 
      l1.hash == l2.hash && List.equal X.equal l1.lst l2.lst

    let update h x =  h := add_sum !h x


    let of_list lst =
      with_counter (hashed_of_list) lst |> mk

    let map f l =
      with_counter (fun h l -> List.map (fun x -> let y = f x in update h y; y) l) l.lst
      |> mk

    let to_list l = l.lst

    let filter f l =
      with_counter (fun h l -> 
          List.filter (fun x -> 
              let b = f x in
              if b then update h x;
              b) l) l.lst 
      |> mk

    let for_all f l = List.for_all f l.lst

    let length l = List.length l.lst
  end


  module SetList ( X : Comparable ) =
  struct
    type elt = X.t
    include L(X)
    let elements = to_list

    let rec mem_list x l =
      match l with
        [] -> false
      | y :: ll ->
        let c = X.compare y x in
        if c < 0 then mem_list x ll
        else c = 0

    let mem x l = mem_list x (to_list l)

    let rec add_aux h x l =
      match l with
        []  -> (x, h) @:: empty_hashed
      | y :: ll ->
        let c = X.compare x y in
        if c < 0 then (x, h) @:: (hashed_of_list h l)
        else if c = 0 then (hashed_of_list h l)
        else y :: add_aux h x ll

    let add x l = 
      let h = init_hash () in
      let lst = add_aux h x l in
      { hash = !h; lst }

    let of_list l =
      let l = Stdlib.List.sort_uniq X.compare l in
      (* overwritten of_list preserves the order *)
      of_list l

    let map f l = of_list (Stdlib.List.map f l.lst)

    let rec union_aux h l1 l2 =
      match l1, l2 with
        [], l | l, [] -> Stdlib.List.iter (update h) l; l
      | x1 :: ll1, x2::ll2 ->
        let c = X.compare x1 x2 in
        if c < 0 then (update h x1; x1 :: union_aux h ll1 l2)
        else if c = 0 then (update h x1; x1 :: union_aux h ll1 ll2)
        else (update h x2; x2 :: union_aux h l1 ll2)

    let union l1 l2 =
      let h = init_hash () in
      let lst = union_aux h l1.lst l2.lst in 
      { hash = !h; lst }

    let rec inter_aux h l1 l2 =
      match l1, l2 with
        [], _ | _, [] -> []
      | x1 :: ll1, x2 :: ll2 ->
        let c = X.compare x1 x2 in
        if c < 0 then inter_aux h ll1 l2
        else if c = 0 then (update h x1; x1:: inter_aux h ll1 ll2)
        else inter_aux h l1 ll2

    let inter l1 l2 = inter_aux h l1 l2


    let rec diff3 l1 h1 l2 =
      match l1 with
        [] -> empty
      | x1 :: ll1 ->
        match l2 with
          [] -> (l1, h1)
        | x2 :: ll2 ->
          match h1 with
            _ :: hh1 ->
            let c = X.compare x1 x2 in
            if c < 0 then x1 $:: diff3 ll1 hh1 l2
            else if c = 0 then diff3 ll1 hh1 ll2
            else diff3 l1 h1 ll2
          | _ -> assert false

    let diff (l1, h1) (l2, _) = diff3 l1 h1 l2

    let cardinal (l, _) = Stdlib.List.length l
    let subset l1 l2 = diff l1 l2 |> is_empty
  end

  module MapList (K : Comparable) (V : Comparable)
  =
  struct
    type key = K.t
    type value = V.t

    module Keys = SetList(K)
    module Set = Keys
    module Values = L(V)
    type t = Keys.t * Values.t


    let[@inline always] hash (lk, lv) =
      mix (Keys.hash lk) (Values.hash lv)


    let rec add4 k v lk hk lv hv =
      match lk with
        [] -> Keys.(k $:: empty), Values.(v  $:: empty)
      | k' :: llk ->
        let c = K.compare k k' in
        if c < 0 then  Keys.(k $:: (lk, hk)), Values.(v $:: (lv,hv))
        else
          match lv with
            [] -> assert false
          | v' :: llv ->
            match hk, hv with
              _::hhk, _::hhv ->
              if c = 0 then
                (lk,hk), Values.(v $:: (llv, hhv))
              else
                let rk, rv = add4 k v llk hhk llv hhv in
                Keys.(k' $:: rk), Values.(v' $:: rv)
            | _ -> assert false

    let add k v ((lk, hk), (lv, hv)) = add4 k v lk hk lv hv

    let bindings ((lk,_), (lv,_)) =
      Stdlib.List.map2 (fun k v -> (k, v)) lk lv

    let compare (lk1, lv1) (lk2, lv2) =
      let c = Keys.compare lk1 lk2 in
      if c <> 0 then c else Values.compare lv1 lv2

    let equal ((lk1, lv1) as m1) ((lk2, lv2) as m2) =
      m1 == m2 || (Keys.equal lk1 lk2 && Values.equal lv1 lv2)

    let empty = Keys.empty, Values.empty

    let exists f ((lk,_), (lv,_)) =
      Stdlib.List.exists2 f lk lv

    let rec filter2 f lk lv =
      match lk with
      | [] -> empty
      | k::llk ->
        match lv with
          [] -> assert false
        | v :: llv ->
          if f k v then
            let rk, rv = filter2 f llk llv in
            Keys.(k $:: rk), Values.(v $:: rv)
          else
            filter2 f llk llv

    let filter f ((lk, _),(lv, _)) = filter2 f lk lv

    let map f ((lk, _) as k, (lv,_) ) =
      (k,
       Stdlib.List.map2 f lk lv |> Values.of_list)

    let rec find_opt2 k lk lv =
      match lk with
        [] -> None
      | k' :: llk ->
        let c = K.compare k k' in
        if c < 0 then None
        else
          match lv with
            v :: llv ->
            if c = 0 then Some v
            else find_opt2 k llk llv
          | _ -> assert false

    let find_opt k ((lk, _),(lv, _)) = find_opt2 k lk lv

    let of_list l = Stdlib.List.fold_left (fun acc (k, v) -> add k v acc) empty l

    let singleton k v = add k v empty

    let to_list = bindings

    let dom (lk, _) = lk

    let rec filter_map2 f lk lv =
      match lk with
        [] -> empty
      | k::llk ->
        match lv with
          [] -> assert false
        | v :: llv ->
          match f k v with
            None -> filter_map2 f llk llv
          | Some v' ->
            let rk, rv = filter_map2 f llk llv in
            Keys.(k $:: rk), Values.(v' $:: rv)

    let filter_map f ((lk, _), (lv, _)) = filter_map2 f lk lv

    let cons_opt k vo ((lk, lv) as m) =
      match vo with
        None -> m
      | Some v -> Keys.(k $:: lk), Values.(v $:: lv)

    let rec merge4 f lk1 lv1 lk2 lv2 =
      match lk1 with
        [] -> filter_map2 (fun k v -> f k None (Some v)) lk2 lv2
      | k1::llk1 ->
        match lk2 with
          [] -> filter_map2 (fun k v -> f k (Some v) None) lk1 lv1
        | k2 ::llk2 ->
          match lv1, lv2 with
            v1 :: llv1, v2 :: llv2 ->
            let c = K.compare k1 k2 in
            if c < 0 then
              let ov1 = f k1 (Some v1) None in
              let r = merge4 f llk1 llv1 lk2 lv2 in
              cons_opt k1 ov1 r
            else if c = 0 then
              let ov = f k1 (Some v1) (Some v2) in
              let r = merge4 f llk1 llv1 llk2 llv2 in
              cons_opt k1 ov r
            else let ov2 = f k2 None (Some v2) in
              let r = merge4 f lk1 lv1 llk2 llv2 in
              cons_opt k2 ov2 r
          | _ -> assert false

    let merge f ((lk1, _),(lv1, _)) ((lk2, _),(lv2, _)) =
      merge4 f lk1 lv1 lk2 lv2

    let values (_, lv) = Values.to_list lv

    let rec values_for_domain3 def ls lk lv =
      match ls with
        []  -> lv
      | x :: lls ->
        match lk with
        | [] -> Stdlib.List.map (fun _ -> def) ls
        | k::llk ->
          let c = K.compare x k in
          if c < 0 then def :: values_for_domain3 def lls lk lv
          else
            match lv with
              v :: llv ->
              if c > 0 then
                v :: values_for_domain3 def ls llk llv
              else
                v :: values_for_domain3 def lls llk llv
            | _ -> assert false

    let values_for_domain (ls, _) def ((lk, _),(lv, _)) =
      values_for_domain3 def ls lk lv

    let init ((l, _) as dom) f =
      (dom, Stdlib.List.map f l |> Values.of_list)

    let constant ((l, _) as dom) def =
      (dom, Stdlib.List.fold_left (fun acc _ -> Values.(def $:: acc)) Values.empty l)

    let fold f acc ((lk,_), (lv,_)) =
      Stdlib.List.fold_left2 f acc lk lv

    let is_singleton_opt ((lk, _), (lv, _)) =
      match lk with
        [] -> None
      | k ::_ ->
        match lv with
          v :: _ -> Some (k, v)
        | _ -> assert false

    let combine ((l, _) as dom) values =
      if Stdlib.List.compare_lengths l values <> 0 then invalid_arg "Map.combine";
      dom, Values.of_list values

    let rec remove5 lk hk lv hv ls =
      match lk with
        [] -> empty
      | k :: llk ->
        match ls with
          [] -> (lk,hk), (lv, hv)
        | s :: lls ->
          let c = K.compare k s in
          match hk, lv, hv with
            _::hhk, v :: llv, _ :: hhv ->
            if c < 0 then 
              let rk, rv = remove5 llk hhk llv hhv ls in
              Keys.(k $:: rk), Values.(v $:: rv)
            else if c = 0 then remove5 llk hhk llv hhv lls
            else remove5 lk hk lv hv lls
          | _ -> assert false

    let remove ((lk, hk), (lv, hv)) (ls, _) = remove5 lk hk lv hv ls
    let for_all f ((lk, _), (lv, _)) = Stdlib.List.for_all2 f lk lv
  end
  module List = L
end : sig
           module List : sig 
             type t
             type (_, 'b) hashed = private 'b
             type hash_counter

             type h = private H

             type hashed_list = (h, X.t list) hashed


             val with_counter : (hash_counter -> 'a -> 'b) -> 'a -> hash_counter * 'b
             val (@::) : X.t * hash_counter -> hashed_list -> hashed_list
             val empty_hashed : hashed_list
             val hashed_of_list : hash_counter -> X.t list -> hashed_list 
             val make : hash_counter  -> hashed_list -> t
             val open_ : t -> hash_counter * hashed_list

             include Comparable with type t := t

             val empty : t
             val is_empty : t -> bool
             val of_list : X.t list -> t
             val map : (X.t -> X.t) -> t -> t
             val to_list : t -> X.t list
             val filter : (X.t -> bool) -> t -> t
             val for_all : (X.t -> bool) -> t -> bool
             val length : t -> int

           end
           module SetList(X:Comparable) : Set with type elt = X.t
           module MapList (K : Comparable) (V : Comparable) : Map with type key = K.t and type value = V.t
         end)