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

module List (X : Comparable) = struct

  type t = X.t list * int list
  let empty = [], []
  let is_empty = function ([], _) -> true | _ -> false
  let uncons = function
      [], [] -> None
    | x::l, _::h -> Some (x, (l, h))
    | _ -> assert false

  let rec compare4 l1 h1 l2 h2 =
    match h1, h2 with
      [],[] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | i1 :: hh1, i2 :: hh2 ->
      let c = Int.compare i1 i2 in
      if c <> 0 then c else
        match l1, l2 with
          x1 :: ll1, x2 :: ll2 ->
          let c = X.compare x1 x2 in
          if c <> 0 then c else compare4 ll1 hh1 ll2 hh2
        | _ -> assert false

  let compare (l1, h1) (l2, h2) = compare4 l1 h1 l2 h2

  let rec equal4 l1 h1 l2 h2 =
    match l1, h1, l2, h2 with
      [], _, [], _ -> true
    | x1::ll1, i1::hh1, x2::ll2, i2::hh2 ->
      i1 == i2 && X.equal x1 x2 && equal4 ll1 hh1 ll2 hh2
    | _ -> false

  let equal (l1, h1) (l2, h2) = equal4 l1 h1 l2 h2

  let[@inline always] get_hash = function
      [] -> const0
    | h :: _ -> h

  let[@inline always] combine_hash h x =
    (mix (get_hash h) (X.hash x)) :: h

  let[@inline always] hash (_, h) = get_hash h

  let[@inline always] ($::) x (l, h) =
    (x :: l,combine_hash h x)

  let of_list l =
    l, l |> List.rev |> List.fold_left combine_hash []

  let map f (l, _) =
    List.rev_map f l |>
    List.fold_left (fun acc x -> x $:: acc) empty

  let to_list = fst

  let rec filter2 f l h =
    match l, h with
      [], _ -> empty
    | x :: ll, _ :: hh ->
      let rl, rh = filter2 f ll hh in
      if f x then
        if rl == ll && rh == hh then (l, h) else x $:: (rl, rh)
      else (rl, rh)
    | _ -> assert false

  let filter f (l, h) = filter2 f l h
  let for_all f (l, _) = List.for_all f l

  let length (l, _ )= List.length l
end


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
  val map : (value -> value) -> t -> t
  val filter_map : (key -> value -> value option) -> t -> t
  val of_list : (key * value) list -> t
  val singleton : key -> value -> t
  val to_list : t -> (key * value) list
  val merge :
    (key -> value option -> value option -> value option) -> t -> t -> t
  val values_for_domain : Set.t -> value -> t -> value list
  val values : t -> value list
  val constant : Set.t -> value -> t
  val fold : ('a -> key -> value -> 'a) -> 'a -> t -> 'a
  val is_singleton_opt : t -> (key * value) option
  val combine : Set.t -> value list -> t
end


module SetList ( X : Comparable ) =
struct
  type elt = X.t
  include List(X)
  let elements = to_list

  let rec mem_list x l =
    match l with
      [] -> false
    | y :: ll ->
      let c = X.compare y x in
      if c < 0 then mem_list x ll
      else c = 0

  let mem x (l, _) = mem_list x l

  let rec add2 x l h =
    match l with
      []  -> x $:: empty
    | y :: ll ->
      let c = X.compare x y in
      if c < 0 then x $:: (l, h)
      else if c = 0 then (l, h)
      else match h with
          _ :: hh ->  y $:: add2 x ll hh
        | _ -> assert false

  let add x (l, h) = add2 x l h

  let of_list l =
    Stdlib.List.fold_left (fun acc x -> add x acc) empty l

  let map f (l,_) =
    Stdlib.List.fold_left (fun acc x -> add (f x) acc) empty l

  let rec union4 l1 h1 l2 h2 =
    match l1 with
      [] -> (l2, h2)
    | x1 :: ll1 ->
      match l2 with
        [] -> (l1, h1)
      | x2 :: ll2 ->
        match h1, h2 with
          (_ :: hh1), (_ :: hh2) ->
          let c = X.compare x1 x2 in
          if c < 0 then x1 $:: union4 ll1 hh1 l2 h2
          else if c = 0 then x1 $:: union4 ll1 hh1 ll2 hh2
          else x2 $:: union4 l1 h1 ll2 hh2
        | _ -> assert false
  let union (l1, h1) (l2, h2) = union4 l1 h1 l2 h2

  let rec inter4 l1 h1 l2 h2 =
    match l1 with
      [] -> empty
    | x1 :: ll1 ->
      match l2 with
        [] -> empty
      | x2 :: ll2 ->
        match h1, h2 with
          (_ :: hh1, _ :: hh2) ->
          let c = X.compare x1 x2 in
          if c < 0 then inter4 ll1 hh1 l2 h2
          else if c = 0 then x1 $:: inter4 ll1 hh1 ll2 hh2
          else inter4 l1 h1 ll2 hh2
        | _ -> assert false

  let inter (l1, h1) (l2, h2) = inter4 l1 h1 l2 h2

  let rec diff4 l1 h1 l2 h2 =
    match l1 with
      [] -> empty
    | x1 :: ll1 ->
      match l2 with
        [] -> (l1, h1)
      | x2 :: ll2 ->
        match h1, h2 with
          (_ :: hh1, _ :: hh2) ->
          let c = X.compare x1 x2 in
          if c < 0 then x1 $:: diff4 ll1 hh1 l2 h2
          else if c = 0 then diff4 ll1 hh1 ll2 hh2
          else diff4 l1 h1 ll2 hh2
        | _ -> assert false

  let diff (l1, h1) (l2, h2) = diff4 l1 h1 l2 h2

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
  module Values = List(V)
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
    Stdlib.List.exists2 (fun k v -> f k v) lk lv

  let rec filter2 f lk lv =
    match lk with
    | [] -> Keys.empty, Values.empty
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

  let map f (lk, lv) = (lk, Values.map f lv)

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
      [] -> Keys.empty, Values.empty
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
end
