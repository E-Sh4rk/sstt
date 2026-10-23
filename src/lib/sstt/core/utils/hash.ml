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
  let z = h * 0x2c1b3c6d in
  let z = z lxor (z lsr 15) in
  let z = z * 0x297a2d39 in
  z lxor (z lsr 13)

(* hash.ml cannot depend on sigs.ml, it would introduce a cyclic dependency
*)
module type Comparable =
sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t
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

module List (X : Comparable) : sig
  type t

  include Comparable with type t := t

  val empty : t
  val is_empty : t -> bool
  val of_list : X.t list -> t
  val ($::) : X.t -> t -> t
  val map : (X.t -> X.t) -> t -> t
  val to_list : t -> X.t list
  val filter : (X.t -> bool) -> t -> t
  val for_all : (X.t -> bool) -> t -> bool
  val length : t -> int

  type 's hash
  type 's hd = private X.t
  type 's lst = private X.t list
  type 's view = [] | (::) of 's hd * 's lst
  type 'r op = { op : 's. 's hash -> 's lst -> 'r }

  val create : 'r op -> 'r
  val update : t -> 'r op -> 'r

  val view : 's lst -> 's view
  val empty_lst : 's lst
  val to_lst : 's hash -> X.t list -> 's lst
  val cons : 's hash -> X.t -> 's lst -> 's lst
  val tail : 's hash -> 's lst -> 's lst
  val (@::) :  's hd -> 's lst -> 's lst
  val make : 's hash -> 's lst -> t 

end = struct 
  type 's hd = X.t
  type 's lst = X.t list

  type t = { hash : int; lst : X.t list } 

  type 's hash = int ref
  type 's view =  [] | (::) of 's hd * 's lst

  type 'r op = { op : 's. 's hash -> 's lst -> 'r }

  let empty = { hash = const0; lst = [] }

  let make h lst = { hash = !h; lst }

  let view : X.t list -> 's view = 
    function List.[] -> []
           | List.(x :: l) -> x :: l

  let empty_lst = List.[]

  let hash s = s.hash land 0x3fffffff
  let[@inline] add_sum sum x = sum + mix_hash (X.hash x)
  let[@inline] rem_sum sum x = sum - mix_hash (X.hash x)

  (* unsafe *)
  let upd h x = h := add_sum !h x

  let update { hash; lst } { op } =
    let h = ref hash in
    op h lst 

  let create op = update empty op

  let equal l1 l2 = 
    l1 == l2 ||
    (l1.hash == l2.hash && 
     (l1.lst == l2.lst || List.equal X.equal l1.lst l2.lst))

  let compare l1 l2 = 
    if l1 == l2 then 0 else
      let c = Int.compare l1.hash l2.hash in
      if c <> 0 then c else List.compare X.compare l1.lst l2.lst

  let cons h x l =
    h := add_sum !h x;
    List.(x :: l)

  let tail h l =
    match l with
      List.[] -> l
    | x :: ll -> h := rem_sum !h x; ll

  let (@::) x l = List.(x :: l)
  let ($::) x l = update l { op = fun h e -> cons h x e |> make h }

  let to_lst (type s) (h : s hash) (l : X.t list) : s lst =
    List.iter (upd h) l;
    l

  let is_empty = function { lst = []; _ } -> true | _ -> false


  let of_list l =
    let h = ref const0 in
    let lst = to_lst h l in
    { hash = !h; lst }

  let map_aux h empty f (l: X.t list) =
    let rec loop l = match view l with
        [] -> empty
      | x :: ll -> cons h (f x) (loop ll)
    in
    let lst = loop l in make h lst

  let map f l =
    create { op = (let f h empty = map_aux h empty f l.lst in f) }

  let to_list l = l.lst

  let filter_aux h empty f l =
    let rec loop l = match view l with
        [] -> empty
      | x :: ll -> 
        if f x then cons h x (loop ll) else loop ll
    in 
    let lst = loop l in
    make h lst

  let filter f l =
    create { op = (let f h empty = filter_aux h empty f l.lst in f)}

  let for_all f l = List.for_all f l.lst

  let length l = List.length l.lst

end




module SetList ( X : Comparable ) =
struct
  type elt = X.t
  include List (X) 

  let elements = to_list

  let rec mem_list x l =
    match l with
      Stdlib.List.[] -> false
    | y :: ll ->
      let c = X.compare y x in
      if c < 0 then mem_list x ll
      else c = 0

  let mem x l = mem_list x (to_list l)
  let rec add_aux h x l =
    match view l with
      []  -> cons h x empty_lst
    | y :: ll ->
      let c = X.compare x (y : _ hd :> X.t) in
      if c < 0 then cons h x l
      else if c = 0 then l
      else y @:: add_aux h x ll

  let add x l =
    update l { op = fun h l -> let lst = add_aux h x l in make h lst  }

  let of_list l =
    let l = Stdlib.List.sort_uniq X.compare l in
    (* overwritten of_list preserves the order *)
    of_list l

  let map f l = of_list (Stdlib.List.map f (to_list l))

  let rec union_aux h l1 l2 =
    match l1, l2 with
      Stdlib.List.([], l | l, [])  -> to_lst h l
    | x1 :: ll1, x2::ll2 ->
      let c = X.compare x1 x2 in
      if c < 0 then cons h x1 (union_aux h ll1 l2)
      else if c = 0 then cons h x1 (union_aux h ll1 ll2)
      else cons h x2 (union_aux h l1 ll2)

  let union l1 l2 =
    let op h _empty : t =  
      let lst = union_aux h (to_list l1) (to_list l2) in
      make h lst 
    in
    create { op }

  let rec inter_aux h  l1 l2 =
    match l1, l2 with
      Stdlib.List.([],_ | _, []) -> empty_lst
    | x1 :: ll1, x2 :: ll2 ->
      let c = X.compare x1 x2 in
      if c < 0 then inter_aux h ll1 l2
      else if c = 0 then cons h x1 (inter_aux h ll1 ll2)
      else inter_aux h l1 ll2

  let inter l1 l2 = 
    let op h _ : t =  
      let lst = inter_aux h (to_list l1) (to_list l2) in
      make h lst 
    in
    create { op }

  let rec diff_aux h l1 l2 =
    match l1, l2 with
      Stdlib.List.(([] as l ,_) | (l, []))  -> to_lst h l
    | x1 :: ll1, x2 :: ll2 ->
      let c = X.compare x1 x2 in
      if c < 0 then cons h x1 (diff_aux h ll1 l2)
      else if c = 0 then diff_aux h ll1 ll2
      else diff_aux h l1 ll2

  let diff l1 l2 =     
    let op h _empty : t =  
      let lst = diff_aux h (to_list l1) (to_list l2) in
      make h lst 
    in
    create { op }



  let cardinal l = Stdlib.List.length (to_list l)
  let rec subset_list l1 l2 =
    match l1, l2 with
      Stdlib.List.[], _ -> true
    | _, Stdlib.List.[] -> false
    | x1 :: ll1, x2 :: ll2 ->
      let c = X.compare x1 x2 in
      if c < 0 then false
      else if c = 0 then subset_list ll1 ll2
      else subset_list l1 ll2

  let subset l1 l2 = subset_list (to_list l1) (to_list l2)
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

  let rec add_aux k v hk lk hv lv =
    match Keys.view lk, Values.view lv with
      [], [] -> Keys.(cons hk k empty_lst), 
                Values.(cons hv v empty_lst)
    | k' :: llk, v'::llv ->
      let c = K.compare k (k' : _ Keys.hd :> key) in
      if c < 0 then Keys.(cons hk k lk), Values.(cons hv v lv)
      else if c = 0 then lk, Values.(cons hv v llv)
      else let rk, rv = add_aux k v hk llk hv llv in
        Keys.(k' @:: rk), Values.(v' @:: rv)
    | _ -> assert false

  let add k v (lk, lv) =
    let opk hk lk =
      let opv hv lv = 
        let rk, rv = add_aux k v hk lk hv lv in
        Keys.make hk rk,
        Values.make hv rv
      in
      Values.update lv { op=opv}
    in
    Keys.update lk {op=opk}


  let bindings (lk, lv) =
    Stdlib.List.map2 (fun k v -> (k, v))
      (Keys.to_list lk)
      (Values.to_list lv)

  let compare (lk1, lv1) (lk2, lv2) =
    let c = Keys.compare lk1 lk2 in
    if c <> 0 then c else Values.compare lv1 lv2

  let equal ((lk1, lv1) as m1) ((lk2, lv2) as m2) =
    m1 == m2 || (Keys.equal lk1 lk2 && Values.equal lv1 lv2)

  let empty = Keys.empty, Values.empty

  let exists f (lk, lv) =
    Stdlib.List.exists2 f (Keys.to_list lk) (Values.to_list lv)

  let rec filter_aux f hk (lk : _ list) hv (lv : _ list) =
    match lk, lv with
      [], [] -> Keys.empty_lst, Values.empty_lst
    | k :: llk, v::llv ->
      if f k v then
        let rk, rv = filter_aux f hk llk hv llv in
        Keys.(cons hk k rk), Values.(cons hv v rv)
      else filter_aux f hk llk hv llv
    | _ -> assert false


  let filter f (lk, lv) =
    let opk hk _ =
      let opv hv _ = 
        let rk, rv = 
          filter_aux f 
            hk (Keys.to_list lk) 
            hv (Values.to_list lv) in
        Keys.make hk rk,
        Values.make hv rv
      in
      Values.create { op=opv}
    in
    Keys.create {op=opk}

  let map f (lk, lv) =
    (lk,
     Stdlib.List.map2 f (Keys.to_list lk) (Values.to_list lv)
     |> Values.of_list)

  let rec find_opt_list k lk lv =
    match lk with
      [] -> None
    | k' :: llk ->
      let c = K.compare k k' in
      if c < 0 then None
      else
        match lv with
          v :: llv ->
          if c = 0 then Some v
          else find_opt_list k llk llv
        | _ -> assert false

  let find_opt k (lk, lv) = find_opt_list k (Keys.to_list lk) (Values.to_list lv)

  let of_list l = Stdlib.List.fold_left (fun acc (k, v) -> add k v acc) empty l

  let singleton k v = add k v empty

  let to_list = bindings

  let dom (lk, _) = lk

  let rec filter_map_aux f hk (lk : _ list) hv (lv : _ list) =
    match lk, lv with
      [], [] -> Keys.empty_lst, Values.empty_lst
    | k :: llk, v::llv -> begin
        match f k v with
          None ->filter_map_aux f hk llk hv llv
        | Some v' ->
          let rk, rv = filter_map_aux f hk llk hv llv in
          Keys.(cons hk k rk), Values.(cons hv v' rv)
      end
    | _ -> assert false

  let filter_map f (lk, lv) =
    let opk hk _ =
      let opv hv _ = 
        let rk, rv = 
          filter_map_aux f 
            hk (Keys.to_list lk) 
            hv (Values.to_list lv) in
        Keys.make hk rk,
        Values.make hv rv
      in
      Values.create {op=opv}
    in
    Keys.create {op=opk}

  let cons_opt k vo hk lk hv lv =
    match vo with
      None -> (lk, lv)
    | Some v -> 
      Keys.(cons hk k lk), Values.(cons hv v lv)


  let rec merge_aux f hk hv lk1 lv1 lk2 lv2  =
    match lk1, lk2 with
      [], _ -> filter_map_aux (fun k v -> f k None (Some v)) hk lk2 hv lv2
    |_, [] -> filter_map_aux (fun k v -> f k (Some v) None) hk lk1 hv lv1
    | k1::llk1, k2 ::llk2 ->
      match lv1, lv2 with
        v1 :: llv1, v2 :: llv2 ->
        let c = K.compare k1 k2 in
        if c < 0 then
          let ov1 = f k1 (Some v1) None in
          let rk, rv = merge_aux f hk hv llk1 llv1 lk2 lv2 in
          cons_opt k1 ov1 hk rk hv rv
        else if c = 0 then
          let ov = f k1 (Some v1) (Some v2) in
          let rk, rv = merge_aux f hk hv llk1 llv1 llk2 llv2 in
          cons_opt k1 ov hk rk hv rv
        else let ov2 = f k2 None (Some v2) in
          let rk, rv = merge_aux f hk hv lk1 lv1 llk2 llv2 in
          cons_opt k2 ov2 hk rk hv rv
      | _ -> assert false

  let merge f (lk1, lv1) (lk2, lv2) =
    let opk hk _ =
      let opv hv _ = 
        let rk, rv = 
          merge_aux f 
            hk  hv 
            (Keys.to_list lk1) 
            (Values.to_list lv1)
            (Keys.to_list lk2) 
            (Values.to_list lv2)
        in
        Keys.make hk rk,
        Values.make hv rv
      in
      Values.create {op=opv}
    in
    Keys.create {op=opk}


  let values (_, lv) = Values.to_list lv

  let rec values_for_domain_aux def ls lk lv =
    match ls with
      []  -> lv
    | x :: lls ->
      match lk, lv with
      | [], _ -> Stdlib.List.map (fun _ -> def) ls
      | k::llk,v :: llv ->
        let c = K.compare x k in
        if c < 0 then def :: values_for_domain_aux def lls lk lv
        else if c > 0 then
          v :: values_for_domain_aux def ls llk llv
        else
          v :: values_for_domain_aux def lls llk llv
      | _ -> assert false

  let values_for_domain ls def (lk, lv) =
    values_for_domain_aux def (Keys.to_list ls) (Keys.to_list lk) (Values.to_list lv)

  let init dom f =
    (dom, Stdlib.List.map f (Keys.to_list dom) |> Values.of_list)

  let constant dom def =
    (dom, Stdlib.List.fold_left (fun acc _ -> def :: acc) [] (Keys.to_list dom) |> Values.of_list)

  let fold f acc (lk, lv)=
    Stdlib.List.fold_left2 f acc (Keys.to_list lk) (Values.to_list lv)

  let is_singleton_opt (lk, lv) =
    let lk = Keys.to_list lk in
    let lv = Values.to_list lv in
    match lk with
      [] -> None
    | k ::_ ->
      match lv with
        v :: _ -> Some (k, v)
      | _ -> assert false

  let combine dom values =
    if Stdlib.List.compare_lengths (Keys.to_list dom) values <> 0 
    then invalid_arg "Map.combine";
    dom, Values.of_list values

  let rec remove_aux hk lk hv lv ls =
    match Keys.view lk with
      [] -> Keys.empty_lst, Values.empty_lst
    | k :: llk ->
      match ls, Values.view lv with
        [], _ -> (lk, lv)
      | s :: lls, v :: llv ->
        let c = K.compare (k : _ Keys.hd :> key) s in
        if c < 0 then 
          let rk, rv = remove_aux hk llk hv llv ls in
          Keys.(k @:: rk), Values.(v @:: rv)
        else if c = 0 then remove_aux hk (Keys.tail hk lk) hv (Values.tail hv lv) lls
        else remove_aux hk lk hv lv lls
      | _ -> assert false

  let remove (lk, lv) ls =
    let op hk lk =
      let op hv lv =
        let rk, rv = remove_aux hk lk hv lv (Keys.to_list ls) in
        Keys.make hk rk, Values.make hv rv
      in
      Values.update lv {op}
    in Keys.update lk {op}

  let for_all f (lk, lv) = 
    Stdlib.List.for_all2 f 
      (Keys.to_list lk) 
      (Values.to_list lv)
end
