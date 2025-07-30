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
    h1 == h2 && X.equal x1 x2
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