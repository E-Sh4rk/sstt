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




(* hash.ml cannot depend on sigs.ml, it would introduce a cyclic dependency 
*)
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
    | (_, h1)::ll1, (_, h2)::ll2 -> Int.compare h1 h2 |> ccmp compare ll1 ll2
  let rec equal l1 l2 = 
    l1 == l2 (* still implement structural equality in case hashconsing is disabled *)
    || match l1, l2 with
      [], [] -> true
    | _, [] | [], _ -> false
    | (x1, h1)::ll1, (x2, h2)::ll2 -> 
      h1 == h2 && X.equal x1 x2 && equal ll1 ll2

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