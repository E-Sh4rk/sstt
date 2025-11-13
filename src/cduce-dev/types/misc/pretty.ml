let std_compare = compare

type 'a regexp =
  | Empty
  | Epsilon
  | Seq of 'a regexp * 'a regexp
  | Alt of 'a regexp * 'a regexp
  | Star of 'a regexp
  | Plus of 'a regexp
  | Trans of 'a

module type TABLE = sig
  type key
  type 'a t

  val create : int -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
end

module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Decompile (H : TABLE) (S : S) = struct
  module B = struct
    type re =
      | RSeq of re list
      | RAlt of re list
      | RTrans of S.t
      | RStar of re
      | RPlus of re

    let rec compare s1 s2 =
      if s1 == s2 then 0
      else
        match (s1, s2) with
        | RSeq x, RSeq y
        | RAlt x, RAlt y ->
            compare_list x y
        | RSeq _, _ -> -1
        | _, RSeq _ -> 1
        | RAlt _, _ -> -1
        | _, RAlt _ -> 1
        | RTrans x, RTrans y -> S.compare x y
        | RTrans _, _ -> -1
        | _, RTrans _ -> 1
        | RStar x, RStar y
        | RPlus x, RPlus y ->
            compare x y
        | RStar _, _ -> -1
        | _, RStar _ -> 1

    and compare_list l1 l2 =
      match (l1, l2) with
      | x1 :: y1, x2 :: y2 ->
          let c = compare x1 x2 in
          if c = 0 then compare_list y1 y2 else c
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1

    let[@ocaml.warning "-32"] rec dump ppf = function
      | RSeq l -> Format.fprintf ppf "Seq(%a)" dump_list l
      | RAlt l -> Format.fprintf ppf "Alt(%a)" dump_list l
      | RStar r -> Format.fprintf ppf "Star(%a)" dump r
      | RPlus r -> Format.fprintf ppf "Plus(%a)" dump r
      | RTrans _ -> Format.fprintf ppf "Trans"

    and[@ocaml.warning "-32"] dump_list ppf = function
      | [] -> ()
      | [ h ] -> Format.fprintf ppf "%a" dump h
      | h :: t -> Format.fprintf ppf "%a,%a" dump h dump_list t

    let rec factor accu l1 l2 =
      match (l1, l2) with
      | x1 :: y1, x2 :: y2 when compare x1 x2 = 0 -> factor (x1 :: accu) y1 y2
      | l1, l2 -> (accu, l1, l2)

    let rec regexp = function
      | RSeq l ->
          let rec aux = function
            | [ h ] -> regexp h
            | h :: t -> Seq (regexp h, aux t)
            | [] -> Epsilon
          in
          aux l
      | RAlt l ->
          let rec aux = function
            | [ h ] -> regexp h
            | h :: t -> Alt (regexp h, aux t)
            | [] -> Empty
          in
          aux l
      | RTrans x -> Trans x
      | RStar r -> Star (regexp r)
      | RPlus r -> Plus (regexp r)

    let epsilon = RSeq []
    let empty = RAlt []
    let rtrans t = RTrans t

    let rec nullable = function
      | RAlt l -> List.exists nullable l
      | RSeq l -> List.for_all nullable l
      | RPlus r -> nullable r
      | RStar _ -> true
      | RTrans _ -> false

    let has_epsilon =
      List.exists (function
        | RSeq [] -> true
        | _ -> false)

    let remove_epsilon =
      List.filter (function
        | RSeq [] -> false
        | _ -> true)

    let rec merge l1 l2 =
      match (l1, l2) with
      | x1 :: y1, x2 :: y2 ->
          let c = compare x1 x2 in
          if c = 0 then x1 :: merge y1 y2
          else if c < 0 then x1 :: merge y1 l2
          else x2 :: merge l1 y2
      | [], l
      | l, [] ->
          l

    let rec sub l1 l2 =
      compare l1 l2 = 0
      ||
      match (l1, l2) with
      | RSeq [ x ], y -> sub x y
      | RPlus x, (RStar y | RPlus y) -> sub x y
      | RSeq (x :: y), (RPlus z | RStar z) -> sub x z && sub (RSeq y) (RStar z)
      | x, (RStar y | RPlus y) -> sub x y
      | _ -> false

    let rec absorb_epsilon = function
      | RPlus r :: l -> RStar r :: l
      | r :: _ as l when nullable r -> l
      | r :: l -> r :: absorb_epsilon l
      | [] -> [ epsilon ]

    let rec simplify_alt accu = function
      | [] -> List.rev accu
      | x :: rest ->
          if List.exists (sub x) accu || List.exists (sub x) rest then
            simplify_alt accu rest
          else simplify_alt (x :: accu) rest

    let alt s1 s2 =
      let s1 =
        match s1 with
        | RAlt x -> x
        | x -> [ x ]
      in
      let s2 =
        match s2 with
        | RAlt x -> x
        | x -> [ x ]
      in
      let l = merge s1 s2 in
      let l = if has_epsilon l then absorb_epsilon (remove_epsilon l) else l in
      let l = simplify_alt [] l in
      match l with
      | [ x ] -> x
      | [ RSeq [ a; RPlus r ]; a' ] when compare a a' = 0 -> RSeq [ a; RStar r ]
      | l -> RAlt l

    let rec simplify_seq = function
      | RStar x :: ((RStar y | RPlus y) :: _ as rest) when compare x y = 0 ->
          simplify_seq rest
      | RPlus x :: (RPlus y :: _ as rest) when compare x y = 0 ->
          simplify_seq (x :: rest)
      | RPlus x :: RStar y :: rest when compare x y = 0 ->
          simplify_seq (RPlus y :: rest)
      | x :: rest -> x :: simplify_seq rest
      | [] -> []

    let rec seq s1 s2 =
      match (s1, s2) with
      | RAlt [], _
      | _, RAlt [] ->
          epsilon
      | RSeq [], x
      | x, RSeq [] ->
          x
      | _ ->
          let s1 =
            match s1 with
            | RSeq x -> x
            | x -> [ x ]
          in
          let s2 =
            match s2 with
            | RSeq x -> x
            | x -> [ x ]
          in
          find_plus [] (s1 @ s2)

    and find_plus before = function
      | [] -> (
          match before with
          | [ h ] -> h
          | l -> RSeq (simplify_seq (List.rev l)))
      | RStar s :: after -> (
          let star =
            match s with
            | RSeq x -> x
            | x -> [ x ]
          in
          let right, star', after' = factor [] star after in
          let left, star'', before' = factor [] (List.rev star') before in
          match star'' with
          | [] ->
              let s = find_plus [] (left @ List.rev right) in
              find_plus (RPlus s :: before') after'
          | _ -> find_plus (RStar s :: before) after)
      | x :: after -> find_plus (x :: before) after

    let star = function
      | RAlt []
      | RSeq [] ->
          epsilon
      | RStar _ as s -> s
      | RPlus s -> RStar s
      | s -> RStar s
  end

  open B

  type slot = {
    mutable weight : int;
    mutable outg : (slot * re) list;
    mutable inc : (slot * re) list;
    mutable self : re;
    mutable ok : bool;
  }

  let alloc_slot () =
    { weight = 0; outg = []; inc = []; self = empty; ok = false }

  let decompile trans n0 =
    let slot_table = H.create 121 in
    let slots = ref [] in
    let slot n =
      try H.find slot_table n with
      | Not_found ->
          let s = alloc_slot () in
          H.add slot_table n s;
          slots := s :: !slots;
          s
    in

    let add_trans s1 s2 t =
      if s1 == s2 then s1.self <- alt s1.self t
      else (
        s1.outg <- (s2, t) :: s1.outg;
        s2.inc <- (s1, t) :: s2.inc)
    in

    let final = alloc_slot () in
    let initial = alloc_slot () in

    let rec conv n =
      let s = slot n in
      if not s.ok then (
        s.ok <- true;
        match trans n with
        | `T (tr, f) ->
            if f then add_trans s final epsilon;
            List.iter (fun (l, dst) -> add_trans s (conv dst) (rtrans l)) tr
        | `Eps (l, dst) -> add_trans s (conv dst) (alt (rtrans l) epsilon));
      s
    in

    let elim s =
      s.weight <- -1;
      let loop = star s.self in
      List.iter
        (fun (s1, t1) ->
          if s1.weight >= 0 then
            List.iter
              (fun (s2, t2) ->
                if s2.weight >= 0 then add_trans s1 s2 (seq t1 (seq loop t2)))
              s.outg)
        s.inc
    in

    add_trans initial (conv n0) epsilon;
    List.iter
      (fun s -> s.weight <- List.length s.inc * List.length s.outg)
      !slots;
    let slots =
      List.sort (fun s1 s2 -> std_compare s1.weight s2.weight) !slots
    in
    List.iter elim slots;
    let r =
      List.fold_left
        (fun accu (s, t) -> if s == final then alt accu t else accu)
        empty initial.outg
    in
    regexp r
end
