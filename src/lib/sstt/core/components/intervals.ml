open Sstt_utils

module Number =
struct
  type t =
    | NegInf
    | PosInf
    | Num of Z.t
  let equal a b =
    a == b
    ||
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> true
    | Num x, Num y -> Z.equal x y
    | _ -> false

  let compare a b =
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _ | _, PosInf -> -1
    | PosInf, _ | _, NegInf -> 1
    | Num x, Num y -> Z.compare x y

  let succ = function
      Num z -> Num (Z.succ z)
    | _ -> failwith "Number.succ"
  let pred = function
      Num z -> Num (Z.pred z)
    | _ -> failwith "Number.pred"


  let ( < ) x1 x2 = compare x1 x2 < 0
  let ( = ) x1 x2 = equal x1 x2
  let ( > ) x1 x2 = compare x1 x2 > 0
  let ( <= ) x1 x2 = compare x1 x2 <= 0
  let ( >= ) x1 x2 = compare x1 x2 >= 0
  let min x1 x2 = if x2 < x1 then x2 else x1
  let max x1 x2 = if x2 > x1 then x2 else x1

  let pp fmt =
    let open Format in
    function Num z -> fprintf fmt "%a" Z.pp_print z
           | _ -> () (* don't print anything, see Interval.pp below *)

  let hash = function
      NegInf -> Hash.const1
    | PosInf -> Hash.const2
    | Num z -> Z.hash z
end

module Interval = struct
  type t = Number.t * Number.t
  let tname = "Interval"
  let mk_bounded lb ub =
    if Z.leq lb ub then Number.(Num lb, Num ub)
    else invalid_arg "Lower bound is greater than upper bound"
  let mk lb ub =
    let open Number in
    match lb, ub with
    | None, None -> NegInf, PosInf
    | Some lb, None -> Num lb, PosInf
    | None, Some ub -> NegInf, Num ub
    | Some lb, Some ub -> mk_bounded lb ub
  let mk_singl i = mk_bounded i i
  let any = Number.(NegInf, PosInf)
  let to_option =
    function
      Number.Num z -> Some z
    | _ -> None
  let get (lb, ub) = (to_option lb, to_option ub)

  let compare (lb1, ub1) (lb2,ub2) =
    Number.compare lb1 lb2 |> ccmp
      Number.compare ub1 ub2

  let equal t1 t2 = compare t1 t2 = 0

  let pp fmt (o1,o2) =
    Format.fprintf fmt "(%a..%a)" Number.pp o1 Number.pp o2

  let hash (b1, b2) = Hash.mix (Number.hash b1) (Number.hash b2)
end

module Atom = Interval
type node

include Hash.List(Interval)

let hash = function
    [] -> Hash.const2
  | (_, h) :: _ -> h

let empty = []
let any = Interval.any $:: []
let mk i = i $:: []

let destruct t = List.map fst t 

(* In all the function below, comparisons are
   those of the Number module. *)
module MemoCons = Hash.Memo1(
  struct
    type nonrec t = (Number.t * Number.t) * t
    let equal ((a1,b1), l1) ((a2, b2), l2)  =
      (l1 == l2 && Number.equal a1 a2 && Number.equal b1 b2)
    let hash ((a,b), l) = Hash.mix3 (Number.hash a) (Number.hash b) (hash l)
  end
  )

let memo_cons = MemoCons.create "Intervals.memo_cons"
let rec ( @:: ) ((a, b) as n) l =
  let key = n,l in
  match MemoCons.find_opt memo_cons key with
    Some ll -> ll
  | None -> let res =
              let open Number in
              match l with
              | [] -> (a,b) $:: []
              | ((c, d),_) :: ll ->
                (* invariant, b < c *)
                if b = PosInf then (a,b) $:: []
                else if succ b = c then (a, d) @:: ll
                else  (a,b) $:: l
    in
    MemoCons.add memo_cons key res
module K = struct
   type nonrec t = t
   let hash = hash
   let equal = equal
end
module Memo2 = Hash.Memo2(K)(K)

let memo_cup = Memo2.create "Intervals.cup"
let rec cup_rec i1 i2 =
  let key = (i1, i2) in
  match Memo2.find_opt memo_cup key with
    Some r -> r
  | None -> let res = 
              let open Number in
              match i1, i2 with
              | [], l | l, [] -> l
              | ((a1, b1) as c1,_) :: ii1, ((a2, b2) as c2,_) :: ii2 ->
                if a1 > b2 then c2 @:: cup i1 ii2
                else if a2 > b1 then c1 @:: cup ii1 i2
                else
                  let u = min a1 a2 in
                  if b1 < b2 then cup ii1 ((u, b2) @:: ii2)
                  else if b1 = b2 then (u, b1) @:: cup ii1 ii2
                  else cup ((u, b1) @:: ii1) ii2
    in Memo2.add memo_cup key res
and cup t1 t2 = fcup ~empty ~any ~cup:cup_rec t1 t2 

let memo_cap = Memo2.create "Intervals.cap"
let rec cap_rec i1 i2 =
  let key = (i1, i2) in
  match Memo2.find_opt memo_cap key with
    Some r -> r
  | None -> 
    let res =
      let open Number in
      match i1, i2 with
      | [], _ | _, [] -> []
      | ((a1, b1),_) :: ii1, ((a2, b2),_) :: ii2 ->
        if a1 > b2 then cap i1 ii2
        else if a2 > b1 then cap ii1 i2
        else
          let u = max a1 a2 in
          if b1 < b2 then (u, b1) @:: cap ii1 ((succ b1, b2) @:: ii2)
          else if b1 = b2 then (u, b1) @:: cap ii1 ii2
          else (u, b2) @:: cap ((succ b2, b1) @:: ii1) ii2
    in Memo2.add memo_cap key res
and cap t1 t2 = fcap ~empty ~any ~cap:cap_rec t1 t2

let memo_diff = Memo2.create "Intervals.diff"
let rec diff_rec i1 i2 =
  let key = (i1, i2) in
  match Memo2.find_opt memo_diff key with
    Some r -> r
  | None -> let res =
              let open Number in
              match i1, i2 with
              | ([] as l), _ | l, [] -> l
              | ((a1, b1) as c1,_) :: ii1, ((a2, b2),_) :: ii2 ->
                if b1 < a2 then c1 @:: diff ii1 i2
                else if b2 < a1 then diff i1 ii2
                else if a2 <= a1 then
                  if b2 < b1 then diff ((succ b2, b1) @:: ii1) ii2 else diff ii1 i2
                else if (* a1 < a2 *)
                  b2 >= b1 then diff ((a1, pred a2) @:: ii1) i2
                else diff ((a1, pred a2) @:: (succ b2, b1) @:: ii1) ii2
    in Memo2.add memo_diff key res
and diff t1 t2 = fdiff ~empty ~any ~diff:diff_rec t1 t2 

let neg i = diff any i
let neg = fneg ~empty ~any ~neg

let of_list l =
  (* To avoid a quadratic behaviour if building from
     the smallest interval to the largest *)
  l |> List.sort (fun x y -> - Interval.compare x y)
  |> List.fold_left (fun acc a -> a@::acc) empty

let construct = of_list

let is_empty = function [] -> true | _ -> false

let direct_nodes _ = []
let map_nodes _ t = t
let simplify t = t

let destruct_neg t = neg t |> destruct
