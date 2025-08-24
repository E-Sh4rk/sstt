open Sstt_utils
open Base

module Atom = Enum

type t = Pos of (Atom.t*int) list | Neg of (Atom.t * int) list

module HList = Hash.List(Atom)
let hash = function
    Neg l -> Hash.(mix const1 (HList.hash l))
  | Pos l -> Hash.(mix const2 (HList.hash l))
let any = Neg []
let empty = Pos []

let ($::) = HList.($::)
let mk e = Pos (e $:: [])
let construct (n,es) =
  let es = List.sort_uniq (fun a b -> Atom.compare b a) es in
  let es = List.fold_left (fun acc e -> e $:: acc) [] es in
  if n then Pos es else Neg es

let destruct t = match t with
  | Pos s -> true, List.map fst s
  | Neg s -> false, List.map fst s

let rec union_list l1 l2 =
  match l1, l2 with
    [], _ -> l2
  | _, [] -> l1
  | (a1,_) :: ll1, (a2,_) :: ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then a1 $:: union_list ll1 l2
    else if n = 0 then a1 $:: union_list ll1 ll2
    else a2 $:: union_list l1 ll2

let rec inter_list l1 l2 =
  match l1, l2 with
    [], _ | _, [] -> []
  | (a1,_) :: ll1, (a2,_)::ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then inter_list ll1 l2
    else if n = 0 then a1 $:: inter_list ll1 ll2
    else inter_list l1 ll2

let rec diff_list l1 l2 =
  match l1, l2 with
    [], _ -> []
  | _, [] -> l1
  | (a1,_) :: ll1, (a2,_) :: ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then a1 $:: diff_list ll1 l2
    else if n = 0 then diff_list ll1 ll2
    else diff_list l1 ll2

let cap t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (inter_list p1 p2)
  | Neg n1, Neg n2 -> Neg (union_list n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Pos (diff_list p n)
let cap = fcap ~empty ~any ~cap
let cup t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (union_list p1 p2)
  | Neg n1, Neg n2 -> Neg (inter_list n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Neg (diff_list n p)

let cup = fcup ~empty ~any ~cup
let neg = function
  | Pos s -> Neg s
  | Neg s -> Pos s
let neg t = fneg t ~empty ~any ~neg
let diff t1 t2 = cap t1 (neg t2)
let diff = fdiff ~empty ~any ~diff 
let is_any = function
  | Neg [] -> true
  | _  -> false

let is_empty = function
  | Pos [] -> true
  | _ -> false
let compare t1 t2 =
  match t1, t2 with
  | Pos _, Neg _ -> 1
  | Neg _, Pos _ -> -1
  | Pos s1, Pos s2 | Neg s1, Neg s2 -> HList.compare s1 s2
let equal t1 t2 = compare t1 t2 = 0

let direct_nodes _ = []
let map_nodes _ t = t
let simplify t = t