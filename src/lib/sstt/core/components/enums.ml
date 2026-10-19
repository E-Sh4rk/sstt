open Sstt_utils
open Base

module Atom = Enum

module HSet = Hash.SetList(Atom)

type t = Pos of HSet.t | Neg of HSet.t

let hash = function
    Neg l -> Hash.(mix const1 (HSet.hash l))
  | Pos l -> Hash.(mix const2 (HSet.hash l))
let any = Neg HSet.empty
let empty = Pos HSet.empty

let ($::) = HSet.add
let mk e = Pos (e $:: HSet.empty)
let construct (n,es) =
  let es = List.sort_uniq (fun a b -> Atom.compare b a) es in
  let es = List.fold_left (fun acc e -> e $:: acc) HSet.empty es in
  if n then Pos es else Neg es

let destruct t = match t with
  | Pos s -> true, HSet.to_list s
  | Neg s -> false, HSet.to_list s
let cap t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (HSet.inter p1 p2)
  | Neg n1, Neg n2 -> Neg (HSet.union n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Pos (HSet.diff p n)
let cap = fcap ~empty ~any ~cap
let cup t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (HSet.union p1 p2)
  | Neg n1, Neg n2 -> Neg (HSet.inter n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Neg (HSet.diff n p)

let cup = fcup ~empty ~any ~cup
let neg = function
  | Pos s -> Neg s
  | Neg s -> Pos s
let neg t = fneg t ~empty ~any ~neg
let diff t1 t2 = cap t1 (neg t2)
let diff = fdiff ~empty ~any ~diff
let is_any = function
  | Neg l -> HSet.is_empty l
  | _  -> false

let is_empty = function
  | Pos l -> HSet.is_empty l
  | _ -> false
let compare t1 t2 =
  match t1, t2 with
  | Pos _, Neg _ -> 1
  | Neg _, Pos _ -> -1
  | Pos s1, Pos s2 | Neg s1, Neg s2 -> HSet.compare s1 s2
let equal t1 t2 = compare t1 t2 = 0

let direct_nodes _ = []
let map_nodes _ t = t
let simplify t = t