module Atom = Id.NamedIdentifier( )

type t = Pos of Atom.t list | Neg of Atom.t list

let any = Neg []
let empty = Pos []

let mk e = Pos [e]
let construct (n,es) =
  let es = List.sort_uniq Atom.compare es in
  if n then Pos es else Neg es

let destruct t = match t with
  | Pos s -> true, s
  | Neg s -> false, s

let rec union_list l1 l2 =
  match l1, l2 with
    [], _ -> l2
  | _, [] -> l1
  | a1 :: ll1, a2 :: ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then a1 :: union_list ll1 l2
    else if n = 0 then a1 :: union_list ll1 ll2
    else a2 :: union_list l1 ll2

let rec inter_list l1 l2 =
  match l1, l2 with
    [], _ | _, [] -> []
  | a1 :: ll1, a2::ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then inter_list ll1 l2
    else if n = 0 then a1 :: inter_list ll1 ll2
    else inter_list l1 ll2

let rec diff_list l1 l2 =
  match l1, l2 with
    [], _ -> []
  | _, [] -> l1
  | a1 :: ll1, a2 :: ll2 ->
    let n = Atom.compare a1 a2 in
    if n < 0 then a1 :: diff_list ll1 l2
    else if n = 0 then diff_list ll1 ll2
    else diff_list l1 ll2

let cap t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (inter_list p1 p2)
  | Neg n1, Neg n2 -> Neg (union_list n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Pos (diff_list p n)
let cup t1 t2 =
  match t1, t2 with
  | Pos p1, Pos p2 -> Pos (union_list p1 p2)
  | Neg n1, Neg n2 -> Neg (inter_list n1 n2)
  | Pos p, Neg n | Neg n, Pos p -> Neg (diff_list n p)
let neg = function
  | Pos s -> Neg s
  | Neg s -> Pos s
let diff t1 t2 = cap t1 (neg t2)

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
  | Pos s1, Pos s2 | Neg s1, Neg s2 -> List.compare Atom.compare s1 s2
let equal t1 t2 = compare t1 t2 == 0

let direct_nodes _ = []
let map_nodes _ t = t
let simplify t = t