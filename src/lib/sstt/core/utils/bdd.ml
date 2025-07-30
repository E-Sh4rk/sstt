open Sstt_utils

module type Leaf = sig
  type t
  val any : t
  val empty : t
  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val simplify : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module BoolLeaf : Leaf with type t = bool = struct
  type t = bool
  let any = true
  let empty = false
  let cap = (&&)
  let cup = (||)
  let diff b1 b2 = b1 && not b2
  let neg = not
  let simplify b = b
  let equal b1 b2 = (b1 == b2)
  let compare b1 b2 = Bool.compare b1 b2
  let hash b = if b then Hash.const1 else Hash.const0
end

module type Atom = sig
  type t
  val simplify : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Make(N:Atom)(L:Leaf) = struct
  type t =
    | Node of N.t * t * t * int
    | Leaf of L.t * int

  let hash = function
      Leaf (_, h) -> h
    | Node (_, _, _, h) -> h

  let hleaf l = Leaf (l, Hash.mix Hash.const2 (L.hash l))
  let hnode a p n = Node (a, p, n, 
                          Hash.mix3 (N.hash a) (hash p) (hash n) )
  let empty = hleaf L.empty
  let any = hleaf L.any


  let singleton a = hnode a any empty
  let nsingleton a = hnode a empty any
  let rec equal t1 t2 =
    t1 == t2 ||
    match t1, t2 with
    | Leaf (l1, h1) , Leaf (l2, h2) -> h1 == h2 && L.equal l1 l2
    | Node _, Leaf _ | Leaf _, Node _ -> false
    | Node (a1, p1, n1, h1), Node (a2, p2, n2, h2) ->
      h1 == h2 &&
      N.equal a1 a2 && equal p1 p2 && equal n1 n2

  let rec compare t1 t2 =
    if t1 == t2 then 0 else
      match t1, t2 with
      | Leaf (l1, h1), Leaf (l2, h2)  -> Int.compare h1 h2 |> ccmp L.compare l1 l2
      | Leaf _, Node _ -> 1
      | Node _, Leaf _ -> -1
      | Node (a1, p1, n1, h1), Node (a2, p2, n2, h2) ->
        let c = Int.compare h1 h2 in if c <> 0 then c else
          let c = N.compare a1 a2 in if c <> 0 then c else
            let c = compare p1 p2 in if c <> 0 then c else
              compare n1 n2

  (* Smart constructor *)
  let node a p n =
    if equal p n then p
    else hnode a p n

  let leaf l =
    let t = hleaf l in
    if equal t empty then empty
    else if equal t any then any
    else t

  let rec neg t =
    if t == empty then any
    else if t == any then empty
    else
      match t with
      | Leaf (l, _) -> leaf (L.neg l)
      | Node (a, p, n, _) ->
        node a (neg p) (neg n)

  let rec op t1 t2 lop nop =
    match t1, t2 with
    | Leaf (l1, _), Leaf (l2,_) -> leaf (lop l1 l2)
    | Leaf _, Node (a,p,n, _) ->
      node a (nop t1 p) (nop t1 n)
    | Node (a,p,n, _), Leaf _ ->
      node a (nop p t2) (nop n t2)
    | Node (a1,p1,n1, _), Node (a2,p2,n2, _) ->
      let n = N.compare a1 a2 in
      if n < 0 then node a1 (nop p1 t2) (nop n1 t2)
      else if n > 0 then node a2 (nop t1 p2) (nop t1 n2)
      else
        node a1 (nop p1 p2) (nop n1 n2)
  and cap t1 t2 =
    if t1 == empty || t2 == empty then empty
    else if t2 == any || t1 == t2 then t1
    else if t1 == any then t2
    else op t1 t2 L.cap cap
  and cup t1 t2 =
    if t1 == any || t2 == any then any
    else if t2 == empty || t1 == t2 then t1
    else if t1 == empty then t2
    else op t1 t2 L.cup cup
  and diff t1 t2 =
    if t1 == empty || t2 == any || t1 == t2 then empty
    else if t2 == empty then t1
    else if t1 == any then neg t2
    else op t1 t2 L.diff diff

  let compare_to_atom a t =
    match t with
      Leaf _ -> -1
    | Node (b, _, _, _) -> N.compare a b

  let rec substitute f t =
    match t with
    | Leaf _ -> t
    | Node (a,p,n, _) ->
      let p,n = substitute f p, substitute f n in
      let t = f a in
      let p,n = cap p t, diff n t in
      cup p n

  let node' a p n =
    let pc = compare_to_atom a p in
    let nc = compare_to_atom a n in
    if pc < 0 && nc < 0 then node a p n
    else if pc < 0 then cup (node a p empty) (cap (nsingleton a) n)
    else if nc < 0 then cup (cap (singleton a) p) (node a empty n)
    else cup (cap (singleton a) p)
        (cap (nsingleton a) n)

  let rec map_nodes f t =
    match t with
      Leaf _ -> t
    | Node (a, p, n, _) ->
      let p' = map_nodes f p in
      let n' = map_nodes f n in
      let a' = f a in
      if a == a' && p == p' && n == n' then t else
        node' a' p' n'

  let rec map_leaves f t =
    match t with
    | Leaf (l, _) -> let l' = f l in if l == l' then t else leaf l'
    | Node (a,p,n, _) ->
      let p' = map_leaves f p in
      let n' = map_leaves f n in
      if p == p' && n == n' then t
      else node a p' n'

  let dnf t =
    let rec aux acc ps ns t =
      match t with
      | Leaf (l, _) -> (ps,ns,l)::acc
      | Node (a,p,n, _) ->
        let acc = aux acc (a::ps) ns p in
        let acc = aux acc ps (a::ns) n in
        acc
    in
    aux [] [] [] t

  let fold_lines f acc t =
    let rec aux acc ps ns t =
      match t with
        Leaf (l, _) -> f acc (ps, ns, l)
      | Node (a, p, n, _) ->
        let acc = aux acc (a :: ps) ns p in
        aux acc ps (a :: ns) n
    in
    aux acc [] [] t

  let for_all_lines f t =
    try
      fold_lines (fun () l -> if not (f l) then raise_notrace Exit) () t;
      true
    with Exit -> false

  let big_op op default = function
    | [ ] -> default
    | [ t ] -> t
    | l -> List.fold_left op default l

  let conj = big_op cap any
  let disj = big_op cup empty

  let conj_map f l acc =
    List.fold_left (fun acc e -> cap (f e) acc) acc l

  let of_dnf dnf =
    let line (ps,ns,l) =
      let l = leaf l in
      cap l (conj_map singleton ps (conj_map nsingleton ns any))
    in
    dnf |> List.map line |> disj

  let atoms t =
    let rec aux acc t =
      match t with
      | Leaf _ -> acc
      | Node (a,p,n, _) ->
        let acc = a::acc in
        let acc = aux acc p in
        let acc = aux acc n in
        acc
    in aux [] t

  let leaves t =
    let rec aux acc t =
      match t with
      | Leaf (l, _) -> l::acc
      | Node (_,p,n, _) ->
        let acc = aux acc p in
        let acc = aux acc n in
        acc
    in aux [] t

  let rec to_t ctx t =
    match ctx with
    | [] -> t
    | (false, a)::ctx -> to_t ctx (hnode a t empty)
    | (true, a)::ctx -> to_t ctx (hnode a empty t)

  module Memo = Hashtbl.Make(struct type nonrec t = t * t 
      let equal (t1, t2) (s1, s2) = equal t1 s1 && equal s2 t2
      let hash (t1, t2) = Hash.mix (hash t1) (hash t2) 
    end)

  let simplify eq t =
    let rec aux ctx t =
      if t == empty || t == any then t else
        match t with
        | Leaf (l,_) -> let l' = L.simplify l in if l == l' then t else leaf l'
        | Node (a, p, n, _) ->
          let p' = aux ((false, a)::ctx) p
          and n' = aux ((true, a)::ctx) n in
          if equal p' n' then p' else
            let t = if p' == p && n' == n then t
              else hnode a p' n' in
            let ctx_t = to_t ctx t in
            if eq ctx_t (to_t ctx p') then p'
            else if eq ctx_t (to_t ctx n') then n'
            else t
    in
    aux [] (map_nodes N.simplify t)
end
