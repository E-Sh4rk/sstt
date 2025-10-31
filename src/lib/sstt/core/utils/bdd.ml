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
  val tname : string

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
  let equal b1 b2 = Bool.equal b1 b2
  let compare b1 b2 = Bool.compare b1 b2
  let hash b = if b then Hash.const1 else Hash.const0
  let tname = "Bool"
end

module type Atom = sig
  type t
  val simplify : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val tname : string
end

module Make(N:Atom)(L:Leaf) = struct
  type t =
    | Node of N.t * t * t * int
    | Leaf of L.t * int

  let hash = function
      Leaf (_, h) -> h
    | Node (_, _, _, h) -> h

  let rec equal n1 n2 =
    n1 == n2 ||
    match n1, n2 with
      Leaf (l1,h1), Leaf (l2,h2) -> h1 == h2 && L.equal l1 l2
    | Node (a1, p1, n1, h1),
      Node (a2, p2, n2, h2) ->
      h1 == h2 && N.equal a1 a2 && equal p1 p2 && equal n1 n2
    | _ -> false

  module Memo = Hash.Memo1(struct
      type nonrec t = t
      let hash = hash
      let equal = equal
    end)

  let tname = Printf.sprintf "Bdd(%s)(%s)" N.tname L.tname
  let memo = Memo.create (tname ^ ".memo")

  let memoize n =
    match Memo.find_opt memo n with
      Some n -> n
    | None -> Memo.add memo n n

  let hleaf l = Leaf (l, Hash.mix Hash.const2 (L.hash l)) |> memoize
  let hnode a p n = Node (a, p, n, 
                          Hash.mix3 (N.hash a) (hash p) (hash n) ) |> memoize
  let empty = hleaf L.empty
  let any = hleaf L.any


  let singleton a = hnode a any empty
  let nsingleton a = hnode a empty any
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

  let leaf l = hleaf l
  module K = struct type nonrec t = t
    let hash  = hash
    let equal = equal
  end

  module Memo2 = Hash.Memo2(K)(K)

  module Memo1 = Hash.Memo1(K)
  let memo_neg = Memo1.create (tname ^ ".memo_neg")
  let rec neg_rec t =
    match Memo1.find_opt memo_neg t with
      Some r -> r
    | None -> let res =
                match t with
                | Leaf (l, _) -> leaf (L.neg l)
                | Node (a, p, n, _) ->
                  node a (neg p) (neg n)
      in Memo1.add memo_neg t res
  and neg t = fneg ~empty ~any ~neg:neg_rec t

  let memo_cap = Memo2.create (tname ^ ".memo_cap")
  let memo_cup = Memo2.create (tname ^ ".memo_cup")
  let memo_diff = Memo2.create (tname ^ ".memo_diff")

  let rec op lop nop t1 t2 =
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
  and ocap t1 t2 = 
    let key = t1, t2 in
    match Memo2.find_opt memo_cap key with
      Some r -> r
    | None -> let res = op L.cap cap t1 t2 in
      Memo2.add memo_cap key res
  and cap t1 t2 = fcap ~empty ~any ~cap:ocap t1 t2
  and ocup t1 t2 = 
    let key = t1, t2 in
    match Memo2.find_opt memo_cup key with
      Some r -> r
    | None -> let res = op L.cup cup t1 t2 in
      Memo2.add memo_cup key res

  and cup t1 t2 = fcup ~empty ~any ~cup:ocup t1 t2
  and odiff t1 t2 = 
    let key = t1, t2 in
    match Memo2.find_opt memo_diff key with
      Some r -> r
    | None -> let res = op L.diff diff t1 t2 in
      Memo2.add memo_diff key res
  and diff t1 t2 = fdiff_neg ~empty ~any ~neg ~diff:odiff t1 t2

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
    let pc = compare_to_atom a p < 0 in
    let nc = compare_to_atom a n < 0 in
    if pc && nc then node a p n
    else if pc then cup (node a p empty) (cap (nsingleton a) n)
    else if nc then cup (cap (singleton a) p) (node a empty n)
    else cup (cap (singleton a) p) (cap (nsingleton a) n)

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
  let memo_dnf = Memo1.create (tname ^ ".memo_dnf")
  let dnf t = match Memo1.find_opt memo_dnf t with
      Some r -> r
    | None -> let res = dnf t in Memo1.add memo_dnf t res

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
    let rec aux ps ns t =
      match t with
        Leaf (l, _) -> f (ps, ns, l)
      | Node (a, p, n, _) ->
        aux (a :: ps) ns p &&
        aux ps (a :: ns) n
    in
    aux [] [] t

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

  let memo_atoms = Memo1.create (tname ^ ".memo_atoms")
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

  let atoms t = 
    match Memo1.find_opt memo_atoms t with
      Some r -> r
    | None -> let res = atoms t in Memo1.add memo_atoms t res

  let memo_leaves = Memo1.create (tname ^ ".memo_leaves")

  let leaves t =
    let rec aux acc t =
      match t with
      | Leaf (l, _) -> l::acc
      | Node (_,p,n, _) ->
        let acc = aux acc p in
        let acc = aux acc n in
        acc
    in aux [] t

  let leaves t =
    match Memo1.find_opt memo_leaves t with
      Some r -> r
    | None -> let res = leaves t in
      Memo1.add memo_leaves t res

  type ctx =
      Pos of N.t * ctx
    | Neg of N.t * ctx
    | Root

  let rec to_t ctx t =
    match ctx with
    | Root -> t
    | Pos (a, ctx) -> to_t ctx (hnode a t empty)
    | Neg (a, ctx) -> to_t ctx (hnode a empty t)

  let simplify eq t =
    let rec aux ctx t =
      if t == empty || t == any then t else
        match t with
        | Leaf (l,_) -> let l' = L.simplify l in if l == l' then t else leaf l'
        | Node (a, p, n, _) ->
          let p' = aux (Pos (a, ctx)) p
          and n' = aux (Neg (a, ctx)) n in
          if equal p' n' then p' else
            let t = if p' == p && n' == n then t
              else hnode a p' n' in
            let ctx_t = to_t ctx t in
            if eq ctx_t (to_t ctx p') then p'
            else if eq ctx_t (to_t ctx n') then n'
            else t
    in
    aux Root (map_nodes N.simplify t)

  (** Assumes simplify is always called with the same eq for a given
      instance of a bdd *)
  let memo_simplify = Memo1.create (tname ^ ".memo_simplify")
  let simplify eq t =
    match Memo1.find_opt memo_simplify t with
      Some r -> r
    | None -> let res = simplify eq t in
      Memo1.add memo_simplify t res
end
