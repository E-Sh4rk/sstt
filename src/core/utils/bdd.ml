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
  let equal b1 b2 = (b1 = b2)
  let compare b1 b2 = compare b1 b2
end

module type Atom = sig
  type t
  val simplify : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Make(N:Atom)(L:Leaf) = struct
  type t =
  | Node of N.t * t * t
  | Leaf of L.t

  let empty = Leaf (L.empty)
  let any = Leaf (L.any)

  let singleton a = Node (a, any, empty)
  let nsingleton a = Node (a, empty, any)
  let mk_leaf l = Leaf l

  let rec equal t1 t2 =
    match t1, t2 with
    | Leaf l1, Leaf l2 -> L.equal l1 l2
    | Node _, Leaf _ | Leaf _, Node _ -> false
    | Node (a1, p1, n1), Node (a2, p2, n2) ->
      N.equal a1 a2 && equal p1 p2 && equal n1 n2

  let rec compare t1 t2 =
    match t1, t2 with
    | Leaf l1, Leaf l2 -> L.compare l1 l2
    | Leaf _, Node _ -> -1
    | Node _, Leaf _ -> 1
    | Node (a1, p1, n1), Node (a2, p2, n2) ->
      N.compare a1 a2 |> ccmp
      compare p1 p2 |> ccmp
      compare n1 n2

  let rec neg t =
    match t with
    | Leaf l -> Leaf (L.neg l)
    | Node (a, p, n) ->
      Node (a, neg p, neg n)

  let normalize t =
    match t with
    | Leaf l -> Leaf l
    | Node (_, p, n) when equal p n -> p
    | Node (a, p, n) -> Node (a, p, n)

  let op lop t1 t2 =
    let rec op t1 t2 =
      let res =
        match t1, t2 with
        | Leaf l1, Leaf l2 -> Leaf (lop l1 l2)
        | Leaf l, Node (a,p,n) ->
          Node (a, op (Leaf l) p, op (Leaf l) n)
        | Node (a,p,n), Leaf l ->
          Node (a, op p (Leaf l), op n (Leaf l))
        | Node (a1,p1,n1), Node (a2,_,_) when N.compare a1 a2 < 0 ->
          Node (a1, op p1 t2, op n1 t2)
        | Node (a1,_,_), Node (a2,p2,n2) when N.compare a1 a2 > 0 ->
          Node (a2, op t1 p2, op t1 n2)
        | Node (a,p1,n1), Node (_,p2,n2) ->
          Node (a, op p1 p2, op n1 n2)
      in
      normalize res
    in
    op t1 t2

  let cap = op L.cap
  let cup = op L.cup
  let diff = op L.diff

  let rec substitute f t =
    match t with
    | Leaf l -> Leaf l
    | Node (a,p,n) ->
      let p,n = substitute f p, substitute f n in
      let t = f a in
      let p,n =  cap p t, cap n (neg t) in
      cup p n

  let map_nodes f t =
    substitute (fun n -> f n |> singleton) t

  let rec map_leaves f t =
    match t with
    | Leaf l -> Leaf (f l)
    | Node (a,p,n) -> Node (a,map_leaves f p, map_leaves f n)

  let dnf t =
    let rec aux acc ps ns t =
      match t with
      | Leaf l -> (ps,ns,l)::acc
      | Node (a,p,n) ->
        let acc = aux acc (a::ps) ns p in
        let acc = aux acc ps (a::ns) n in
        acc
    in
    aux [] [] [] t

  let conj = List.fold_left cap any
  let disj = List.fold_left cup empty
  let of_dnf dnf =
    let line (ps,ns,l) =
      let ps = ps |> List.map singleton in
      let ns = ns |> List.map nsingleton in
      let l = mk_leaf l in
      l::(ps@ns) |> conj
    in
    dnf |> List.map line |> disj

  let atoms t =
    let rec aux acc t =
      match t with
      | Leaf _ -> acc
      | Node (a,p,n) ->
        let acc = a::acc in
        let acc = aux acc p in
        let acc = aux acc n in
        acc
      in aux [] t

  let leaves t =
    let rec aux acc t =
      match t with
      | Leaf l -> l::acc
      | Node (_,p,n) ->
        let acc = aux acc p in
        let acc = aux acc n in
        acc
    in aux [] t

  type ctx =
  | CNode of N.t * ctx * ctx
  | CT of t
  | CHole
  let fill ctx t =
    let rec aux ctx =
      match ctx with
      | CNode (a, p, n) -> CNode (a, aux p, aux n)
      | CT t -> CT t
      | CHole -> t
    in
    aux ctx
  let rec to_t ctx =
    match ctx with
    | CNode (a,p,n) -> Node (a, to_t p, to_t n)
    | CT t -> t
    | CHole -> assert false
  let test_equiv eq ctx t nt =
    let t = fill ctx (CT t) |> to_t in
    let nt = fill ctx (CT nt) |> to_t in
    eq t nt
  let simplify eq t =
    let rec aux ctx t =
      match t with
      | Leaf l -> Leaf (L.simplify l)
      | Node (a, p, n) ->
        let a = N.simplify a in
        let p = aux (fill ctx (CNode (a, CHole, CT n))) p in
        let t = Node (a, p, n) in
        if test_equiv eq ctx t p then p
        else
          let n = aux (fill ctx (CNode (a, CT p, CHole))) n in
          let t = Node (a, p, n) in
          if test_equiv eq ctx t n then n else t
    in
    aux CHole t
end
