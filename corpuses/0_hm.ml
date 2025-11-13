val (+) : (int, int) -> int
val (-) : (int, int) -> int
val ( * ) : (int, int) -> int
val (/) : (int, int) -> int
val (%) : (int, int) -> int
val compare : 'a -> 'a -> int
val (<) : ('a, 'a) -> bool
val (>) : ('a, 'a) -> bool
val (=) : ('a, 'a) -> bool
val (<=) : ('a, 'a) -> bool
val (>=) : ('a, 'a) -> bool
val failwith : string -> empty
val invalid_arg : string -> empty

#infer_overload = false

(* ===== Operations on lists ===== *)

let length_aux n l =
  match l with
  | [] -> n
  | _::l -> length_aux (n+1) l
  end

let length l = length_aux 0 l

let hd' l =
  match l with
  | [] -> failwith "hd"
  | x::_ -> x
  end
let hd_safe l =
  match l with
  | x::_ -> x
  end

let tl' l =
  match l with
  | [] -> failwith "tl"
  | _::ll -> ll
  end
let tl_safe l =
  match l with
  | _::ll -> ll
  end

let nth_aux l n =
  match l with
  | [] -> failwith "nth"
  | x::ll -> if n = 0 then x else nth_aux ll (n-1)
  end

let nth l n =
  if n < 0 then failwith "nth"
  else nth_aux l n

let append l1 l2 =
  match l1 with
  | [] -> l2
  | x::l -> x::(append l l2)
  end
let (@) (l1, l2) = append l1 l2

let rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x::l -> rev_append l (x::l2)
  end

let rev l =
  if l is [] then [] else (rev (tl l))@[hd l]

let flatten l =
  match l with
  | [] -> []
  | x::r -> x @ (flatten r)
  end

let concat l = flatten l

let map f l =
  match l with
  | [] -> []
  | x::ll -> (f x)::(map f ll)
  end

let mapi_aux i f l =
  match l with
  | [] -> []
  | x::ll -> let r = f i x in r::(mapi_aux (i+1) f ll)
  end

let mapi f x = mapi_aux 0 f x

let fold_left f l acc =
  match l with
  | [] -> acc
  | x::ll -> fold_left f ll (f acc x)
  end

let fold_right f acc l =
  match l with
  | [] -> acc
  | x::ll -> f x (fold_right f acc ll)
  end

(* ===== Operations on trees ===== *)

type t('a) =
  Nil | Node(t('a), Key, 'a, t('a), int)

let height (x: t('a)) =
  match x with
  | Nil -> 0
  | Node(_,_,_,_,h) -> h
  end

let create l x d r =
  let hl = height l in
  let hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

val bal : t('a) -> Key -> 'a -> t('a) -> t('a)
let bal l x d r =
(* let bal (l:t('a)) (x: Key) (d:'a) (r:t('a)) : t('a) = *)
  let hl = match l with Nil -> 0 | Node(_,_,_,_,h) -> h end in
  let hr = match r with Nil -> 0 | Node(_,_,_,_,h) -> h end in
  if hl > (hr + 2) then
    match l with
    | Nil -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if (height ll) >= (height lr) then
        create ll lv ld (create lr x d r)
      else
        match lr with
        | Nil -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end
  else if hr > (hl + 2) then
    match r with
    | Nil -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if (height rr) >= (height rl) then
        create (create l x d rl) rv rd rr
      else
        match rl with
        | Nil -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end
  else Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

(* ===== Fixpoint combinator ===== *)

(* The type deduced for fixpoint can be read as follows
   forall('c <: 'a -> 'b)('d <:'c). ('c -> 'd) -> 'd 
*)
let fixpoint = fun f ->
  let delta = fun x ->
     f ( fun  v -> ( x x v ))
   in delta delta

let fact_stub fact n =
  if n is 0 then 1 else (fact (n-1))*n

let fact' = fixpoint fact_stub

let length_stub length lst =
  if lst is [] then 0 else (length (tl lst))+1

let length' = fixpoint length_stub

let map_stub map f lst =
  if lst is [] then []
  else (f (hd lst))::(map f (tl lst))

let map' x = fixpoint map_stub x
