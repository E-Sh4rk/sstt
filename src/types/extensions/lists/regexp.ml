(**
   Taken from cs.stackexchange.com
   https://cs.stackexchange.com/a/2392
**)

module type Letter = sig
  type t
  val equal : t -> t -> bool
end

type 'l t =
| Empty | Epsilon | Letter of 'l
| Union of 'l t * 'l t
| Concat of 'l t * 'l t
| Star of 'l t

type 'l t_ext =
| EEpsilon | ELetter of 'l
| EUnion of 'l t_ext list
| EConcat of 'l t_ext list
| EStar of 'l t_ext
| EOption of 'l t_ext
| EPlus of 'l t_ext

module Make(L:Letter) = struct

  module R = struct
    type nonrec t = L.t t
    let rec equal t t' =
      match t, t' with
      | Empty, Empty -> true
      | Epsilon, Epsilon -> true
      | Letter l, Letter l' -> L.equal l l'
      | Union (r1,r2), Union (r1', r2')
      | Concat (r1,r2), Concat (r1', r2') -> equal r1 r1' && equal r2 r2'
      | Star r, Star r' -> equal r r'
      | _, _ -> false
  end
  module RExt = struct
    type nonrec t = L.t t_ext
    let rec equal t t' =
      match t, t' with
      | EEpsilon, EEpsilon -> true
      | ELetter l, ELetter l' -> L.equal l l'
      | EUnion lst, EUnion lst'
      | EConcat lst, EConcat lst' ->
        List.length lst = List.length lst' &&
        List.for_all2 equal lst lst'
      | EStar r, EStar r'
      | EOption r, EOption r'
      | EPlus r, EPlus r' -> equal r r'
      | _, _ -> false
  end
  type t = R.t
  type t_ext = RExt.t

  let brzozowski a b =
    let m = Array.length a in
    for n = m-1 downto 0 do
      b.(n) <- Concat (Star a.(n).(n), b.(n));
      for j = 0 to n-1 do
        a.(n).(j) <- Concat (Star a.(n).(n), a.(n).(j));
      done;
      for i = 0 to n-1 do
        b.(i) <- Union (b.(i), Concat (a.(i).(n), b.(n)));
        for j = 0 to n-1 do
          a.(i).(j) <- Union (a.(i).(j), Concat (a.(i).(n), a.(n).(j)));
        done
      done;
      for i = 0 to n-1 do
        a.(i).(n) <- Empty;
      done;
    done;
    b.(0)

  let simple_re =
    let rec simple = function
      | Union (e, f) when R.equal e f -> e
      | Union (Empty, e) | Union (e, Empty) -> e
      | Union (Union (e, f), g) -> simple (Union (e, Union (f, g)))
      | Concat (Epsilon, e) | Concat (e, Epsilon) -> simple e
      | Concat (Empty, _) | Concat (_, Empty) -> Empty
      | Concat (Concat (e, f), g) -> simple (Concat (e, Concat (f, g)))
      | Star Empty -> Epsilon
      | Star Epsilon -> Epsilon
      | Star e -> Star (simple e)
      | Union (e, f) -> Union (simple e, simple f)
      | Concat (e, f) -> Concat (simple e, simple f)
      | (Empty | Epsilon | Letter _) as e -> e
    in
    let rec f e =
      let e' = simple e in
      if R.equal e e' then  e' else f e'
    in f

  let concat r1 r2 =
    match r1, r2 with
    | EStar s, r when RExt.equal s r -> EPlus r
    | r, EStar s when RExt.equal s r -> EPlus r
    | EConcat l1, EConcat l2 -> EConcat (l1@l2)
    | EConcat l1, r2 -> EConcat (l1@[r2])
    | r1, EConcat l2 -> EConcat ([r1]@l2)
    | r1, r2 -> EConcat [r1;r2]
  let union r1 r2 =
    match r1, r2 with
    | EEpsilon, r | r, EEpsilon -> EOption r
    | EUnion l1, EUnion l2 -> EUnion (l1@l2)
    | EUnion l1, r2 -> EUnion (l1@[r2])
    | r1, EUnion l2 -> EUnion ([r1]@l2)
    | r1, r2 -> EUnion [r1;r2]

  let rec to_ext t =
    match t with
    | Epsilon -> EEpsilon
    | Letter l -> ELetter l
    | Concat (r1, r2) -> concat (to_ext r1) (to_ext r2)
    | Union (r1, r2) -> union (to_ext r1) (to_ext r2)
    | Star r -> EStar (to_ext r)
    | Empty -> failwith "Regexp should not contain Empty"
end