open Cduce_types

(* Subtyping bug
   https://gitlab.math.univ-paris-diderot.fr/cduce/cduce/-/issues/37
*)
let mk_type a b =
  let open Types in
  let x46 = cons any in
  let x458 = cons @@ diff any Sequence.nil_type in
  let x910 = cons @@ neg (cup a b) in
  let x919 = cons @@ neg (times x910 x46) in
  let x958 = cons @@ Sequence.star a in
  let x960 = cons @@ Sequence.star a in
  let p1 = times x458 x46 in
  let p2 = times x919 x46 in
  let p3 = times x960 x958 in
  cap p1 (cap (neg p2)  (p3))

let a = Types.var (Var.mk "a")
let b = Types.var (Var.mk "b")
let p1 = mk_type a b
let p2 = mk_type a b
let t = Types.diff p1 p2
let () =
  assert (Types.is_empty p1);
  assert (Types.is_empty p2);
  assert (Types.is_empty t);
  assert Types.(subtype (diff a b) any);
  assert Types.(subtype a (cup a b));
  assert Types.(subtype
                  (times (cons (diff a b)) (cons (cup a b)))
                  (times (cons any) (cons any)))