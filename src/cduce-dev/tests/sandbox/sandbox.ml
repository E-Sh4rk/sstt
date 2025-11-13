(* Experiments *)
open Cduce_types
open Cduce_lib

let () = Format.set_margin 200

(** Typing of records: *)
let parse_type str =
  match[@alert "-deprecated"] Parse.pat (String.to_seq str) with
  | exception _ -> Types.empty
  | p -> Types.descr (Typer.typ Builtin.env p)

let re1 = parse_type "{ .. }"

let () =
  Format.printf "TYPE: %a\n" Types.Print.print re1;
  let b1, b2 = Types.Record.empty_cases re1 in
  Format.printf "EMPTY_CASES: %b, %b\n%!" b1 b2

let re2 = parse_type "{ ..} |{}| { a = Int } | { b = Bool .. }"

let () =
  let lab = Types.Record.first_label re2 in
  Format.printf "LABEL: %a\n%!" Ident.Label.print_attr lab;
  let l = Types.Record.get re2 in
  Format.printf "TYPE: %a\n%!" Types.Print.print re2;
  Format.printf "RECORD.GET:\n%!";
  List.iter
    (fun (map, o1, o2) ->
       Format.printf "%a, %b, %b\n%!"
         (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
            (fun ppf (l, (b, t)) ->
               Format.fprintf ppf "%a=%b %a" Ident.Label.print_attr l b
                 Types.Print.print t))
         (Ident.LabelMap.get map) o1 o2)
    l

let re3 = parse_type "{x = Int; y = Int } | {x = Bool; y = Int }"

let () =
  let l = Types.Record.focus re3 (Ident.Label.mk_ascii "x") in
  Format.printf "TYPE: %a\n" Types.Print.print re3;
  Format.printf "GET_THIS : %a\n" Types.Print.print (Types.Record.get_this l);
  Format.printf "NEED_OTHERS : %b\n" (Types.Record.need_others l)

let re4 = parse_type "{x = Int; y = Int } | {x = Char; y = Bool }"

let () =
  let l = Types.Record.focus re4 (Ident.Label.mk_ascii "x") in
  Format.printf "TYPE: %a\n" Types.Print.print re4;
  Format.printf "GET_THIS : %a\n" Types.Print.print (Types.Record.get_this l);
  Format.printf "NEED_OTHERS : %b\n%!" (Types.Record.need_others l)

(** Variables *)

let () =
  let pr_map vv =
    Var.Map.iteri
      (fun x nx -> Format.printf "%a → %a\n%!" Var.print x Var.print nx)
      vv;
    Format.printf "--\n%!"
  in
  let v = [ "toto"; "titi"; "toto"; "tutu"; "foo"; "bar"; "foo"; "foo" ] in
  let v = v @ v @ v @ v in
  let v = v @ v @ v @ v in
  let v = v @ v @ v @ v in
  let v = List.map Var.mk v in
  let v = Var.Set.from_list v in
  let v1 = Var.renaming v in
  let v2 = Var.full_renaming v in
  pr_map v1;
  pr_map v2

(* Polymorphic subtyping *)

let () =
  let va = Types.var (Var.mk "a") in
  let vb = Types.var (Var.mk "b") in
  let s1 = Types.(times (cons va) (cons Int.any)) in
  let s2 = Types.(times (cons vb) (cons vb)) in
  Format.printf "@[SUBTYPE @[";
  List.iter
    (fun (x, y) ->
       Format.printf "(%a) < (%a) : %b @\n" Types.Print.print x Types.Print.print
         y (Types.subtype x y))
    Types.
      [
        (va, vb);
        (s1, times (cons any) (cons Int.any));
        (s2, times (cons any) (cons any));
        (Sequence.star va, Sequence.star any);
        (cap Int.any va, Int.any);
        (cup (cap Int.any va) (cap Int.any vb), Int.any);
      ];
  Format.printf "@]@]\n%!"

(** Demo*)
let () =
  let t = Types.Int.any in
  let s = Types.interval Intervals.(left (V.from_int 42)) in
  let u = Types.(times (cons t) (cons s)) in
  let v = Types.(arrow (cons t) (cons u)) in
  let w = Types.(arrow (cons Types.Char.any) (cons Types.Char.any)) in
  let x = Types.cap v w in
  let res =
    Types.Arrow.apply (Types.Arrow.get x) (Types.cup s Types.Char.any)
  in
  Format.printf "My Type: %a\n%!" Types.Print.print u;
  Format.printf "My Type: %a\n%!" Types.Print.print x;
  Format.printf "My Type: %a\n%!" Types.Print.print res

(** Substitutions *)
let () =
  let va = Var.mk "a" in
  let vb = Var.mk "b" in
  let vc = Var.mk "c" in
  let subst =
    Var.Map.from_list
      (fun _ _ -> assert false)
      [
        (va, Types.Int.any);
        (vb, Types.(times (cons Types.Char.any) (cons Types.Char.any)));
      ]
  in
  let t1 = Types.var va in
  let t2 = Types.Sequence.star (Types.var vb) in
  let t3 = Types.(times (cons t1) (cons (var vc))) in
  let t4 = Types.diff Types.Function.any Types.(arrow (cons t1) (cons t1)) in
  let t5 = parse_type " X where X = <foo>[(X 'a X 'b)*]" in

  let subst2 =
    Var.Map.from_list
      (fun _ _ -> assert false)
      (List.map (fun x -> (x, t4)) (Var.Set.get (Types.Subst.vars t5)))
  in
  let subst = Var.Map.merge (fun _ _ -> assert false) subst subst2 in
  Format.printf "@[SUBSTITUTION @[";
  List.iter
    (fun t ->
       let ts = Types.Subst.apply_full subst t in
       Format.printf "%a => %a @\n" Types.Print.print t Types.Print.print ts)
    [ t1; t2; t3; t4; t5 ];
  Format.printf "@]@]\n%!"

(* Solve rectypes*)
let () =
  let a = Var.mk "a" in
  let t = Types.(cup Sequence.nil_type (times (cons Int.any) (cons (var a)))) in
  Format.printf "@[SOLVE RECTYPE@[ ";
  List.iter
    (fun (v, t) ->
       let s = Types.Subst.solve_rectype t v in
       Format.printf "%a = %a → %a@\n" Var.print v Types.Print.print t
         Types.Print.print s)
    [ (a, t) ];
  Format.printf "@]@]\n%!"

(** Tallying *)
let () =
  let a = Var.mk "a" in
  let b = Var.mk "b" in
  let c = Var.mk "c" in
  let d = Var.mk "d" in
  let vtimes x y = Types.(times (cons (var x)) (cons (var y))) in
  let delta = Var.Set.singleton a in
  let s = Types.Sequence.star (Types.var b) in
  let t = Types.Sequence.star Types.Int.any in
  let s1 = Types.(arrow (cons (var b)) (cons (var b))) in
  let t1 = Types.(arrow (cons Char.any) (cons Char.any)) in
  let s2 = Types.Sequence.star Types.(cup Char.any Int.any) in
  let t2 = Types.Sequence.star (Types.var b) in
  let s3 = Types.(times (cons (var b)) (cons (var c))) in
  let t3 = Types.(times (cons Atom.any) (cons Atom.any)) in
  Format.printf "@[TALLYING@[@\n";
  List.iter
    (fun l ->
       let res = Types.Tallying.tallying delta l in
       let open Format in
       let open Custom.Print in
       printf "%a ~> %a@\n"
         (pp_list (fun ppf (s, t) ->
              fprintf ppf "%a <? %a" Types.Print.print s Types.Print.print t))
         l Types.Subst.print_list res)
    [
      [ (vtimes a b, vtimes c d) ];
      [ (vtimes a c, vtimes b d) ];
      [ (vtimes c d, vtimes a b) ];
      [ (vtimes a d, vtimes b c) ];
      [ (s, t) ];
      [ (s1, t1) ];
      [ (s2, t2) ];
      [ (s3, t3); (Builtin_defs.bool, Types.(var b)) ];
      [ (Types.Int.any, Types.any) ];
      [ (Types.any, Types.empty) ];
    ];
  Format.printf "@]@]\n%!"

(** Square apply *)
let () =
  let t_even = parse_type "(('c\\Int) -> ('c\\Int)) & (Int -> Bool)" in
  let t_map = parse_type "('a -> 'b) -> [ 'a * ] -> [ 'b * ]" in
  let f1 = parse_type "('a -> 'a)" in
  let arg1 = Types.Int.any in
  let open Format in
  eprintf "@[SQUARE APPLY@[@\n";
  List.iter
    (fun (f, arg) ->
       match Types.Tallying.apply_raw Var.Set.empty f arg with
       | Some (subst, ff, aa, res) ->
         eprintf "@[(%a) • (%a)@] ~>@[fun=%a@\narg=%a@\nsubst=%a@\nres=%a@]@\n"
           Types.Print.print f Types.Print.print arg Types.Print.print ff
           Types.Print.print aa Types.Subst.print_list subst Types.Print.print
           res
       | None ->
         eprintf "@[(%a) • (%a)@] ~> Ill-typed" Types.Print.print f
           Types.Print.print arg)
    [ (f1, arg1); (t_map, t_even) ];
  eprintf "@]@]\n%!"

(** Debug the order of variables choosen during tallying. *)
let () =
  let va = Var.mk "a" in
  let vb = Var.mk "b" in
  let vc = Var.mk "c" in
  let vd = Var.mk "d" in
  let a = Types.var @@ va in
  let b = Types.var @@ vb in
  let c = Types.var @@ vc in
  let d = Types.var @@ vd in
  let vtimes x y = Types.(times (cons x) (cons y)) in
  let varrow x y = Types.(arrow (cons x) (cons y)) in
  Format.printf "@[TALLYING@[@\n";
  List.iter
    (fun l ->
       List.iter
         (fun var_order ->
            let res = Types.Tallying.tallying ~var_order Var.Set.empty l in
            let open Format in
            let open Custom.Print in
            printf "(%a) %a ~> %a@\n" (pp_list Var.print) var_order
              (pp_list (fun ppf (s, t) ->
                   fprintf ppf "%a <? %a" Types.Print.print s Types.Print.print t))
              l Types.Subst.print_list res)
         [ []; [ va; vb; vc; vd ]; [ vd; vc; vb; va ]; [ vc; vb; va; vd ] ])
    [
      [ (vtimes a b, vtimes c d) ];
      [ (vtimes a c, vtimes b d) ];
      [ (vtimes c d, vtimes a b) ];
      [ (vtimes a d, vtimes b c) ];
      [ (varrow a b, varrow c d) ];
      [ (varrow a c, varrow b d) ];
      [ (varrow c d, varrow a b) ];
      [ (varrow a d, varrow b c) ];
      [ (varrow a Types.Int.any, varrow Types.Int.any d) ];
      [ (varrow d Types.Int.any, varrow Types.Int.any a) ];
      [ (varrow a b, varrow Types.Int.any c) ];
      [ (varrow Types.Int.any c, varrow a b) ];
      [ (varrow Types.Int.any c, varrow a d) ];
      [ (varrow a d, varrow Types.Int.any c) ];
    ];
  Format.printf "@]@]\n"

(*Min/Max types:*)
let () =
  let a = Types.var @@ Var.mk "a" in
  let b = Types.var @@ Var.mk "b" in
  let c = Types.var @@ Var.mk "c" in
  let d = Types.var @@ Var.mk "d" in
  let vset = Types.Subst.vars d in
  let vtimes x y = Types.(times (cons x) (cons y)) in
  let varrow x y = Types.(arrow (cons x) (cons y)) in
  let t1 = vtimes a a in
  let t2 = varrow t1 b in
  let t3 = varrow c (varrow b a) in
  let t4 = Types.(vtimes (neg a) (neg a)) in
  let t5 = Types.neg (varrow a a) in
  let t6 = varrow (Types.Sequence.star a) a in
  let t7 = varrow d (vtimes c d) in
  Format.printf "@[Min/Max type@[\n";
  List.iter
    (fun t ->
       let open Format in
       printf "type: %a, vars: %a, min: %a, max: %a\n%!" Types.Print.print t
         Var.Set.print vset Types.Print.print
         (Types.Subst.min_type vset t)
         Types.Print.print
         (Types.Subst.max_type vset t))
    [ t1; t2; t3; t4; t5; t6; t7 ];
  Format.printf "@]@]\n"

let () =
  let a = Types.var @@ Var.mk "a" in
  let b = Types.var @@ Var.mk "b" in
  let c = Types.var @@ Var.mk "c" in
  let t1 = Types.(times (cons a) (cons b)) in
  let t2 = Types.(arrow (cons a) (cons b)) in
  let t3 = Types.(arrow (cons a) (cons a)) in
  let t4 = Types.diff a b in
  let t5 = Builtin_defs.seq_type (Types.cons t3) in
  let t6 = Builtin_defs.seq_type (Types.cons t1) in
  let l = [ a; b; c; t1; t2; t3; t4; t5; t6 ] in
  Format.printf "@[var_polarities@[@\n";
  List.iter
    (fun t ->
       let open Format in
       let pr_pol fmt (v, x) =
         fprintf fmt "(%a,%s)" Var.print v
           (match x with
            | `Pos -> "+"
            | `Neg -> "-"
            | `Both -> "=")
       in
       let pr_map fmt m =
         let l = Var.Map.get m in
         fprintf fmt "{%a}"
           (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pr_pol)
           l
       in
       let m = Types.Subst.var_polarities t in
       printf "@[%a : %a@]@\n" Types.Print.print t pr_map m)
    l;
  Format.printf "@]@\n"

let () =
  let c = Var.mk "c" in
  let tc = Types.var c in
  let r = Var.mk "r" in
  let tr = Types.var r in
  let ctc = Types.cons tc in
  let t1 = Types.arrow ctc ctc in
  let t2 =
    Types.arrow
      (Types.cons (Types.interval Intervals.(atom (V.from_int 42))))
      (Types.cons tr)
  in
  let t3 =
    Types.rec_of_list true
      [
        (false, Ident.Label.mk_ascii "x", tc);
        (false, Ident.Label.mk_ascii "y", Types.Int.any);
        (false, Ident.Label.mk_ascii "z", Types.Int.any);
        (false, Ident.Label.mk_ascii "t", tc);
      ]
  in
  let t4 =
    Types.rec_of_list true
      [
        (false, Ident.Label.mk_ascii "x", Builtin_defs.bool);
        (false, Ident.Label.mk_ascii "y", tr);
      ]
  in
  let l =
    [ ([], t1, t2); ([ r; c ], t1, t2); ([ c; r ], t1, t2); ([], t3, t4) ]
  in
  List.iter
    (fun (var_order, s, t) ->
       let res = Types.Tallying.tallying ~var_order Var.Set.empty [ (s, t) ] in
       let open Format in
       printf "Tallying %a, %a <? %a = %a @\n" (pp_print_list Var.print)
         var_order Types.Print.print s Types.Print.print t Types.Subst.print_list
         res)
    l

(* Tallying bug *)
let () =
  let a = Var.mk "a" in
  let ta = Types.var a in
  let b = Var.mk "b" in
  let tb = Types.var b in
  let any_n = Types.cons Types.any in
  let mk_tree v =
    let r = Types.make () in
    let seq_r = Builtin_defs.seq_type r in
    let seq_r_cup =
      Types.cup seq_r (Types.diff v (Builtin_defs.seq_type any_n))
    in
    Types.define r seq_r_cup;
    seq_r_cup
  in
  let mk_flatten v =
    Types.(arrow (cons @@ mk_tree v) (cons @@ Builtin_defs.seq_type (cons v)))
  in
  let fa = mk_flatten ta in
  let fb = mk_flatten tb in
  let l = [ (Var.Set.singleton a, fb, fa) ] in
  List.iter
    (fun (delta, s, t) ->
       let open Format in
       let () =
         printf "Tallying DEBUG %a : %a vs %a @\n" Var.Set.print delta
           Types.Print.print s Types.Print.print t
       in
       let res = Types.Tallying.tallying delta [ (s, t) ] in
       List.iter
         (fun subst ->
            let ss = Types.Subst.apply subst s in
            let tt = Types.Subst.apply subst t in
            let b = Types.subtype ss tt in
            printf "Tallying yield %a <? %a = %b %a @\n----@\n" Types.Print.print
              ss Types.Print.print tt b Types.Subst.print subst)
         res)
    l
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
let () = Format.printf "P1: %a, P2: %a, P1\\P2: %a, is_empty: %b@\n@\n======@\n%a@\n"
    Types.Print.print p1
    Types.Print.print p2
    Types.Print.print t
    (Types.is_empty p1)
    Types.print_witness (a)


let u = Types.(times (cons a) (cons b))
let v = Types.(times (cons any) (cons any))
let () = Format.printf "%a %b@\n" Types.Print.print u
(Types.subtype u v)
