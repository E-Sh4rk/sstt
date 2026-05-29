open Sstt
open Sstt_repl

let () = Format.set_margin Format.pp_infinity

let ints = [
  "int";
  "('a,'a)";
  "(1..)";
  "(..2)";
  "(7..10)";
  "(1..6) | (9..12)";
  "42"
]

let enums = [
  "enum";
  "true | false";
  "true";
  "false";
  "enum \\ (true | false)";
  "enum \\ (aaa | aa | a)"
]

let tuples = [
  "tuple";
  "(int, int)";
  "true | false, int";
  "true, true";
  "true | false, true";
  "(int, (true | false), (true | false))";
  "(int, int) | ((true | false),(true | false),(true | false)) | (int)";
  "tuple \\ ((int, int) | ((true | false),(true | false),(true | false)) | (int))";
  "tuple \\ (int,int)";
  "tuple \\ (int, (true | false), (true | false))";
]
let tags = [
  "tag";
  "foo(int)";
  "tag(true | false)";
  "tag \\ foo(int)";
  "tag \\ (foo( true | false ) | foo2(int))";
  "tag \\ (tag2(int) | tag1(true | false)) ";
  "foo(true | false) | foo2(true | false)";
  "foo(int) | foo2(empty)";
  "foo(empty) | foo2(int)"
]
let arrows = [
  "arrow";
  "int -> int";
  "(int -> int) & (bool -> bool)"
]

let records = [
  "{int : int}";
  "record";
  "{l1 : any ; l2 :any ..}";
  "{  ;; int? }";
  "{  ;; int }";
  "record \\ { l1 : any? }";
  "{  ;; any } \\ { y : any? ; x : any? ;; (~(true | false))? }";
  "{ bool : true | false ; opt_int : int? ;; any }";
  "{ }";
  "{ ..} ";
  "{ l1 : bool ; l2 : true }";
  "{ l1 : true ; l2 : true ..} | { l1 : false ; l2 : true }";
  "{ l1 : true ; l2 : true ..} | { l1 : false ; l2 : true }";
  "{ l1 : bool ; l2 : true ..}";
  "{ l1:42 }";
  "{ l1:42 ;; int? }";
  "{ l1:42 ; l2:73 ..}";
  "{ l1:42 ..}";
  "{ l:42 ;; X|int } where X = { l:42 ;; X|int }";
  "{ ;; 41|42 }";
  "{ ;; 41 } | { ;; 42 }";
  "{ l1 : bool ; l2 : int ..} \\ { l1 : true ; l2 : int ..} ";
  "{ l1 : bool ; l2 : int ..} \\ { l1 : true ; l2 : int } ";
  "{ l1 : bool ; l2 : int } \\ { l1 : true ; l2 : int ..} ";
]

let others = [
  "~(int |enum | arrow| tag |tuple | record)";
  "other"
] 

let recur = [
  "x1 where x1 = foo(true | false) | oof(x1)";
  "x1 where x1 = (true | false, true | false) | (int, x1)";
  "x1 where x1 = Nil | (int, x1) ";
  "(int, x1), x1 where x1 = Nil | (int, x1) ";
  "{ t2 : (int, x1), x1 ; x : true | false } where x1 = Nil | (int, x1) ";
  "x1 where x1 = 42 | tag(x1) ";
  "x1 where x1 = true | false | (x1 -> any) ";
  "true | (x1 -> any) where x1 = true | false | (x1 -> any) ";
  "x2 where x1 = nil | int | (x1, x2) and x2 = (x2 | (),x1)";
]

let vars = [
  "'A";
  "'a";
  "('a,'b,'c)\\('a,'b)";
  "'a |'b";
  "'a \\ (int | enum |arrow)";
  "(record, 'a) \\ (record, 'b)";
  "(record, 'a) \\ (record, 'B)";
  "(record, 'A) \\ (record, 'b)";
  "(record, 'A) \\ (record, 'B)";
  "(record, 'b) \\ (record, 'a)";
  "'x & 'y";
  "'x | 'y";
  "('a,'b)";
  "'x & int | 'x \\ int";
  "'A -> 'b";
  "'A -> 'B";
  "('A -> 'B, 'Y)";
  "{ a: any ;; 'R }";
  "'g & ~'a";


]

let mixup = [
  "any";
  "~(true | false)";
  "()";
  "x where x = tag( x & tag(x)) | 38";
  "~tag(true | false)";
  "~tag1(true | false) & ~tag2(int)";
  "(int,true | false) \\ (int,true)";
  "{ b:'b ; a : 73 ;; r } where r = { a:int }";
  "x2 where
   x1 = {
        l1 : any -> any ;
        l2 : (x2 -> x1) & ({ l1 : int -> int ..} -> any)
   }
   and
   x2 = {
        l1 : (any -> (x1, x1)) & (any -> (x2, x2)) ;
        l2 : any -> any
   }";
  "{x : int | 'a | 'a -> 'a ; y: (int,int)}";
  "('a -> ('B -> 'B))-> (('B -> 'B)-> 'a)";
  "{x : 'a -> 'b} & 'a ";
  "x where x = (('a, x) | ('a, nil)) & 'a";
  "(x,'a) where x = ('a, x) | nil";
  "(('a & int) | 'b, ('a \\ int) | 'b)";
  "(('a & int) , ('a \\ int))";
  "('a,'a) \\ ('b,'a)";
  "('A,'A) \\ ('B, 'A)";
  "('a,~'a)";
  "(0 .. 4) \\ ('a \\ (0 .. 1))";
  "(0..4) \\ (((0..4) \\ 'a) & (0..4)) ";
  "'a \\ ('b | (0..3))";
  "('X, ~'X) \\ ('A,'B)";
  "'x \\ 'y";
  "'X \\ 'y";
  "'x \\ 'Y";
  "'X \\ 'Y";
  "('X -> 'Y) \\ ('x -> 'y)";
  "('X -> 'Y) \\ ('Z -> bool)";
  "('X, 'Y) \\ ('x, 'y)";
  "('X, 'Y, 'Z) \\ ('x, 'y, 'z)";
  "('A -> 'B, 'Y) \\ ('x, 'y)";
  "('A -> 'B, 'Y) \\ ('a -> 'b, 'y)";
  "('X -> 'Y, 'X -> 'Y) \\ ('a -> 'b, 'b -> 'a)";
  "('X, ~'X) \\ ('A, 'B)";
  "{ l : 'X ..} \\ { l : 'Y }";
  "{ l : 'X } \\ { l : 'Y ..} ";
  "'X, 'Y \\ int, bool";
  "'X -> 'Y \\ 'A -> 'B ";
  "'X & int -> 'X & int | 'Y \\ int -> 'Y ";
  "(~'b \\ 'a -> any) \\ (any -> empty), ~'b \\ 'a -> any";
  "(~int & 'A) \\ ('B | (int \\ 42))";
  "(~int | 'A) \\ ('B | (int \\ 42))";
]

let all_tests =
  if true then ints @
               enums @
               tags @
               tuples @
               arrows @
               records @
               others @
               recur @
               vars @
               mixup
  else ["('X -> 'Y, 'X -> 'Y) \\ ('a -> 'b, 'b -> 'a)"]
(*
let env = 
  let depart = ["type bool = true | false;;";
"type unit = ();;"] in 
List.fold_left (fun a ->)
  match boul |> Lexing.from_string |> IO.parse_command with 
  | End -> failwith "impossible"
  | Elt elt -> Repl.treat_elt Repl.empty_env elt
*)
let env = Ast.empty_env
let type_all = List.map 
    (fun  a -> 
       let r, _ = 
         Ast.(build_ty 
                env
                (IO.parse_type a)) in 
       r)
    all_tests
(*
let () = let stri = "x2, x1 where x1 = nil | int | (x1, (x2, x1)) and x2 = tuple0 | (x2, x1)" in 
let typ, _ = Ast.(build_ty empty_env (IO.parse_type stri)) in 
let tally = Tallying.tally MixVarSet.empty [(typ, Ty.empty)] in 
Format.printf "t : %a \n %!" Printer.print_ty' typ;
let v = typ |> Ty.vars |> VarSet.to_list |> List.map Ty.mk_var in 
Format.printf "vars : %a \n %!" (Format.pp_print_list Printer.print_ty') v ;
Format.printf "tally : %a \n %!" (Format.pp_print_list Printer.print_subst') tally 
*)

let check_trueness w t =
  let open Witness in 
  let (sigma,wit) = w in 
  Ty.leq (to_ty wit) (Subst.apply sigma t)



let true_witness w t = 
  if check_trueness w t then
    Format.printf "@[<h> %a : %a@]@\n"
      Printer.print_ty' t
      Witness.pp w
  else 
    Format.printf "FALSE : %a is not a witness of %a\n"
      Witness.pp w 
      Printer.print_ty' t

let%expect_test _ = 
  List.iter  
    (fun t -> let w = Witness.mk t in  true_witness w t)
    type_all;
  [%expect {|
    int : 42
    'a, 'a : 42, 42 with subst [ 'a : any ]
    (1..) : 1
    (..2) : 2
    (7..10) : 7
    (1..6) | (9..12) : 1
    42 : 42
    enum : " a "
    true | false : " true "
    true : " true "
    false : " false "
    enum \ (true | false) : " a "
    enum \ (aaa | aa | a) : " aaaa "
    tag : a(16)
    foo(int) : foo(42)
    tag(true | false) : tag(true)
    tag \ foo(int) : a(16)
    tag \ (foo(true | false) | foo2(int)) : a(16)
    tag \ (tag2(int) | tag1(true | false)) : a(16)
    foo(true | false) | foo2(true | false) : foo(true)
    foo(int) : foo(42)
    foo2(int) : foo2(42)
    tuple : 0
    int, int : 42, 42
    true | false, int : true, 42
    true, true : true, true
    true | false, true : true, true
    int, true | false, true | false : 42, true, true
    int | (int, int) | (true | false, true | false, true | false) : 42
    tuple \ ((int, int) | (true | false, true | false, true | false)) : 0
    tuple \ (int, int) : 0
    tuple \ (int, true | false, true | false) : 0
    arrow : fun < arrow >
    int -> int : fun < int -> int >
    (bool -> bool) & (int -> int) : fun < (bool -> bool) & (int -> int) >
    { int : int } : { int : 42 }
    record : {  }
    { l1 : any ; l2 : any ..} : { l1 : 42 ; l2 : 42 }
    {  ;; int? } : {  }
    {  ;; int } : {  ;; 42 }
    record \ { l1 : any? } : { aaa : 42 }
    {  ;; any } \ { y : any? ; x : any? ;; (~(true | false))? } : { aa : true ;; 42 }
    { bool : true | false ; opt_int : int? ;; any } : { bool : true ;; 42 }
    {  } : {  }
    record : {  }
    { l1 : bool ; l2 : true } : { l1 : bool ; l2 : true }
    { l1 : false ; l2 : true } | { l1 : true ; l2 : true ..} : { l1 : false ; l2 : true }
    { l1 : false ; l2 : true } | { l1 : true ; l2 : true ..} : { l1 : false ; l2 : true }
    { l1 : bool ; l2 : true ..} : { l1 : bool ; l2 : true }
    { l1 : 42 } : { l1 : 42 }
    { l1 : 42 ;; int? } : { l1 : 42 }
    { l1 : 42 ; l2 : 73 ..} : { l1 : 42 ; l2 : 73 }
    { l1 : 42 ..} : { l1 : 42 }
    { l : 42 ;; x1 } where x1 = int | { l : 42 ;; x1 } : {  ;; 42 }
    {  ;; (41..42) } : {  ;; 41 }
    {  ;; 41 } | {  ;; 42 } : {  ;; 41 }
    { l1 : bool ; l2 : int ..} : { l1 : bool ; l2 : 42 }
    { l1 : bool ; l2 : int ..} : { l1 : bool ; l2 : 42 }
    { l1 : bool ; l2 : int } : { l1 : bool ; l2 : 42 }
    ~(enum | arrow | int | tag | tuple | record) : #Other
    other : " other "
    x1 where x1 = foo(true | false) | oof(x1) : foo(true)
    x1 where x1 = (int, x1) | (true | false, true | false) : true, true
    x1 where x1 = Nil | (int, x1) : " Nil "
    (int, x1), x1 where x1 = Nil | (int, x1) : (42, Nil), Nil
    { t2 : (int, x1), x1 ; x : true | false } where x1 = Nil | (int, x1) : { t2 : (42, Nil), Nil ; x : true }
    x1 where x1 = 42 | tag(x1) : 42
    x1 where x1 = true | false | (x1 -> any) : " true "
    true | (x1 -> any) where x1 = true | false | (x1 -> any) : " true "
    x1, x2 where x1 = tuple0 | (x1, x2) and x2 = nil | int | (x2, (x1, x2)) : tuple0, 42
    'A : 42 with subst [ 'A : any ]
    'a : 42 with subst [ 'a : any ]
    'a, 'b, 'c : 42, 42, 42 with subst [ 'a : any ; 'b : any ; 'c : any ]
    'b | 'a : 42 with subst [ 'a : any ]
    'a \ (enum | arrow | int) : a(16) with subst [ 'a : any ]
    (record, 'a) \ (record, 'b) : {  }, 42 with subst [ 'a : any ; 'b : ~42 ]
    (record, 'a) \ (record, 'B) : {  }, 42 with subst [ 'a : any ; 'B : ~42 ]
    (record, 'A) \ (record, 'b) : {  }, 42 with subst [ 'A : any ; 'b : ~42 ]
    (record, 'A) \ (record, 'B) : {  }, 42 with subst [ 'A : any ; 'B : ~42 ]
    (record, 'b) \ (record, 'a) : {  }, 42 with subst [ 'b : any ; 'a : ~42 ]
    'y & 'x : 42 with subst [ 'x : any ; 'y : any ]
    'y | 'x : 42 with subst [ 'x : any ]
    'a, 'b : 42, 42 with subst [ 'a : any ; 'b : any ]
    'x : 42 with subst [ 'x : any ]
    'A -> 'b : fun < arrow > with subst [ 'A : empty ; 'b : empty ]
    'A -> 'B : fun < arrow > with subst [ 'A : empty ; 'B : empty ]
    'A -> 'B, 'Y : any -> any, 42 with subst [ 'A : any ; 'B : any ; 'Y : any ]
    { a : any ;; 'R } : {  ;; 42 } with subst [ 'R : any ]
    'g \ 'a : 42 with subst [ 'g : any ; 'a : ~42 ]
    any : 42
    ~(true | false) : 42
    tuple0 : tuple0
    38 : 38
    ~tag(true | false) : 42
    ~(tag1(true | false) | tag2(int)) : 42
    (int, true | false) \ (int, true) : 42, false
    { a : 73 ; b : 'b ;; { a : int } } : { a : 73 ; b : 42 ;; { a : 42 } } with subst [ 'b : any ]
    x2 where x1 = { l1 : any -> any ; l2 : (x2 -> x1) & ({ l1 : int -> int ..} -> any) } and x2 = { l1 : (any -> (x2, x2)) & (any -> (x1, x1)) ; l2 : any -> any } : { l1 : x2 ; l2 : any -> any } where x1 = { l1 : any -> any ; l2 : ({ l1 : x2 ; l2 : any -> any } -> x1) & ({ l1 : int -> int ..} -> any) } and x2 = (any -> ({ l1 : x2 ; l2 : any -> any }, { l1 : x2 ; l2 : any -> any })) & (any -> (x1, x1))
    { x : int | 'a -> 'a ; y : int, int } : { x : int -> empty ; y : 42, 42 } with subst [ 'a : empty ]
    ('a -> 'B -> 'B) -> ('B -> 'B) -> 'a : fun < arrow -> arrow -> empty > with subst [ 'a : empty ; 'B : empty ]
    'a & { x : 'a -> 'b } : { x : any -> any } with subst [ 'a : any ; 'b : any ]
    x1 where x1 = 'a & (('a, nil) | ('a, x1)) : 42, nil with subst [ 'a : any ]
    x1, 'a where x1 = nil | ('a, x1) : nil, 42 with subst [ 'a : any ]
    'a & int | 'b, 'a \ int | 'b : 42, a with subst [ 'a : any ; 'b : empty ]
    'a & int, 'a \ int : 42, a with subst [ 'a : any ]
    ('a, 'a) \ ('b, 'a) : 42, 42 with subst [ 'a : any ; 'b : ~42 ]
    ('A, 'A) \ ('B, 'A) : 42, 42 with subst [ 'A : any ; 'B : ~42 ]
    'a, ~'a : 42, 41 with subst [ 'a : 42 ]
    ~'a & (0..4) | (0..1) : 0 with subst [ 'a : empty ]
    'a & (0..4) : 0 with subst [ 'a : ~42 ]
    'a & ~'b & ~(0..3) : 42 with subst [ 'a : any ; 'b : ~42 ]
    ('X, ~'X) \ ('A, 'B) : 42, 41 with subst [ 'X : 42 ; 'A : ~42 ; 'B : any ]
    'x \ 'y : 42 with subst [ 'x : any ; 'y : ~42 ]
    'X \ 'y : 42 with subst [ 'X : any ; 'y : ~42 ]
    'x \ 'Y : 42 with subst [ 'x : any ; 'Y : ~42 ]
    'X \ 'Y : 42 with subst [ 'X : any ; 'Y : ~42 ]
    ('X -> 'Y) \ ('x -> 'y) : fun < (42 -> empty) \ (any -> empty) > with subst [ 'X : 42 ; 'Y : empty ; 'x : any ; 'y : empty ]
    ('X -> 'Y) \ ('Z -> bool) : fun < (any -> any) \ (any -> bool) > with subst [ 'X : any ; 'Y : any ; 'Z : any ]
    ('X, 'Y) \ ('x, 'y) : 42, 42 with subst [ 'X : any ; 'Y : any ; 'x : any ; 'y : ~42 ]
    ('X, 'Y, 'Z) \ ('x, 'y, 'z) : 42, 42, 42 with subst [ 'X : any ; 'Y : any ; 'Z : any ; 'x : any ; 'y : any ; 'z : ~42 ]
    ('A -> 'B, 'Y) \ ('x, 'y) : any -> any, 42 with subst [ 'A : any ; 'B : any ; 'Y : any ; 'x : ~(any -> any) ]
    ('A -> 'B, 'Y) \ ('a -> 'b, 'y) : 42 -> any, 42 with subst [ 'A : 42 ; 'B : any ; 'Y : any ; 'a : any ; 'b : empty ; 'y : empty ]
    ('X -> 'Y, 'X -> 'Y) \ ('a -> 'b, 'b -> 'a) : (42 -> empty) \ (any -> empty), 42 -> empty with subst [ 'X : 42 ; 'Y : empty ; 'a : any ; 'b : empty ]
    ('X, ~'X) \ ('A, 'B) : 42, 41 with subst [ 'X : 42 ; 'A : ~42 ; 'B : any ]
    { l : 'X ..} \ { l : 'Y } : { l : 42 } with subst [ 'X : any ; 'Y : empty ]
    { l : 'X } \ { l : 'Y ..} : { l : 42 } with subst [ 'X : any ; 'Y : ~42 ]
    'X, 'Y \ int, bool : 42, a, bool with subst [ 'X : any ; 'Y : any ]
    'X -> 'Y \ 'A -> 'B : fun < arrow > with subst [ 'X : empty ; 'Y : empty ; 'A : empty ; 'B : empty ]
    'X & int -> 'Y \ int | 'X & int -> 'Y : fun < arrow > with subst [ 'X : empty ; 'Y : empty ]
    (~'a \ 'b -> any) \ (any -> empty), ~'a \ 'b -> any : (any -> any) \ (any -> empty), any -> any with subst [ 'b : empty ; 'a : empty ]
    'A & ~'B & ~int : " a " with subst [ 'A : any ; 'B : 42 ]
    ~'B \ int | 'A & ~'B & ~(int \ 42) : " a " with subst [ 'B : int ]
    |}]