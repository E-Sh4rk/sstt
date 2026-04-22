open Sstt
open Sstt_repl

let () = Format.set_margin Format.pp_infinity

let ints = [
  "int";
  "(1..)";
  "(..2)";
  "(7..10)";
  "(1..6) | (9..12)"
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
  "{ bool : true | false ; opt_int : int? ;; any }"
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

let mixup = [
  "any";
  "~(true | false)";
  "()";
  "x where x = tag( x & tag(x)) | 38";
  "~tag(true | false)";
  "~tag1(true | false) & ~tag2(int)";
  "(int,true | false) \\ (int,true)";
]

let all_tests = ints @
                enums @
                tags @
                tuples @
                arrows @
                records @
                recur @
                mixup

let type_all = List.map (fun  a -> 
    let r, _ = Ast.(build_ty 
                      empty_env 
                      (IO.parse_type a)) in 
    r)
    all_tests


let%expect_test _ = 
  List.iter 

    (fun t -> let w = Witness.mk t in if Witness.is_in w t then 
        Format.printf "@[%a : %a@]@\n" 
          Printer.print_ty' t 
          Witness.pp w 
      else 
        Format.printf "FALSE : %a is not a witness of %a\n" Witness.pp w Printer.print_ty' t) 
    type_all;
  [%expect {|
    int : 42
    (1..) : 1
    (..2) : 2
    (7..10) : 7
    (1..6) | (9..12) : 1
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
    arrow : arrow
    int -> int : int -> int
    (bool -> bool) & (int -> int) : (bool -> bool) & (int -> int)
    { int : int } : { int : 42 }
    record : {  }
    { l1 : any ; l2 : any ..} : { l1 : 42 ; l2 : 42 }
    {  ;; int? } : {  }
    {  ;; int } : {  ;; 42 }
    record \ { l1 : any? } : { aaa : 42 }
    {  ;; any } \ { y : any? ; x : any? ;; (~(true | false))? } : { aa : true ;; 42 }
    { bool : true | false ; opt_int : int? ;; any } : { bool : true ;; 42 }
    x1 where x1 = foo(true | false) | oof(x1) : foo(true)
    x1 where x1 = (int, x1) | (true | false, true | false) : true, true
    x1 where x1 = Nil | (int, x1) : " Nil "
    (int, x1), x1 where x1 = Nil | (int, x1) : (42, Nil), Nil
    { t2 : (int, x1), x1 ; x : true | false } where x1 = Nil | (int, x1) : { t2 : (42, Nil), Nil ; x : true }
    x1 where x1 = 42 | tag(x1) : 42
    x1 where x1 = true | false | (x1 -> any) : " true "
    true | (x1 -> any) where x1 = true | false | (x1 -> any) : " true "
    x2, x1 where x1 = nil | int | (x1, (x2, x1)) and x2 = tuple0 | (x2, x1) : tuple0, 42
    any : 42
    ~(true | false) : 42
    tuple0 : tuple0
    38 : 38
    ~tag(true | false) : 42
    ~(tag1(true | false) | tag2(int)) : 42
    (int, true | false) \ (int, true) : 42, false
    |}]