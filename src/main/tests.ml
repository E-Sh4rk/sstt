open Sstt_parsing
open Sstt_types

let%expect_test "tests" =
  let fn = "tests.txt" in
  let cin = open_in fn in
  let buf = Lexing.from_channel cin in
  let rec test env =
    match IO.parse_command buf with
    | End -> ()
    | Elt elt ->
      let env = Repl.treat_elt env elt in
      (*[%expect {| |}] ; *) test env
  in
  Output.with_basic_output Format.std_formatter
    (fun () -> test Repl.empty_env) () ;
  [%expect {|
    any1: true
    any2: false
    empty1: false
    empty2: true
    atom1: false
    atom2: true
    tags1: tag((false, true) | (true, false))
    tags2: tag1(true, false) | tag2(false, true)
    tags3: true
    tags4: 42
    tuple1: false
    tuple2: true
    tuple3: true
    tuple4: false
    tuple5: true
    tuple6: false
    tuples1: true
    tuples2: false
    tuples3: true
    tuples4: false
    tuples5: false
    tuples6: false
    record1: true
    record2: false
    record3: true
    record4: false
    record5: true
    record5: true
    arrow1: false
    arrow2: true
    arrow3: true
    arrow_inter1: true
    arrow_inter2: true
    arrow_inter3: false
    arrow_inter4: false
    rec1: true
    rec2: false
    rec3: false
    rec4: true
    rec5: false
    list1: false
    list2: true
    list3: false
    list4: false
    var1: true
    var2: false
    var3: empty
    var4: any
    var5: 'x
    print1: (int -> bool -> true) | (any -> any)
    print2: (false, false) | (true, true)
    print3: { l1 : true ; l2 : true ..} | { l1 : false ; l2 : true }
    print4: nil | int | (any, x1) where x1 = nil | (any, x1)
    print5: (int -> int) -> bool -> bool
    print6: 'b & ('a, 'b) | 'a
    print7: ~true
    print8: ~(any -> bool)
    print9: ~(any -> bool) | ~(true -> false)
    print10: ~((false, true) | (true, false))
    print11: bool
    print13: 'y
    print14: nil, (bool, x1) where x1 = nil | (bool, x1)
    print15: tuple \ tuple2
    print16: ~(40..44)
    print17: ~tag(42)
    print18: ('c -> 'd) & ('a -> 'b) & ~('g -> 'h) & ~('e -> 'f)
    tally1:
    tally2: [
              'X: 'X & 'y
            ]
    tally3: [
              'Y: 'Y | 'x
            ]
    tally4: [
              'X: 'Y & 'X
            ]
    tally5: [
              'X: 'X | 'x ;
              'Y: 'Y & 'y
            ]
    tally6: [
              'X: 'Z | 'X ;
              'Y: 'Y & bool
            ]
           [
             'Z: empty
           ]
    tally7: [
              'X: 'X & 'x ;
              'Y: 'Y & 'y
            ]
           [
             'X: empty
           ]
           [
             'Y: empty
           ]
    tally8: [
              'X: 'X & 'x ;
              'Y: 'Y & 'y ;
              'Z: 'Z & 'z
            ]
           [
             'X: empty
           ]
           [
             'Y: empty
           ]
           [
             'Z: empty
           ]
    tally9: [
              'Y: empty
            ]
    tally10: [
               'Y: empty
             ]
            [
              'Y: 'Y & 'y ;
              'A: 'A | 'a ;
              'B: 'B & 'b
            ]
    tally11: [
               'X: 'X | 'b | 'a ;
               'Y: 'Y & 'b & 'a
             ]
    tally12: [
               'X: empty
             ]
            [
              'X: any
            ]
            [
              'X: ~'B | 'A & 'X ;
              'A: ~'B | 'A
            ]
    tally13: [
               'X: empty
             ]
    tally14: [
               'X: 'Y & 'X
             ]
    tally15: [
               'X: int ;
               'Y: bool
             ]
    app1: int
    app2: any
    app3: (-5..5)
    app4: empty
    app5: bool
    |}]

open Extensions

let%expect_test "tests_ext" =
    let fn = "tests_ext.txt" in
    let cin = open_in fn in
    let buf = Lexing.from_channel cin in
    let pparams = [
      Lists.printer_params' ; Bools.printer_params' ; Chars.printer_params' ;
      Floats.printer_params' ; Strings.printer_params'
    ] |> Printer.merge_params in
    let rec test env =
      match IO.parse_command buf with
      | End -> ()
      | Elt elt ->
        let env = Repl.treat_elt ~pparams env elt in
        test env
    in
    let env = Repl.empty_env in
    let env = { env with Ast.tagenv=Ast.StrMap.add "lst" Lists.tag env.tagenv } in
    let env = { env with Ast.tagenv=Ast.StrMap.add "bool" Bools.tag env.tagenv } in
    let env = { env with Ast.tagenv=Ast.StrMap.add "flt" Floats.tag env.tagenv } in
    let env = { env with Ast.tagenv=Ast.StrMap.add "str" Strings.tag env.tagenv } in
    let env = { env with Ast.tagenv=Ast.StrMap.add "chr" Chars.tag env.tagenv } in
    Output.with_basic_output Format.std_formatter
      (fun () -> test env) () ;
    [%expect {|
      list_42_43: [ 42 43 any* ]
      int_list: [ int* ]
      list_not_only_int: [ (~int) | (~int) int+ | int+ ((~int) int*) ]
      list_union: [ 43 42 | 43 42 any+ | 42 | 42 any+ ]
      list_regexp: [ ((('b \ 'a) | (('b \ 'a)* 'a)+) ('b \ 'a)*)? ]
      list_with_vars: (42::('a & [ int* ]))
      char_any: char
      char_union: (('\000'-'1') | ('e'-'\255'))
      char_singl: '*'
      list_invalid: lst(int, lst(int, int))
      bool_invalid: bool(42)
      float_invalid: flt(42)
      string_invalid: str(42)
      char_invalid: chr(something)
      |}]
  