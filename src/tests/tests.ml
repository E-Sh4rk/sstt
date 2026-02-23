open Sstt
open Sstt_repl

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
    tags1: tag((true, false) | (false, true))
    tags2: tag1(true, false) | tag2(false, true)
    tags3: true
    tags4: 42
    tags5: ~tag(bool)
    tags6: ~(tag1(bool) | tag2(int))
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
    tuples7: int, false
    record1: true
    record2: false
    record3: true
    record4: false
    record5: true
    record6: true
    record7: false
    record8: true
    record9: false
    record10: true
    record11: { a : int }
    record12: { a : int ; b : 42 }
    record13: { a : 73 ; b : 42 }
    record14: { a : 73 ; b : 42 ;; `s }
    record15: empty
    record16: { l : 42 ;; x1 } where x1 = int | { l : 42 ;; x1 }
    record17: false
    record18: { l1 : false ; l2 : int ..}
    record19: { l1 : bool ; l2 : int ..} \ { l1 : true ; l2 : int }
    record20: { l1 : false ; l2 : int }
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
    print1: (any -> any) | (int -> bool -> true)
    print2: (true, true) | (false, false)
    print3: { l1 : false ; l2 : true } | { l1 : true ; l2 : true ..}
    print4: nil | int | (any, x1) where x1 = nil | (any, x1)
    print5: (int -> int) -> bool -> bool
    print6: 'b & ('a, 'b) | 'a
    print7: ~true
    print8: ~(any -> bool)
    print9: ~((any -> bool) & (true -> false))
    print10: ~((true, false) | (false, true))
    print11: bool
    print13: 'y
    print14: nil, (bool, x1) where x1 = nil | (bool, x1)
    print15: tuple \ tuple2
    print16: ~(40..44)
    print17: ~tag(42)
    print18: ('a -> 'b) & ('c -> 'd) & ~('e -> 'f) & ~('g -> 'h)
    print19: tag \ sometag(unit)
    print20: ~(bool | int | unit)
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
              'Z: empty
            ]
            [
              'X: 'Z | 'X ;
              'Y: 'Y & bool
            ]
    tally7: [
              'X: empty
            ]
            [
              'X: 'X & 'x ;
              'Y: 'Y & 'y
            ]
            [
              'Y: empty
            ]
    tally8: [
              'X: empty
            ]
            [
              'X: 'X & 'x ;
              'Y: 'Y & 'y ;
              'Z: 'Z & 'z
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
               'X: 'X | 'a | 'b ;
               'Y: 'Y & 'a & 'b
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
    tally16: [
               'A: empty
             ]
             [
               'X: 'A | 'X ;
               'B: any
             ]
             [
               'X: 'A | 'X ;
               'Y: 'B & 'Y
             ]
    tally_row1: [
                  `R: {  ;; `S & `R }
                ]
    tally_row2: [
                  `R: {  ;; `R & `s }
                ]
    tally_row3: [
                  `S: {  ;; `S | `r }
                ]
    tally_row4:
    tally_row5: [
                  `R: { a : `R ; b : 73 | `R ;; empty? | `R }
                ]
    tally_row6: [
                  'X: { a : empty? ; b : 73 | `R ;; empty? | `R } | 'X ;
                  `R: { a : `R ; b : 73 | `R ;; empty? | `R }
                ]
    tally_row7: [
                  'X: { a : empty? ; b : 73 | `R ;; empty? | `R } | 'X ;
                  `R: { a : 42 | `R ; b : 73 | `R ;; empty? | `R }
                ]
    tally_row8: [
                  'X: { a : 73 | 'A ; b : 73 | `R ;; empty? | `R } | 'X ;
                  'A: 73 | 'A ;
                  `R: { a : 42 | `R ; b : 73 | `R ;; empty? | `R }
                ]
    tally_row9: [
                  `A: { na_rm : 0 ; p : `A }
                ]
    app1: int
    app2: any
    app3: (-5..5)
    app4: empty
    app5: bool
    exttags1: [
                'X1: 'Y1 & 'X1 ;
                'X2: 'Y2
              ]
    exttags2: [
                'X1: 'X1 \ 'X2 | 'Y2 & 'Y1 & 'X1
              ]
    exttags3: [
                'X1: 'Y2 & 'Y1 & 'X1
              ]
              [
                'X2: 'Y2 & 'Y1 & 'X2
              ]
    exttags4: [
                'X1: 'Y2 \ 'X2 | 'Y1 \ 'X2 | 'X1
              ]
    exttags5: [
                'X1: 'Y2 | 'Y1 | 'X1
              ]
              [
                'X2: 'Y2 | 'Y1 | 'X2
              ]
    exttags6: tagandex(42)
    exttags7: ~tagorex()
    perf1: true
    perf2: (15..34)
    perf3: [
             'X: (15..34) | 'X
           ]
    perf4: true
    perf5: [
             'X: 'x25 & 'X
           ]
    |}]

open Extensions

let%expect_test "tests_ext" =
    let fn = "tests_ext.txt" in
    let cin = open_in fn in
    let buf = Lexing.from_channel cin in
    let abs_tag = Abstracts.define "abs" [Abstracts.Inv] in
    let abs_printer = Abstracts.printer_params abs_tag in
    let pparams = [
      Lists.printer_params ; Bools.printer_params ; Chars.printer_params ;
      Floats.printer_params ; Strings.printer_params ; Maps.printer_params ;
      abs_printer
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
    let env = { env with Ast.tagenv=Ast.StrMap.add "map" Maps.tag env.tagenv } in
    let env = { env with Ast.tagenv=Ast.StrMap.add "abs" abs_tag env.tagenv } in
    Output.with_basic_output Format.std_formatter
      (fun () -> test env) () ;
    [%expect {|
      list_42_43: [ 42 43 any* ]
      int_list: [ int* ]
      list_not_only_a: [ any any* (~'a) any* | (~'a) any* ]
      list_union: [ 43 42 any* | 42 any* ]
      list_regexp: [ ('b | 'a \ 'b)* ]
      list_with_vars: 42::('a & [ int* ])
      char_any: char
      char_union: ('\000'-'1') | ('e'-'\255')
      char_singl: '*'
      map_any: {{  }}
      map_ib: {{ int => bool }}
      not_map_ib: ~{{ int => bool }}
      map_not_ib: {{ int ~> bool }}
      map_ib_not_ib: empty
      map_ib_ii: {{ int => empty }}
      list_invalid: lst(int, lst(int, int))
      bool_invalid: bool(42)
      float_invalid: flt(42)
      string_invalid: str(42)
      char_invalid: chr(something)
      map_invalid: map(arrow)
      abs_any: abs
      abs_invalid: __abs(42)
      |}]
