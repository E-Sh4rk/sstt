open Sstt

let () = Format.set_margin Format.pp_infinity

let true_t = Enum.mk("True") |> Descr.mk_enum |> Ty.mk_descr
let false_t = Enum.mk("False") |> Descr.mk_enum |> Ty.mk_descr
let int = Intervals.any |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def
let bool = Ty.cup true_t false_t


let ints = [
  Intervals.any;
  Intervals.Atom.mk (Some Z.one) None |> Intervals.mk;
  Intervals.Atom.mk None (Some (Z.of_int 2)) |> Intervals.mk;
  Intervals.Atom.mk_bounded (Z.of_int 7) (Z.of_int 10)|> Intervals.mk;
  [Intervals.Atom.mk_bounded Z.one (Z.of_int 6); Intervals.Atom.mk_bounded (Z.of_int 9)(Z.of_int 12)] |> Intervals.construct
]
let type_ints = List.map (fun t -> Descr.mk_intervals t |> VDescr.mk_descr |> Ty.of_def) ints


let enums = [
  Enums.any;
  Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]); 
  Enums.construct(false, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]);
  Enums.construct(false, [Enums.Atom.mk "a"; Enums.Atom.mk "aa";Enums.Atom.mk "aaa"])
]
let type_enums = List.map (fun t -> Descr.mk_enums t |> VDescr.mk_descr |> Ty.of_def) enums


let tuples = [
  Tuples.any;
  Tuples.mk [int; int ];
  Tuples.mk [int; bool; bool];
  Tuples.construct (true,[ Tuples.Comp.mk [int;int]; Tuples.Comp.mk[bool;bool; bool]; Tuples.Comp.mk[int]]);
  Tuples.construct (false,[ Tuples.Comp.mk [int;int]; Tuples.Comp.mk[bool;bool; bool]; Tuples.Comp.mk[int]]);
  Tuples.construct (true, [Tuples.Comp.mk [int;int]]);
  Tuples.construct (false, [Tuples.Comp.mk [int;int]]);
  Tuples.construct (false, [Tuples.Comp.mk [int; bool; bool]])
]
let type_tuple = List.map (fun t -> Descr.mk_tuples t |> VDescr.mk_descr |> Ty.of_def) tuples


let tags = [
  Tags.any;
  Tags.mk (Tag.mk("foo"),int);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int)]);
  Tags.construct(false, [TagComp.mk(Tag.mk "foo", int)]);
  Tags.construct(false, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", bool)]);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", bool)]);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", Ty.empty)]);
]
let type_tags = List.map (fun t -> Descr.mk_tags t |> VDescr.mk_descr |> Ty.of_def) tags


let tup_to_arrow =
  let rec creation l = match l with
      a :: b :: l -> (a,b) :: creation l 
    |_ -> [] 
  in creation type_tuple
let arrows = [
  Arrows.any;
  Arrows.mk (int, int);
  Arrows.mk (List.hd type_tuple, List.hd (List.tl type_tuple ));
  Arrows.of_dnf [(tup_to_arrow, [])];
  Arrows.of_dnf [[], tup_to_arrow];
]
let type_arrows = List.map (fun t -> Descr.mk_arrows t |> VDescr.mk_descr |> Ty.of_def) arrows

let records = [
  Row.mk [(Label.mk "int", int |> Ty.O.required |> Ty.F.mk_descr)] (Ty.F.mk_descr Ty.O.absent) ;
  Row.mk [] Ty.F.any;
  Row.mk [(Label.mk "l1",Ty.any |> Ty.F.OTy.required |> Ty.F.mk_descr); (Label.mk "l2", Ty.any |> Ty.F.OTy.required |> Ty.F.mk_descr)] Ty.F.any;
  Row.mk [] (int |> Ty.F.OTy.optional |> Ty.F.mk_descr);
  Row.mk [] (int |> Ty.F.OTy.required |> Ty.F.mk_descr);
  

]
let records2 = [
  {Records.Atom'.bindings = LabelMap.of_list []; tail = Ty.F.OTy.optional Ty.any |> Ty.F.mk_descr; exists = [(LabelSet.of_list [Label.mk "l1"],Ty.any |> Ty.F.OTy.required |> Ty.F.mk_descr)]} 
]
let type_records = List.map (fun a -> a|> Row.to_record_atom |> Descr.mk_record |> Ty.mk_descr) records @
List.map (fun a -> Records.of_dnf' [a]|> Descr.mk_records |> Ty.mk_descr ) records2

let int_bool_list = 
  let vx1 = Var.mk "'x1" in 
  let tx1 = Ty.mk_var vx1 in 
  Ty.of_eqs [(vx1,Ty.cup (Descr.mk_tuple [int;tx1] |> Ty.mk_descr) (Descr.mk_tuple [bool;bool] |> Ty.mk_descr))] |> VarMap.of_list |> VarMap.find vx1

let vx2 = Var.mk "'x2"
let tx2 = Ty.mk_var vx2
let tag_bool = Descr.mk_tag (Tag.mk"foo",bool) |> Ty.mk_descr
let tag_x2_tx2 = Descr.mk_tag (Tag.mk"oof",tx2) |> Ty.mk_descr
let tag_x2_tx2_tag_bool = Ty.cup  tag_bool tag_x2_tx2
let solved2 = Ty.of_eqs[(vx2, tag_x2_tx2_tag_bool)]
let bool_tag_list = solved2 |> VarMap.of_list |> VarMap.find vx2

let nil = Enum.mk "Nil" |> Descr.mk_enum |> Ty.mk_descr
let vt1 = Var.mk "'t1"
let tt1 = Ty.mk_var vt1
let int_t1_tt1 = Descr.mk_tuple [int;tt1] |> Ty.mk_descr
let nil_int_t1_tt1 = Ty.cup nil int_t1_tt1

let solvedt1 = Ty.of_eqs [(vt1, nil_int_t1_tt1)]
let t1 = solvedt1 |> VarMap.of_list |>VarMap.find vt1
let int_t1 = Descr.mk_tuple [int;t1] |> Ty.mk_descr
let t2 = Descr.mk_tuple [int_t1;t1] |> Ty.mk_descr
let rr1 = Row.mk [(Label.mk "x", bool |> Ty.O.required |> Ty.F.mk_descr); (Label.mk "t2", t2 |> Ty.O.required |> Ty.F.mk_descr)] (Ty.O.absent |> Ty.F.mk_descr)
let r1 = rr1 |> Row.to_record_atom |> Descr.mk_record |> Ty.mk_descr
let rec_types = [ bool_tag_list; int_bool_list;t1; t2; r1]


let list1 = [Ty.any;
             bool;
             true_t;
             false_t;
            ]

let vx1 = Var.mk "'x1"
let tx1 = Ty.mk_var vx1
let part1 = (Descr.mk_tag (Tag.mk "tag", tx1) |> Ty.mk_descr)
let a =Ty.cup 
    (42|> Z.of_int|> Intervals.Atom.mk_singl |>Descr.mk_interval |> Ty.mk_descr)
    (part1)
let a = Ty.of_eqs [(vx1, a)] |> VarMap.of_list |> VarMap.find vx1
let list2 = [
  Tags.mk (Tag.mk "tag", bool)|> Descr.mk_tags |> Ty.mk_descr;
  Ty.diff Ty.any bool;
  a;
  Tags.construct(false, [TagComp.mk (Tag.mk "tag1", bool);TagComp.mk (Tag.mk "tag2", int)]) |> Descr.mk_tags |> Ty.mk_descr;
]

let list3 = [
  TupleComp.mk [bool;int] |> Descr.mk_tuplecomp |> Ty.mk_descr;
  Tuples.any |> Descr.mk_tuples |> Ty.mk_descr;
  Tuples.construct (true, [TupleComp.mk [true_t;true_t]; TupleComp.mk [false_t;true_t]]) |> Descr.mk_tuples |> Ty.mk_descr;
  TupleComp.mk [bool; true_t] |> Descr.mk_tuplecomp |> Ty.mk_descr;
]

let tx = Var.mk "'X"
let vx = Ty.mk_var tx
let x = Ty.of_eqs [(tx,Ty.cup ((vx, Ty.any) |> Descr.mk_arrow |> Ty.mk_descr) bool)] |> VarMap.of_list |> VarMap.find tx
let x1 = Ty.cup ((x,Ty.any)|> Descr.mk_arrow |> Ty.mk_descr) true_t
let list4 = [
  x;
  x1
]
let all_types = type_ints @ type_enums @ type_tags @ type_tuple @ type_arrows @ type_records @ rec_types @ list1 @ list2 @list3 @list4


let%expect_test _ = 
  List.iter 

    (fun t -> let w = Witness.mk t in if Witness.is_in w t then 
        Format.printf "@[%a : %a@]@\n" 
          Printer.print_ty' t 
          Witness.pp w 
      else 
        Format.printf "FALSE : %a is not a witness of %a\n" Witness.pp w Printer.print_ty' t) 
    all_types;
  [%expect {|
    int : 42
    (1..) : 1
    (..2) : 2
    (7..10) : 7
    (1..6) | (9..12) : 1
    enum : " a "
    False | True : " False "
    enum \ (False | True) : " aaaaaa "
    enum \ (aaa | aa | a) : " aaaa "
    tag : a(16)
    foo(int) : foo(42)
    foo(int) : foo(42)
    tag \ foo(int) : a(16)
    tag \ (foo2(True | False) | foo(int)) : a(16)
    foo2(True | False) | foo(int) : foo2(True)
    foo(int) : foo(42)
    tuple : ( 0 )
    int, int : ( 42, 42 )
    int, True | False, True | False : ( 42, True, True )
    (int) | (int, int) | (True | False, True | False, True | False) : ( 42 )
    tuple \ ((int) | (int, int) | (True | False, True | False, True | False)) : ( 0, 1, 2, 3 )
    int, int : ( 42, 42 )
    tuple \ (int, int) : ( 0 )
    tuple \ (int, True | False, True | False) : ( 0 )
    arrow : fun :< empty -> empty >
    int -> int : fun :< int -> int >
    tuple -> (int, int) : fun :< tuple -> int, int >
    tuple -> (int, int) : fun :< tuple -> int, int >
    arrow & ~(tuple \ (int, int) -> tuple \ (int, True | False, True | False)) & ~((int, True | False, True | False) -> (int) | (int, int) | (True | False, True | False, True | False)) : fun :< empty -> empty >
    { int : int } : { int : 42 }
    record : {  }
    { l2 : any ; l1 : any ..} : { l2 : 42 ; l1 : 42 }
    {  ;; int? } : {  }
    {  ;; int } : {  ;; 42 }
    record \ { l1 : any? } : { aaa : 42 }
    x1 where x1 = foo(True | False) | oof(x1) : foo(True)
    x1 where x1 = (True | False, True | False) | (int, x1) : ( True, True )
    x1 where x1 = Nil | (int, x1) : " Nil "
    (int, x1), x1 where x1 = Nil | (int, x1) : ( (42, Nil), Nil )
    { t2 : (int, x1), x1 ; x : True | False } where x1 = Nil | (int, x1) : { t2 : (42, Nil), Nil ; x : True }
    any : 42
    True | False : " True "
    True : " True "
    False : " False "
    tag(True | False) : tag(True)
    ~(True | False) : 42
    x1 where x1 = 42 | tag(x1) : 42
    tag \ (tag2(int) | tag1(True | False)) : a(16)
    True | False, int : ( True, 42 )
    tuple : ( 0 )
    True, True : ( True, True )
    True | False, True : ( True, True )
    x1 where x1 = True | False | (x1 -> any) : " True "
    True | (x1 -> any) where x1 = True | False | (x1 -> any) : " True "
    |}]