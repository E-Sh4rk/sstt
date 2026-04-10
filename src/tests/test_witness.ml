open Sstt

type test_t = 
|Int_test of Intervals.t
|Enum_test of Enums.t
|Tuple_test of Tuples.t
|Tag_test of Tags.t
|Arrow_test of Arrows.t
let int = Intervals.any |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def
let bool = Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]) |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def
let print_good typ i w t = 
  Format.printf "%s line %i : witness %a is an inhabitant of %a \n" 
  typ
  i
  Witness.pp w
  Printer.print_ty' t

let print_bad typ i w t = 
  Format.printf "Problem for %s line %i : witness %a encapsulated as %a is not an inhabitant of %a \n" 
  typ
  i
  Witness.pp w
  Printer.print_ty' (Witness.singl_to_Ty w)
  Printer.print_ty' t

let test to_test =
  let t = 
    match to_test with
    Int_test i -> Descr.mk_intervals i
    |Enum_test e -> Descr.mk_enums e 
    |Tuple_test u -> Descr.mk_tuples u
    |Tag_test ta -> Descr.mk_tags ta
    |Arrow_test a-> Descr.mk_arrows a
  in let t =  VDescr.mk_descr t|> Ty.of_def in
  let w = Witness.mk t in
  (t,w)

let rec test_list_inter l i print= 
  match l with 
    to_test :: l -> let t,w = test (Int_test to_test) in if (Witness.is_in w t) then begin 
      if print then print_good "Inter" i w t;test_list_inter l (i+1) print end else 
      print_bad "Inter" i w t
    
    |[] -> ()   

let ints = [
      Intervals.any;
      Intervals.Atom.mk (Some Z.one) None |> Intervals.mk;
      Intervals.Atom.mk None (Some (Z.of_int 2)) |> Intervals.mk;
      Intervals.Atom.mk_bounded (Z.of_int 7) (Z.of_int 10)|> Intervals.mk;
      [Intervals.Atom.mk_bounded Z.one (Z.of_int 6); Intervals.Atom.mk_bounded (Z.of_int 9)(Z.of_int 12)] |> Intervals.construct
    ]
let type_ints = List.map (fun t -> Descr.mk_intervals t |> VDescr.mk_descr |> Ty.of_def) ints

let rec test_list_enums l i print = 
  match l with 
  to_test :: l -> let t,w = test (Enum_test to_test) in if (Witness.is_in w t) then 
    begin if print then
      print_good "Enum" i w t;
      test_list_enums l (i+1) print
    end else 
    print_bad "Enum" i w t
  |[] -> () 
  
let enums = [
  Enums.any;
  Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]); 
  Enums.construct(false, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]);
  Enums.construct(false, [Enums.Atom.mk "a"; Enums.Atom.mk "aa";Enums.Atom.mk "aaa"])
]
let type_enums = List.map (fun t -> Descr.mk_enums t |> VDescr.mk_descr |> Ty.of_def) enums

let rec test_list_tuples l i print = 
  match l with 
    to_test :: l -> let t,w = test (Tuple_test to_test) in if (Witness.is_in w t) then
    begin
      if print then print_good "Tuple" i w t;
      test_list_tuples l (i+1) print
    end
  else print_bad "Tuple" i w t
    |[] -> ()

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
let rec _test_list_tags l i print =
  match l with 
    to_test :: l -> let t,w = test (Tag_test to_test) in if (Witness.is_in w t) then
    begin
      if print then print_good "Tag" i w t;
      _test_list_tags l (i+1) print
    end
  else print_bad "Tag" i w t
    |[] -> ()

let tags = [
  Tags.any;
  Tags.mk (Tag.mk("foo"),int);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int)]);
  Tags.construct(false, [TagComp.mk(Tag.mk "foo", int)]);
  Tags.construct(false, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", bool)]);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", bool)]);
  Tags.construct(true, [TagComp.mk(Tag.mk "foo", int); TagComp.mk(Tag.mk "foo2", Ty.empty)])
  
]
let type_tags = List.map (fun t -> Descr.mk_tags t |> VDescr.mk_descr |> Ty.of_def) tags
let rec test_list_arrows l i print=
  match l with 
  to_test :: l -> let t,w = test (Arrow_test to_test) in if (Witness.is_in w t) then
  begin
    if print then print_good "Arrow" i w t;
    test_list_arrows l (i+1) print
  end
  else print_bad "Arrow" i w t
  |[] -> ()


let to_use =
  let rec creation l = match l with
a :: b :: l -> (a,b) :: creation l 
|_ -> [] 
in creation type_tuple

let arrows = [
  Arrows.any;
  Arrows.mk (int, int);
  Arrows.mk (List.hd type_tuple, List.hd (List.tl type_tuple ));
  Arrows.of_dnf [(to_use, [])];
  Arrows.of_dnf [[], to_use]

]
let _type_arrows = List.map (fun t -> Descr.mk_arrows t |> VDescr.mk_descr |> Ty.of_def) arrows
let all_types = type_ints @ type_enums @ type_tags @ type_tuple

let rec test_list l i print =
  match l with 
    t :: l -> let w =  Witness.mk t in if (Witness.is_in w t) then
    begin
      if print then print_good "All_types" i w t;
      test_list l (i+1) print
    end
  else print_bad "All_types" i w t
    |[] -> ()

let vx1 = Var.mk "'x1"
let tx1 = Ty.mk_var vx1

let bool_bool = Descr.mk_tuple [bool;bool] |> Ty.mk_descr
let int_x1_tx1 = Descr.mk_tuple [int;tx1] |> Ty.mk_descr
let int_x1_tx1_bool_bool = Ty.cup  int_x1_tx1 bool_bool

let solved = Ty.of_eqs [(vx1, int_x1_tx1_bool_bool)]
let int_bool_list = solved |> VarMap.of_list |> VarMap.find vx1

let vx2 = Var.mk "'x2"
let tx2 = Ty.mk_var vx2
let tag_bool = Descr.mk_tag (Tag.mk"foo",bool) |> Ty.mk_descr
let tag_x2_tx2 = Descr.mk_tag (Tag.mk"oof",tx2) |> Ty.mk_descr
let tag_x2_tx2_tag_bool = Ty.cup tag_bool tag_x2_tx2
let rec_types = [ int_bool_list]

(*
let () = let rec prin l =
  match l with 
  (a,b) :: l -> Format.printf "%a -> %a\n" Printer.print_ty' a Printer.print_ty' b; prin l 
  |_ -> () in prin to_use
*)

let () = Format.printf "The type %a is empty : %b\n" Printer.print_ty' int_bool_list (int_bool_list |> Ty.get_descr |> Descr.get_arrows |> Descr.mk_arrows |> Ty.mk_descr |> Ty.is_empty)
let () = test_list_inter ints 1 false; 
test_list_enums enums 1 false;
test_list_tuples tuples 1 false;
(*test_list_tags tags 1 false;*)
test_list_arrows arrows 1 false;
test_list rec_types 1 true;
test_list all_types 1 true


(*Format.printf "Witness for Enum : %a is \"%a\"\n" Printer.print_ty' t Witness.pp w*)