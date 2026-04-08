open Sstt



(*test for integers*)
let test_inters to_test =  
  let t = Descr.mk_intervals to_test|> VDescr.mk_descr |> Ty.of_def in
  let w = Witness.make t in
  (t,w) 

let rec test_list_inter l i= 
  match l with 
    to_test :: l -> let t,w = test_inters to_test in if (Witness.is_in w t) then test_list_inter l (i+1) else 
      Format.printf "Problem for Int line %i : witness %a is not an inhabitant of %a \n" 
      i
      Witness.pp w
      Printer.print_ty' t
    
    |[] -> ()   


let ints = [
      Intervals.any;
      Intervals.Atom.mk (Some Z.one) None |> Intervals.mk;
      Intervals.Atom.mk None (Some (Z.of_int 2)) |> Intervals.mk;
      Intervals.Atom.mk_bounded (Z.of_int 7) (Z.of_int 10)|> Intervals.mk;
      [Intervals.Atom.mk_bounded Z.one (Z.of_int 6); Intervals.Atom.mk_bounded (Z.of_int 9)(Z.of_int 12)] |> Intervals.construct
    ]





(*test for enumerators*)
let test_enums to_test =
  let t = Descr.mk_enums to_test|> VDescr.mk_descr |> Ty.of_def in
  let w = Witness.make t in (t,w)


let rec test_list_enums l i= 
  match l with 
  to_test :: l -> let t,w = test_enums to_test in if (Witness.is_in w t) then test_list_enums l (i+1) else 
    let w_capsule = match w with String s -> Enums.Atom.mk  s|> Enums.mk |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def |_ -> failwith "not possible" in 
    Format.printf "Problem for Enum line %i : witness \"%a\" encapsulated as %a is not an inhabitant of %a \n" 
    i
    Witness.pp w
    Printer.print_ty' w_capsule
    Printer.print_ty' t
  |[] -> () 
  
let enums = [
  Enums.any;
  Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]);
  Enums.construct(false, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]);
  Enums.construct(false, [Enums.Atom.mk "a"; Enums.Atom.mk "aa";Enums.Atom.mk "aaa"])
]


let test_arrows to_test = 
  let t = Descr.mk_arrows to_test |> VDescr.mk_descr |> Ty.of_def in
  let w = Witness.make t in (t,w)

let rec test_list_arrows l i=
  match l with 
  to_test :: l -> let t,w = test_arrows to_test in if (Witness.is_in w t) then test_list_arrows l (i+1) else
    Format.printf "Problem for Function line %i : witness %a is not an inhabitant of %a \n" 
      i
      Witness.pp w
      Printer.print_ty' t
      |[] -> ()

let arrows = [
  Arrows.any
]







let int_t = Intervals.any |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def
let bool_t = Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]) |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def

let a = Arrows.of_dnf [[(int_t,int_t)],[(bool_t,bool_t)] ] |> Descr.mk_arrows |> VDescr.mk_descr |> Ty.of_def
let () = Format.printf "%a \n" Printer.print_ty' a



let () = test_list_inter ints 1; 
test_list_enums enums 1;
test_list_arrows arrows 1


(*Format.printf "Witness for Enum : %a is \"%a\"\n" Printer.print_ty' t Witness.pp w*)