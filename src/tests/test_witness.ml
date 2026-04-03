open Sstt

let test_Inter = 
  let simple = 
    let no_bounds = Intervals.any |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def in
    let low_bound = Intervals.Atom.mk (Some Z.one) None |> Intervals.mk |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def in
    let up_bound = Intervals.Atom.mk None (Some (Z.of_int 2)) |> Intervals.mk |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def in
    let all_bounds = Intervals.Atom.mk_bounded (Z.of_int 7) (Z.of_int 10)|> Intervals.mk |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def in 
    (*let not_an_inter = Enums.any |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def in*)


    let w_no_bounds = Witness.make no_bounds in
    let w_low_bound = Witness.make low_bound in
    let w_up_bound = Witness.make up_bound in
    let w_all_bounds = Witness.make all_bounds in
    (*let w_not_an_inter = Witness.make not_an_inter in *)
      Format.printf "Witness for %a is %a\n" Printer.print_ty' no_bounds Witness.pp w_no_bounds ; 
      Format.printf "Witness for %a is %a\n" Printer.print_ty' low_bound Witness.pp w_low_bound ;
      Format.printf "Witness for %a is %a\n" Printer.print_ty' up_bound Witness.pp w_up_bound ;
      Format.printf "Witness for %a is %a\n" Printer.print_ty' all_bounds Witness.pp w_all_bounds ;
      (*Format.printf "Witness for %a is %a\n" Printer.print_ty' not_an_inter Witness.pp w_not_an_inter*)
  in
  let disj_union = 
    let union1 =  [Intervals.Atom.mk (Some Z.one) (Some (Z.of_int 6)); Intervals.Atom.mk (Some (Z.of_int 9)) (Some (Z.of_int 12))] |> Intervals.construct |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def in 
    let w_union1 = Witness.make union1 in 
    Format.printf "Witness for %a is %a\n" Printer.print_ty' union1 Witness.pp w_union1 

  in simple; disj_union

let _test_Enums = 
  let simple =
    let enum_any = Enums.any |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def in
    let enum_true_bool = Enums.construct(true, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]) |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def in
    let enum_false_bool = Enums.construct(false, [Enums.Atom.mk "True"; Enums.Atom.mk "False" ]) |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def in

    let w_enum_any = Witness.make enum_any in
    
    let w_enum_true_bool = Witness.make enum_true_bool in
    let w_enum_false_bool = Witness.make enum_false_bool in


    Format.printf "Witness for %a is %a\n" Printer.print_ty' enum_any Witness.pp w_enum_any ;
    Format.printf "Witness for %a is %a\n" Printer.print_ty' enum_true_bool Witness.pp w_enum_true_bool;
    Format.printf "Witness for %a is %a\n" Printer.print_ty' enum_false_bool Witness.pp w_enum_false_bool
  in simple



let () = test_Inter