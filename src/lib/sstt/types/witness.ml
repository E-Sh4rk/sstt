open Core

type single_t = Int of Z.t
|String of string
|Arrow of Arrows.Atom.t
|Tuple of Tuples.Comp.t



let mk_intervals t = 
  let t_interval = Ty.get_descr t |> Descr.get_intervals |> Intervals.destruct |> List.hd |> Intervals.Atom.get in 
  match t_interval with 
  |(None, None) ->  Int (Z.of_int 42)
  |(Some z1, _) | (None, Some z1) -> Int z1

let mk_enums t = 
  let t_enum = Ty.get_descr t |> Descr.get_enums |> Enums.destruct in 
  match t_enum with
      |(false, lt_enum) -> String (String.make ((List.fold_left (fun a b -> max a (String.length (Enums.Atom.name b))) 0 lt_enum )+1) 'a')
      |(true, lt_enum) -> String (List.hd lt_enum |> Enums.Atom.name)

let mk_tuples t = 
  let t_tuple = Ty.get_descr t |> Descr.get_tuples |> Tuples.destruct in 
  match t_tuple with 
  |(true, lt_tuple) -> Tuple(List.hd lt_tuple)
  |(false, _lt_tuple) -> failwith "sfdsff" 




let mk_arrows t = 
   let a1,_a2 = Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd in 
   match a1 with 
   a :: _ -> Arrow a
   |[] -> failwith "oskour"


let make t = 
  let t_descr = Ty.get_descr t in 
  if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then mk_intervals t
  else 
  if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then mk_enums t
  else 
    if Descr.get_tuples t_descr|> Tuples.equal Tuples.empty |> not
      then mk_tuples t
  else
  if Descr.get_arrows t_descr |> Arrows.equal Arrows.empty |> not
    then mk_arrows t
  else failwith"TODO"






 
 
 let pp fmt single_t = match single_t with
  Int i -> Z.pp_print fmt i
  |String s -> Format.pp_print_string fmt s
  |Arrow (a1,a2) -> Format.pp_print_string fmt "fun :< "; Printer.print_ty' fmt a1; Format.pp_print_string fmt " -> "; Printer.print_ty' fmt a2; Format.pp_print_string fmt " >"
|_ -> failwith "TODO"

  
  let equal t1 t2 =
    match (t1,t2) with 
    Int t1, Int t2 -> Z.equal t1 t2
    |String t1, String t2 -> String.equal t1 t2
    |Arrow t1, Arrow t2 -> Arrows.equal (Arrows.mk t1) (Arrows.mk t2)
    |_ -> false

  let is_in singl t =
    let t' = match singl with
    Int i -> Intervals.Atom.mk_singl i |> Intervals.mk |> Descr.mk_intervals |> VDescr.mk_descr |> Ty.of_def
    |String s -> Enums.Atom.mk s |> Enums.mk |> Descr.mk_enums |> VDescr.mk_descr |> Ty.of_def
    |Arrow a -> Arrows.mk a |> Descr.mk_arrows |> VDescr.mk_descr |> Ty.of_def
    |_ -> failwith "TODO"
  in Ty.leq t' t