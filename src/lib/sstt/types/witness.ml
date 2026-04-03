open Core

type single_t = Int of Z.t
|String of string
|Arrow of (Arrows.Atom.t list * Arrows.Atom.t list)


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

let mk_arrows t = 
   let t_arrow = Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd in
   Arrow t_arrow


let make t = 
  let t_descr = Ty.get_descr t in 
  if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then mk_intervals t
  else 
  if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then mk_enums t
else 
  if Descr.get_arrows t_descr |> Arrows.equal Arrows.empty |> not then 
    mk_arrows t
else failwith"TODO"



 
 
 
 
 let pp fmt single_t = match single_t with
  Int i -> Z.pp_print fmt i
  |String s -> Format.print_string s
  |Arrow _a -> failwith "TODO"
 

  
