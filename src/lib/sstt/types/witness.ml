open Core

type single_t = Int of Z.t
|Enum of Enums.Atom.t
|Arrow of Arrows.t
|Tuple of single_t list
|Tag of (Tag.t * single_t)
|Other
|Wrong
|Empty


let rec singl_to_Ty s =
  match s with 
  Int i -> Intervals.Atom.mk_singl i |> Descr.mk_interval |> VDescr.mk_descr |> Ty.of_def
  |Enum e -> Descr.mk_enum e|> VDescr.mk_descr |> Ty.of_def
  |Tuple tu -> List.map singl_to_Ty tu |> Descr.mk_tuple |> VDescr.mk_descr |> Ty.of_def
  |Tag (ta,ty) -> (ta, singl_to_Ty ty) |> Descr.mk_tag |> VDescr.mk_descr |> Ty.of_def
  |Arrow a -> Descr.mk_arrows a |> VDescr.mk_descr |> Ty.of_def
  |Other -> Descr.mk_others true |> VDescr.mk_descr |> Ty.of_def
  |Wrong -> Ty.empty
  |Empty -> Ty.empty
  
let mk_intervals t = 
  let t_interval = Ty.get_descr t |> Descr.get_intervals |> Intervals.destruct |> List.hd |> Intervals.Atom.get in 
  match t_interval with 
    |(None, None) ->  Int (Z.of_int 42)
    |(Some z1, _) | (None, Some z1) -> Int z1
  
let mk_enums t = 
  let t_enum = Ty.get_descr t |> Descr.get_enums |> Enums.destruct in 
  match t_enum with
    |(true, lt_enum) -> Enum (List.hd lt_enum)
    |(false, lt_enum) -> Enum (Enums.Atom.mk (String.make ((List.fold_left (fun a b -> max a (String.length (Enums.Atom.name b))) 0 lt_enum )+1) 'a'))
  

    
let mk_arrows t = 
    let a1,a2 = Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd in
    let rec help_arrow t a1 a2 a3 = 
      let test_arrow = Arrows.of_dnf [(a1,a2)] |> Descr.mk_arrows |> Ty.mk_descr in if Ty.leq test_arrow t then Arrows.of_dnf [a1,a2] 
    else begin 
      match a3 with 
      a:: l -> help_arrow t a1 (a :: a2) l
      |_ -> [Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd] |> Arrows.of_dnf
      end in
     Arrow(help_arrow t a1 [] a2)

let rec mk_tuple_atoms make mem t len a1 a2 a3 = 
  let test_tuple = Tuples.construct (true,[TupleComp.of_dnf len [(a1,a2)]]) |> Descr.mk_tuples |> Ty.mk_descr in 
  if Ty.leq test_tuple t then let w = Tuple(List.map (fun a -> let _,w = make mem a in w) (List.hd (Op.TupleComp.as_union(TupleComp.of_dnf len [a1,a2] )))) in ((Ty.hash t)::mem,w)
else begin 
  match a3 with 
  a:: l -> mk_tuple_atoms make mem t len a1 (a :: a2) l
  |_ ->  let w = Tuple(List.map (fun a -> let _,w =make mem a in w) (List.hd (Op.TupleComp.as_union(TupleComp.of_dnf len [a1,a2] )))) in ((Ty.hash t)::mem,w)
  end 

let rec mk_tuple_atom_list_atom_list_list make mem t len pn_list = match pn_list with 
(p,n) :: l -> let (mem',w) = mk_tuple_atoms make mem t len p [] n in if (w == Empty ) then mk_tuple_atom_list_atom_list_list make mem t len l else ((Ty.hash t)::mem',w)
|[] -> (mem,Empty)
let rec mk_tuplecomp_list make mem t lt_tuple = match lt_tuple with 
a::l -> let pn_list = TupleComp.dnf a in 
      let (mem',w) = mk_tuple_atom_list_atom_list_list make mem t (TupleComp.len a) pn_list in if (w == Empty) then mk_tuplecomp_list make mem t l else ((Ty.hash t)::mem',w) 
|[] -> (mem,Empty)

let mk_tuples_mem mk_mem mem t = let t_tuple = Ty.get_descr t |> Descr.get_tuples |> Tuples.destruct in 
match t_tuple with 
  |(true, lt_tuple) -> mk_tuplecomp_list mk_mem mem t lt_tuple
  |(false, lt_tuple) -> (Ty.hash t::mem,Tuple(List.init ((List.fold_left (fun a b -> max a (Tuples.Comp.len b)) 0 lt_tuple)+1) (fun x -> Int(Z.of_int x))))

let rec mk_tag_atom make mem t tag a1 a2 a3 = 
  let test_tag = Tags.construct (true,[TagComp.of_dnf tag [(a1,a2)]]) |> Descr.mk_tags |> Ty.mk_descr in 
  if Ty.leq test_tag t then let _,w = make mem (let _,b =List.hd (Op.TagComp.as_union(TagComp.of_dnf tag [a1,a2] )) in b) in ((Ty.hash t)::mem,Tag(tag,w)) else
  match a3 with 
  a:: l -> mk_tag_atom make mem t tag a1 (a :: a2) l
  |_ ->  let _,w = make mem (let _,b = List.hd (Op.TagComp.as_union(TagComp.of_dnf tag [a1,a2] )) in b) in ((Ty.hash t)::mem,Tag(tag,w))
   

let rec mk_tag_atoms_list_atoms_list_list make mem t tag pn_list =
  match pn_list with 
  (p,n) :: l -> 
    let (mem',w) = mk_tag_atom make mem t tag p [] n in 
    if (w == Empty ) 
      then mk_tag_atoms_list_atoms_list_list make mem t tag l 
    else ((Ty.hash t)::mem',w)
  |[] -> (mem,Empty)
let rec mk_tagcomp_list make mem t lt_tag =
  match lt_tag with 
  a::l -> let pn_list = TagComp.dnf a in 
  let (mem',w) = mk_tag_atoms_list_atoms_list_list make mem t (TagComp.tag a) pn_list in if (w == Empty) then mk_tagcomp_list make mem t l else ((Ty.hash t)::mem',w) 
|[] -> (mem,Empty)


let mk_tag_mem mk_mem mem t = 
  let t_tag = Ty.get_descr t |> Descr.get_tags |> Tags.destruct in 
  match t_tag with 
  | (true,co_l) -> mk_tagcomp_list mk_mem mem t co_l
  | (false, co_l) -> 
  let new_tag = Tag.mk (String.make ((List.fold_left (fun a b -> let b = TagComp.tag b |> Tag.name |> String.length in if (b > a) then b else a) 1 co_l)+1) 'a') in 
  let new_type = Int(Z.of_int 16)  in let w = Tag(new_tag,new_type) in ((Ty.hash t)::mem, w)


let rec mk_mem mem t = 
  if List.mem (Ty.hash t) mem then (mem,Empty) 
  else
  let t_descr = Ty.get_descr t in 
  if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then (mem,mk_intervals t)
  else 
  if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then (mem,mk_enums t)
  else 
  if Descr.get_arrows t_descr |> Descr.mk_arrows |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not
    then (mem,mk_arrows t)
  else 
  if Descr.get_tags t_descr|> Descr.mk_tags |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not 
    then mk_tag_mem mk_mem mem t
  else
  if Descr.get_tuples t_descr|> Descr.mk_tuples |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not
    then mk_tuples_mem mk_mem mem t
  else failwith "Type not implemented yet"



























    
let _testing make mem t = 
  let t_tuple = Ty.get_descr t |> Descr.get_tuples |> Tuples.destruct in
match t_tuple with 
|(true, lt_tuple) -> 
let rec inside_story make mem lt_tuple =
  begin
    match lt_tuple with 
  |a::l -> if List.mem a mem then inside_story make mem l else Tuple(List.map (fun b -> make (a::mem) b) (List.hd (Op.TupleComp.as_union a)) )
  |_ -> failwith "oskour"

  end in inside_story make mem lt_tuple



|(false, lt_tuple) -> Tuple(List.init ((List.fold_left (fun a b -> max a (Tuples.Comp.len b)) 0 lt_tuple)+1) (fun x -> Int(Z.of_int x)))

let mk_tuples make mem t = let t_tuple = Ty.get_descr t |> Descr.get_tuples |> Tuples.destruct in 
match t_tuple with 
  |(true, lt_tuple) -> let fst_tuple = List.hd lt_tuple in  Tuple(List.map (fun a -> make mem a) (List.hd (Op.TupleComp.as_union fst_tuple)) )
  |(false, lt_tuple) -> Tuple(List.init ((List.fold_left (fun a b -> max a (Tuples.Comp.len b)) 0 lt_tuple)+1) (fun x -> Int(Z.of_int x)))

let mk_tags make mem t =
  let t_tag = Ty.get_descr t |> Descr.get_tags |> Tags.destruct in 
  match t_tag with 
  | (true,co_l) -> let fst_tag = List.hd co_l in let ta,t = Op.TagComp.as_union fst_tag |> List.hd in Tag(ta, make mem t)
  | (false, co_l) -> let new_tag = Tag.mk (String.make ((List.fold_left (fun a b -> let b = TagComp.tag b |> Tag.name |> String.length in if (b > a) then b else a) 1 co_l)+1) 'a') in 
let new_type = Int(Z.of_int 16)  in Tag(new_tag,new_type)



let rec _make mem t = 
  let t_descr = Ty.get_descr t in 
  if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then mk_intervals t
  else 
  if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then mk_enums t
  else 
  if Descr.get_tuples t_descr|> Descr.mk_tuples |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not
    then mk_tuples _make mem t
  else
  if Descr.get_tags t_descr|> Descr.mk_tags |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not 
    then mk_tags _make mem t
  else
  if Descr.get_arrows t_descr |> Descr.mk_arrows |> VDescr.mk_descr |> Ty.of_def |> Ty.is_empty |> not
    then mk_arrows t
  else 
  if Descr.get_records t_descr |> Descr.mk_records |> VDescr.mk_descr |>Ty.of_def |> Ty.is_empty |> not
    then Wrong
  else
  if Descr.get_others t_descr then Other
  else failwith"empty type"

let mk t = 
  let _,w = mk_mem [] t in w
 
 let pp fmt s = match s with
  Int i -> Z.pp_print fmt i
  |Enum s -> Format.pp_print_string fmt "\"";Printer.print_ty' fmt (singl_to_Ty (Enum s)); Format.pp_print_string fmt "\""
  |Tuple t -> Format.pp_print_string fmt "(";Printer.print_ty' fmt (singl_to_Ty (Tuple t )); Format.pp_print_string fmt ")"
  |Tag ta -> Printer.print_ty' fmt (singl_to_Ty (Tag ta))
  |Arrow a -> Format.pp_print_string fmt "fun :< "; Printer.print_ty' fmt (Op.Arrows.dom a); Format.pp_print_string fmt " -> "; Printer.print_ty' fmt (Op.Arrows.apply a (Op.Arrows.dom a)); Format.pp_print_string fmt " >"
  |Other -> Format.pp_print_string fmt "Other"
  |Wrong -> Format.pp_print_string fmt "NOT IMPLEMENTED YET"
  |Empty -> Format.pp_print_string fmt "Empty type"

  
  let rec equal t1 t2 =
    match (t1,t2) with 
    Int t1, Int t2 -> Z.equal t1 t2
    |Enum e1, Enum e2 -> Enums.equal (Enums.mk e1) (Enums.mk e2)
    |Tuple t1, Tuple t2 -> begin
      match t1, t2 with 
      a1::l1, a2::l2 -> equal a1 a2 && equal (Tuple l1)(Tuple l2) 
      |_, _ -> true 
    end
    |Tag (ta1, ty1), Tag (ta2,ty2) -> Tag.equal ta1 ta2 && equal ty1 ty2
    |Arrow t1, Arrow t2 -> Arrows.equal t1 t2
    |_ -> false

  let is_in singl t =
    Ty.leq (singl_to_Ty singl) t

