open Core


type t = Int of Z.t
       | Enum of Enums.Atom.t
       | Arrow of Arrows.t
       | Tag of (Tag.t * t)
       | Tuple of t list
       | Record of Records.Atom.t
       | Other


module VDHash = Hashtbl.Make(Descr)
let hash = VDHash.create 16

let rec to_ty s =
  match s with 
    Int i -> Intervals.Atom.mk_singl i |> Descr.mk_interval |> Ty.mk_descr
  | Enum e -> Descr.mk_enum e|> Ty.mk_descr
  | Arrow a -> Descr.mk_arrows a |> Ty.mk_descr
  | Tag (ta,ty) -> (ta, to_ty ty) |> Descr.mk_tag |> Ty.mk_descr
  | Tuple tu -> List.map to_ty tu |> Descr.mk_tuple |> Ty.mk_descr
  | Record r -> Descr.mk_record r |> Ty.mk_descr
  | Other -> Descr.mk_others true |> Ty.mk_descr



let pp fmt s = match s with
    Int i -> Z.pp_print fmt i
  | Enum s -> Format.fprintf fmt "\" %a \"" Printer.print_ty' (to_ty (Enum s))
  | Tuple t -> Format.fprintf fmt "( %a )" Printer.print_ty' (to_ty (Tuple t ))
  | Tag ta -> Printer.print_ty' fmt (to_ty (Tag ta))
  | Arrow a -> Format.fprintf fmt "fun :< %a -> %a >" Printer.print_ty'  (Op.Arrows.dom a) Printer.print_ty'  (Op.Arrows.apply a (Op.Arrows.dom a))
  | Record r -> Printer.print_ty' fmt (to_ty (Record r))
  | Other -> Format.pp_print_string fmt "Other"



let compare w1 w2 = 
  match (w1,w2) with 
    Int i1,Int i2 -> Z.compare i1 i2
  | Int _, _ -> -1
  | _, Int _ -> 1
  | Enum e1, Enum e2 -> Enums.Atom.compare e1 e2
  | Enum _, _ -> -1
  | _, Enum _ -> 1
  | Arrow a1, Arrow a2 -> Arrows.compare a1 a2
  |Arrow _, _ -> -1
  | _ , Arrow _ -> 1
  | Tag (ta1, t1), Tag (ta2,t2) -> let a = Tag.compare ta1 ta2 in if (a==0) then compare t1 t2 else a
  | Tag _, _ -> -1
  | _ , Tag _ -> 1
  | Tuple tu1, Tuple tu2 -> begin
      match tu1,tu2 with
      | a1::l1, a2::l2 -> let res = compare a1 a2 in if (res==0) then compare l1 l2 else res
      |[], _::_ -> -1
      |_ -> 1
    end
  | Tuple _ , _ -> -1
  |_, Tuple _ -> 1
  | Record _ , Record _  -> failwith "Not implemented"
  | Record _, _ -> -1
  | _ , Record _  -> 1
  | _ -> 0


let rec equal w1 w2 =
  match (w1,w2) with 
    Int t1, Int t2 -> Z.equal t1 t2
  | Enum e1, Enum e2 -> Enum.equal e1 e2
  | Tuple t1, Tuple t2 -> begin
      try List.for_all2 equal t1 t2 with _ -> false
    end
  | Tag (ta1, ty1), Tag (ta2,ty2) -> Tag.equal ta1 ta2 && equal ty1 ty2
  | Arrow t1, Arrow t2 -> Arrows.equal t1 t2
  | _ -> false

let is_in singl t =
  Ty.leq (to_ty singl) t


let mk_intervals t = 
  let t_interval = Ty.get_descr t |> Descr.get_intervals |> Intervals.destruct |> List.hd |> Intervals.Atom.get in 
  match t_interval with 
  | (None, None) ->  Int (Z.of_int 42)
  | (Some z1, _) | (None, Some z1) -> Int z1

let mk_enums t = 
  let t_enum = Ty.get_descr t |> Descr.get_enums |> Enums.destruct in 
  match t_enum with
  | (true, lt_enum) -> Enum (List.hd lt_enum)
  | (false, lt_enum) -> Enum (Enums.Atom.mk (String.make ((List.fold_left (fun a b -> max a (String.length (Enums.Atom.name b))) 0 lt_enum )+1) 'a'))



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


let mk_tag_atom make tag a = 
  let w = make  (let _,b = List.hd (Op.TagComp.as_union(TagComp.of_dnf tag [a] )) in b)in 
  match w with 
  | None -> None 
  | Some w -> Some (Tag(tag,w))


let rec mk_tag_atoms_list_atoms_list_list make tag pn_list =
  match pn_list with 
    a :: l -> begin
      let w = mk_tag_atom make tag a in 
      match w with 
      |None -> mk_tag_atoms_list_atoms_list_list make tag l 
      |Some _ -> w

    end
  | [] -> None


let rec mk_tagcomp_list make t lt_tag =
  match lt_tag with 
    a::l -> 
    begin
      let pn_list = TagComp.dnf a in 
      let w = mk_tag_atoms_list_atoms_list_list make (TagComp.tag a) pn_list in
      match w with 
      | None -> mk_tagcomp_list make t l
      | Some _ -> w
    end
  |[] -> None

let rec len_false_tag len l = 
  if List.for_all (fun a -> (a |> TagComp.tag |> Tag.name |> String.length) != len) l 
  then len 
  else len_false_tag (len+1) l
let mk_tag make t = 
  let t_tag = Ty.get_descr t |> Descr.get_tags |> Tags.destruct in 
  match t_tag with 
  | (true,co_l) -> mk_tagcomp_list make t co_l
  | (false, co_l) -> 
    let new_tag = 
      Tag.mk (String.make (len_false_tag 1 co_l) 'a') in 
    let new_type = Int(Z.of_int 16)  in  Some (Tag(new_tag,new_type))


let mk_tuple_atoms make atom = 
  let w = List.map (fun a -> make a ) atom in 
  if List.mem (let x = None in x) w then None
  else Some(Tuple(List.map (fun a -> match a with  |None -> failwith "impossible" |Some w -> w) w))

let rec mk_tuplecomp_atom_list make atom_list =

  match atom_list with 
    a::l -> begin let w = mk_tuple_atoms make a in match w with 
    | Some _ -> w
    | None -> mk_tuplecomp_atom_list make l
    end
  | []-> None


let rec mk_tuplecomp_list make lt_tuple = 
  match lt_tuple with 
    a :: l -> begin 
      let atom_list = Op.TupleComp.as_union a in 
      let w = mk_tuplecomp_atom_list make atom_list in
      match w with 
      | Some _ -> w
      | None -> mk_tuplecomp_list make l 
    end
  |[]-> None

let rec len_false_tuple len l = 
  if List.for_all (fun a -> TupleComp.len a!= len) l 
  then len 
  else len_false_tuple (len+1) l
let mk_tuples_mem mk_mem t = 
  let t_tuple = Ty.get_descr t |> Descr.get_tuples |> Tuples.destruct in 
  match t_tuple with 
  | (true, lt_tuple) -> mk_tuplecomp_list mk_mem  lt_tuple
  | (false, lt_tuple) -> Some (Tuple(List.init (len_false_tuple 1 lt_tuple) (fun x -> Int(Z.of_int x))))


let mk_records_atom' (make : (Ty.t -> t option)) atom =
  let doms =  Records.Atom'.dom atom in 
  let bindings = LabelSet.to_list doms |> List.map (fun dom -> (dom,Records.Atom'.find dom atom |> Ty.F.get_descr)) in 
  let w = List.map (fun (a,b) -> if Ty.F.OTy.is_optional b then (a,None) else (a, Some(make (Ty.F.OTy.get b)))) bindings in 
  if List.for_all (fun (_,b) -> match b with |Some(None) -> false |_ -> true) w then 
    let w = List.fold_left (fun map (a,b) -> match b with |Some(Some(wit)) -> LabelMap.add a (Ty.F.mk_descr (to_ty wit,false)) map |_ -> map ) LabelMap.empty w in 
  if LabelMap.bindings w |> List.is_empty then 
    Some(Record{bindings = w; tail = Ty.F.any}) else Some(Record{bindings = w; tail = Ty.F.empty})
  else None

let rec mk_records_list make l = 
  match l with 
  |a::l -> begin let w = mk_records_atom' make a in 
      match w with 
      |Some _ -> w
      |None -> mk_records_list make l
    end
  |[] -> None

let mk_records_mem make t = 
  let record_list = Ty.get_descr t |> Descr.get_records |> Records.dnf' in 
  mk_records_list make record_list





let rec mk_mem t = 
  let t_descr = Ty.get_descr t in 
  let w = VDHash.find_opt hash t_descr in
  match w with 
  | Some(None) -> None
  | Some a -> a
  | None ->
    VDHash.add hash t_descr None;
    if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then let w = Some (mk_intervals t) in VDHash.replace hash t_descr w; w
    else 
    if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then let w = Some (mk_enums t) in VDHash.replace hash t_descr w; w
    else 
    if Descr.get_arrows t_descr |> Descr.mk_arrows |> Ty.mk_descr |> Ty.is_empty |> not
    then let w = Some (mk_arrows t) in VDHash.replace hash t_descr w; w
    else 
    if Descr.get_tags t_descr|> Descr.mk_tags |> Ty.mk_descr |> Ty.is_empty |> not 
    then let w = mk_tag mk_mem t in VDHash.replace hash t_descr w; w
    else
    if Descr.get_tuples t_descr|> Descr.mk_tuples |> Ty.mk_descr |> Ty.is_empty |> not
    then let w= mk_tuples_mem mk_mem t in VDHash.replace hash t_descr w; w
    else
    if Descr.get_records t_descr |> Descr.mk_records |> VDescr.mk_descr |>Ty.of_def |> Ty.is_empty |> not
    then let w = mk_records_mem mk_mem t in VDHash.replace hash t_descr w;w
    else
    if Descr.get_others t_descr then Some(Other)
    else None

let mk t = 
  VDHash.reset hash;
  match mk_mem t with 
  | Some w -> w
  | None -> failwith "Empty Type"