open Core

type t = Int of Z.t
       | Enum of Enums.Atom.t
       | Arrow of Arrows.t
       | Tag of (Tag.t * t)
       | Tuple of t list
       | Record of (Label.t * t) list * (t option)
       | Other


module VDHash = Hashtbl.Make(Descr)
let mem = VDHash.create 16


let rec to_ty s =
  let open Ty in 
  let open Descr in 
  match s with 
    Int i -> Intervals.Atom.mk_singl i |> mk_interval |> mk_descr
  | Enum e -> mk_enum e |> mk_descr
  | Arrow a -> mk_arrows a |> mk_descr
  | Tag (ta,ty) -> (ta, to_ty ty) |> mk_tag |> mk_descr
  | Tuple tu -> List.map to_ty tu |> mk_tuple |> mk_descr
  | Record (binding,tail) -> 
    let binding = List.map 
        (fun(a,b) -> (a, to_ty b |> O.required |> F.mk_descr))  
        binding  in
    let tail = match tail with 
      | Some t -> to_ty t |> F.OTy.required |> F.mk_descr 
      | None -> F.OTy.absent |> F.mk_descr in
    Row.mk binding tail |> Row.to_record_atom |> mk_record |> mk_descr
  | Other -> mk_others true |> mk_descr

let create_record_tail tail = let open Ty.F in 
  (match tail with 
   | Some t -> to_ty t |> OTy.required |> mk_descr 
   | None -> OTy.absent |> mk_descr)
let pp fmt s =
  let open Format in 
  let open Printer in 
  match s with
    Int i -> Z.pp_print fmt i
  | Enum s -> fprintf fmt "\" %a \"" print_ty' (to_ty (Enum s))
  | Tuple t -> fprintf fmt "( %a )" print_ty' (to_ty (Tuple t))
  | Tag ta -> print_ty' fmt (to_ty (Tag ta))
  | Arrow a -> fprintf fmt "fun :< %a -> %a >" 
                 print_ty' (Op.Arrows.dom a) 
                 print_ty' (Op.Arrows.apply a (Op.Arrows.dom a))
  | Record (b,t) -> print_ty' fmt (to_ty (Record(b,t)))
  | Other -> pp_print_string fmt "Other"

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
  | Tag (ta1, t1), Tag (ta2,t2) -> 
    let res = Tag.compare ta1 ta2 in 
    if (res == 0) then compare t1 t2 else res
  | Tag _, _ -> -1
  | _, Tag _ -> 1
  | Tuple tu1, Tuple tu2 -> begin
      match tu1, tu2 with
      | a1 :: l1, a2 :: l2 -> 
        let res = compare a1 a2 in 
        if (res == 0) then compare l1 l2 else res
      |[], _ :: _ -> -1
      |_ :: _, [] -> 1
      |[], [] -> 0
    end
  | Tuple _ , _ -> -1
  |_, Tuple _ -> 1
  | Record (b1, t1), Record (b2, t2)  -> begin 
      match b1, b2 with 
      | (a1, b1) :: l1, (a2, b2)::l2 -> 
        let res = Label.compare a1 a2 in 
        if (res == 0) then
          let ty = compare b1 b2 in 
          if (ty == 0) then 
            compare l1 l2 
          else ty 
        else res
      |[], _ :: _ -> -1
      |_ :: _ , [] -> 1
      |[], [] -> 
        match t1,t2 with 
        |None, None -> 0
        |None, Some _ -> -1
        |Some _, None -> 1
        |Some a, Some b -> compare a b 
    end
  | Record _, _ -> -1
  | _ , Record _  -> 1
  | _ -> 0

let rec equal w1 w2 =
  match (w1,w2) with 
    Int t1, Int t2 -> Z.equal t1 t2
  | Enum e1, Enum e2 -> Enum.equal e1 e2
  | Arrow t1, Arrow t2 -> Arrows.equal t1 t2
  | Tag (ta1, ty1), Tag (ta2,ty2) -> Tag.equal ta1 ta2 && equal ty1 ty2
  | Tuple t1, Tuple t2 -> begin
      try List.for_all2 equal t1 t2 with _ -> false
    end
  | Record (b1,t1), Record (b2,t2) -> 
    let t1 = Row.mk
        (List.map 
           (fun(a,b) -> (a, to_ty b |> Ty.O.required |> Ty.F.mk_descr)) 
           b1 )
        (create_record_tail t1)
      |> Row.to_record_atom |> Records.mk in
    let t2 = Row.mk
        (List.map 
           (fun(a,b) -> (a, to_ty b |> Ty.O.required |> Ty.F.mk_descr)) 
           b2 )
        (create_record_tail t2) 
      |> Row.to_record_atom |> Records.mk in 
    Records.equal t1 t2
  |Other, Other -> true
  |_ -> false 

let is_in singl t =
  Ty.leq (to_ty singl) t

let mk_intervals t = 
  let atom_list = Ty.get_descr t |> Descr.get_intervals |> Intervals.destruct in
  let atom = List.hd atom_list |> Intervals.Atom.get in 
  match atom with 
  | (None, None) ->  Int (Z.of_int 42)
  | (Some z1, _) | (None, Some z1) -> Int z1

let mk_enums t = 
  let atom_list = Ty.get_descr t |> Descr.get_enums |> Enums.destruct in 
  match atom_list with
  | (true, lt_enum) -> Enum (List.hd lt_enum)
  | (false, lt_enum) -> 
    let false_enum = (String.make 
                        ((List.fold_left 
                            (fun a b -> max a (String.length (Enums.Atom.name b))) 
                            0 lt_enum ) + 1) 
                        'a') in 
    Enum (Enums.Atom.mk false_enum)

let mk_arrows t = 
  let a1,a2 = Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd in
  let rec help_arrow t a1 a2 a3 = 
    let test_arrow = Arrows.of_dnf [(a1,a2)] |> Descr.mk_arrows |> Ty.mk_descr in
    if Ty.leq test_arrow t then Arrows.of_dnf [a1,a2] 
    else
      begin 
        match a3 with 
          a:: l -> help_arrow t a1 (a :: a2) l
        |_ -> 
          let atom_list = [Ty.get_descr t |> Descr.get_arrows |> Arrows.dnf |> List.hd] in
          Arrows.of_dnf atom_list
      end in
  Arrow(help_arrow t a1 [] a2)


let mk_tag_atom make tag atom = 
  let _,ty = [atom] |> TagComp.of_dnf tag |> Op.TagComp.as_union |> List.hd in
  let w = make ty in 
  match w with 
  | None -> None 
  | Some w -> Some (Tag (tag,w))


let rec mk_tag_pn_list make tag pn_list =
  match pn_list with 
  | a :: l -> begin
      let w = mk_tag_atom make tag a in 
      match w with 
      | None -> mk_tag_pn_list make tag l 
      | Some _ -> w
    end
  | [] -> None

let rec mk_tagcomp_list make t lt_tag =
  match lt_tag with 
  | a :: l -> 
    begin
      let pn_list = TagComp.dnf a in 
      let w = mk_tag_pn_list make (TagComp.tag a) pn_list in
      match w with 
      | None -> mk_tagcomp_list make t l
      | Some _ -> w
    end
  | [] -> None

let rec len_false_tag len l = 
  if List.for_all 
      (fun a -> (a |> TagComp.tag |> Tag.name |> String.length) != len) 
      l 
  then len 
  else len_false_tag (len+1) l
let mk_tag make t = 
  let t_tag = Ty.get_descr t |> Descr.get_tags |> Tags.destruct in 
  match t_tag with 
  | (true,co_l) -> mk_tagcomp_list make t co_l
  | (false, co_l) -> 
    let new_tag = Tag.mk (String.make (len_false_tag 1 co_l) 'a') in 
    let new_type = Int(Z.of_int 16) in 
    Some (Tag(new_tag,new_type))

let mk_tuple_atoms make atom = 
  let w = List.map (fun a -> make a ) atom in 
  if List.mem (let x = None in x) w then None
  else Some(Tuple(List.map 
                    (fun a -> match a with  
                       |None -> failwith "Impossible : Empty type undetected in tuples" 
                       |Some w -> w) 
                    w))

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
  | (false, lt_tuple) -> Some (Tuple(List.init 
                                       (len_false_tuple 1 lt_tuple) 
                                       (fun x -> Int(Z.of_int x))))


let mk_record_binding make bindings = 
  List.fold_left
    (fun map (a,b) -> 
       let new_b = b |> Ty.F.get_descr in 
       match new_b with 
       | (_ , true)-> map
       | (ty,false) -> let w_ty = make ty in 
         match  w_ty with 
         | None -> failwith "Empty required field of a record" 
         | Some w -> (a,w)::map)
    [] bindings 

let mk_records_tail make tail = 
  if Ty.F.OTy.is_optional tail 
  then None 
  else make (Ty.F.OTy.get tail)

let mk_record_exists make exists bind_len tail = 
  let create_false_label label_set bind_len =
    let label_list = LabelSet.to_list label_set in 
    let new_len = List.fold_left 
        (fun len lbl -> 
           let lbl_len = lbl |> Label.name |> String.length in 
           max len lbl_len)
        bind_len 
        label_list in 
    String.make (new_len+1) 'a' |> Label.mk
  in
  let refined_tail = Ty.F.OTy.get tail  in 
  let exist_to_witness w_exists (lbl_list,fty) = 
    let oty = Ty.F.get_descr fty in
    if Ty.F.OTy.is_optional oty then w_exists else 
      let ty = Ty.F.OTy.get oty in 
      let w_ty = 
        let w = make (Ty.cap ty refined_tail) in
        match w with 
        |None -> failwith "Impossible : Undetected empty type in records" 
        | Some w -> w 
      in 
      ( create_false_label lbl_list bind_len, w_ty ) :: w_exists in
  List.fold_left exist_to_witness [] exists

let mk_records_atom' make atom =
  let bindings = atom.Records.Atom'.bindings |> LabelMap.to_list in 
  let tail = atom.Records.Atom'.tail |> Ty.F.get_descr in 
  let exists = atom.Records.Atom'.exists in 
  let w_binding = mk_record_binding make bindings in 
  let w_tail = mk_records_tail make tail in 
  let max_len_binding = List.fold_left 
      (fun len (a,_) -> let lbl_len = a |> Label.name |> String.length in max len lbl_len ) 
      0 bindings in 
  let w_exists = mk_record_exists make exists max_len_binding tail in 

  Some(Record(w_binding@w_exists,w_tail))

let rec mk_records_list make l = 
  match l with 
  |a::l -> begin let w = mk_records_atom' make a in 
      match w with 
      |Some _ -> w
      |None -> mk_records_list make l 
    end
  |[] -> None

let mk_records_mem make t = 
  let record = Ty.get_descr t |> Descr.get_records in 
  let record_list = Records.dnf' record in
  mk_records_list make record_list

let rec mk_mem t = 
  let t_descr = Ty.get_descr t in 
  let w = VDHash.find_opt mem t_descr in
  match w with 
  | Some(None) -> None
  | Some a -> a
  | None ->
    VDHash.add mem t_descr None;
    if Descr.get_intervals t_descr |> Intervals.equal Intervals.empty |> not
    then let w = Some (mk_intervals t) in VDHash.replace mem t_descr w; w
    else 
    if Descr.get_enums t_descr |> Enums.equal Enums.empty |> not 
    then let w = Some (mk_enums t) in VDHash.replace mem t_descr w; w
    else 
    if Descr.get_arrows t_descr |> Descr.mk_arrows |> Ty.mk_descr |> Ty.is_empty |> not
    then let w = Some (mk_arrows t) in VDHash.replace mem t_descr w; w
    else 
    if Descr.get_tags t_descr|> Descr.mk_tags |> Ty.mk_descr |> Ty.is_empty |> not 
    then let w = mk_tag mk_mem t in VDHash.replace mem t_descr w; w
    else
    if Descr.get_tuples t_descr|> Descr.mk_tuples |> Ty.mk_descr |> Ty.is_empty |> not
    then let w = mk_tuples_mem mk_mem t in VDHash.replace mem t_descr w; w
    else
    if Descr.get_records t_descr |> Descr.mk_records |> Ty.mk_descr |> Ty.is_empty |> not
    then let w = mk_records_mem mk_mem t in VDHash.replace mem t_descr w;w
    else
    if Descr.get_others t_descr then Some(Other)
    else None

let mk t = 
  VDHash.reset mem;
  match mk_mem t with 
  | Some w -> w
  | None -> failwith "Empty Type"