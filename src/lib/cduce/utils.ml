
let identity x = x
let filter_options x = List.filter_map identity x

let rec split3 lst =
  match lst with
  | [] -> ([],[],[])
  | (a,b,c)::lst ->
    let (ar,br,cr) = split3 lst in
    (a::ar,b::br,c::cr)
let rec split4 lst =
  match lst with
  | [] -> ([],[],[],[])
  | (a,b,c,d)::lst ->
    let (ar,br,cr,dr) = split4 lst in
    (a::ar,b::br,c::cr,d::dr)

let rec regroup_equiv equiv lst =
  let extract_eq elt = List.partition (equiv elt) in
  match lst with
  | [] -> []
  | e::lst ->
    let (es, lst) = extract_eq e lst in
    (e::es)::(regroup_equiv equiv lst)

let remove_duplicates equiv lst =
  regroup_equiv equiv lst |> List.map List.hd

let remove_greater leq elt = List.filter (fun e -> leq elt e |> not)
let keep_only_minimal leq lst =
  let rec aux explored lst =
    match lst with
    | [] -> List.rev explored
    | e::lst ->
      let explored = remove_greater leq e explored in
      let lst = remove_greater leq e lst in
      aux (e::explored) lst
  in aux [] lst

let pp_long_list pp_elt fmt lst =
  Format.fprintf fmt "[@,@[<v 1>" ;
  List.iter (fun elt -> Format.fprintf fmt " %a ;@ " pp_elt elt) lst ;
  Format.fprintf fmt "@]]"

let pp_list pp_elt fmt lst =
  Format.fprintf fmt "[ " ;
  List.iter (fun elt -> Format.fprintf fmt "%a ; " pp_elt elt) lst ;
  Format.fprintf fmt "]"

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let trd3 (_,_,c) = c

let pairs s1 s2 =
  let rec aux s1 s2 =
    match s1 with
    | [] -> []
    | a1::s1 ->
      let pairs = aux s1 s2 in
      (List.map (fun a2 -> (a1,a2)) s2) @ pairs
  in
  aux s1 s2

let add_others lst =
  let rec aux treated lst =
    match lst with
    | [] -> []
    | a::lst ->
      let others = treated@lst in
      (a,others)::(aux (treated@[a]) lst)
  in
  aux [] lst

let find_among_others pred lst =
  lst |> add_others |> List.find_opt (fun (a,o) -> pred a o)

let find_map_among_others f lst =
  lst |> add_others |> List.find_map (fun (a,o) -> f a o)  

(* let filter_among_others pred lst =
  lst |> add_others |> List.filter (fun (a,o) -> pred a o) |> List.map fst *)

let fold_acc_rem f lst =
  let rec aux acc rem =
    match rem with
    | [] -> acc
    | c::rem -> aux (f c acc rem) rem
  in
  aux [] (List.rev lst)

let filter_among_others pred =
  fold_acc_rem (fun c acc rem -> if pred c (acc@rem) then c::acc else acc)

let map_among_others f =
  fold_acc_rem (fun c acc rem -> (f c (acc@rem))::acc)

let map_among_others' f lst =
  let rec aux acc left right =
    match left with
    | [] -> acc
    | c::left -> aux ((f c (left@right))::acc) left (c::right)
  in
  aux [] (List.rev lst) []

let merge_when_possible merge_opt lst =
  let merge_opt a b others =
    Option.map (fun a -> (a, others)) (merge_opt a b)
  in
  let rec aux lst =
    match lst with
    | [] -> []
    | e::lst ->
      begin match find_map_among_others (fun e' lst -> merge_opt e e' lst) lst with
      | None -> e::(aux lst)
      | Some (e, lst) -> aux (e::lst)
      end
    in
    aux lst

let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h::t ->
    (x::lst) :: (List.map (fun el -> h::el) (insert x t))

let rec perm lst =
  match lst with
  | [] -> [[]]
  | h::t ->
    List.flatten (List.map (insert h) (perm t))

let carthesian_product l1 l2 =
  l1 |> List.map (fun e1 ->
    l2 |> List.map (fun e2 ->
      (e1, e2)
    )
  ) |> List.flatten

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []
