
(* PRINT *)

let print_seq f sep fmt l =
  let fst = ref true in
  l |> List.iter
    (fun e -> Format.fprintf fmt "%s%a" (if !fst then "" else sep) f e ; fst := false)

let print_seq_cut f fmt l =
  let fst = ref true in
  l |> List.iter (fun e ->
      if !fst then
        (Format.fprintf fmt "%a" f e ; fst := false)
      else
        Format.fprintf fmt "@,%a" f e
    )

let print_seq_space f fmt l =
  let fst = ref true in
  l |> List.iter (fun e ->
      if !fst then
        (Format.fprintf fmt "%a" f e ; fst := false)
      else
        Format.fprintf fmt "@ %a" f e
    )

(* MISC *)

let identity x = x

let ccmp f e1 e2 r =
  match r with
  | 0 -> f e1 e2
  | n -> n

let unwrap_or default = function
| None -> default
| Some x -> x

(* LISTS *)

let rec take_one lst =
  match lst with
  | [] -> []
  | e::lst ->
      (e, lst)::(List.map (fun (e',lst) -> (e',e::lst)) (take_one lst))

let carthesian_product l1 l2 =
  l1 |> List.map (fun e1 ->
    l2 |> List.map (fun e2 ->
      (e1, e2)
    )
  ) |> List.flatten

let mapn default f lst =
  let rec aux f lst =
    match List.hd lst with
    | [] -> []
    | _::_ ->
      let hds = List.map List.hd lst in
      let tls = List.map List.tl lst in
      (f hds)::(aux f tls)
  in
  if lst = [] then default () else aux f lst

let subsets lst =
  let rec aux s1 s2 lst =
    match lst with
    | [] -> [List.rev s1, List.rev s2]
    | e::lst ->
      let res1 = aux (e::s1) s2 lst in
      let res2 = aux s1 (e::s2) lst in
      res1@res2
  in
  aux [] [] lst

let map_among_others' f lst =
  let rec aux acc left right =
    match right with
    | [] -> List.rev acc
    | c::right -> aux ((f left c right)::acc) (left@[c]) right
  in
  aux [] [] lst

let partitions n lst =
  let rec aux part lst =
    match lst with
    | [] -> [List.map List.rev part]
    | e::lst ->
      let parts = part |> map_among_others' (fun left s right -> left@[e::s]@right) in
      parts |> List.map (fun part -> aux part lst) |> List.concat
  in
  aux (List.init n (fun _ -> [])) lst

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

let merge_when_possible merge_opt lst =
  let merge_opt a b others =
    merge_opt a b |> Option.map (fun a -> (a, others))
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

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []
