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

let print_set f fmt s =
  Format.fprintf fmt "{ %a }" (print_seq f " ; ") s

let print_corr f1 f2 fmt (a,b) =
  Format.fprintf fmt "%a=%a" f1 a f2 b
let print_map f1 f2 fmt s =
  Format.fprintf fmt "%a" (print_set (print_corr f1 f2)) s

let identity x = x

let ccmp f e1 e2 r =
  match r with
  | 0 -> f e1 e2
  | n -> n

let unwrap_or default = function
| None -> default
| Some x -> x

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
