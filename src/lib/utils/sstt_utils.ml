
let print_seq f sep =
  Format.(pp_print_list  ~pp_sep:(fun fmt () -> pp_print_string fmt sep) f)

let print_seq_cut f =
  Format.(pp_print_list ~pp_sep:pp_print_cut f)

let print_seq_space f =
  Format.(pp_print_list ~pp_sep:pp_print_space f)

(* MISC *)

let[@inline always] ccmp f e1 e2 r =
  if r <> 0 then r else f e1 e2

(* LISTS *)

let rec take_one lst =
  match lst with
  | [] -> []
  | e::lst ->
    (e, lst)::(List.map (fun (e',lst) -> (e',e::lst)) (take_one lst))

let cartesian_product l1 l2 =
  let rec loop l acc =
    match l with
    | [] -> acc
    | e2::ll -> loop_one e2 l1 ll acc
  and loop_one e2 l1 ll acc =
    match l1 with
    | [] -> loop ll acc
    | e1::ll1 -> loop_one e2 ll1 ll ((e1, e2)::acc)
  in
  loop l2 []
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
  let rec aux left right =
    match right with
      [] -> []
    | c :: right -> (f (List.rev left) c right)::(aux (c::left) right)
  in
  aux [] lst

let partitions n lst =
  let rec aux part lst =
    match lst with
    | [] -> [List.map List.rev part]
    | e::lst ->
      let parts = part |> map_among_others' (fun left s right -> left@[e::s]@right) in
      parts |> List.concat_map (fun part -> aux part lst)
  in
  aux (List.init n (fun _ -> [])) lst

(*
  iter_distribute_comb f comb [x1;x2;...;xn] [y1;y2;...;yn]
  computes
  f [comb x1 y1; x2; ...; xn]
  f [x1; comb x2 y2; ...; xn]
  ...
  f [x1;x2; ....; comb xn yn]

*)
let iter_distribute_comb f comb ss tt =
  let rec loop acc ss tt =
    match ss, tt with
    | [], [] -> ()
    | s::ss, t::tt ->
      let line = List.rev_append acc ((comb s t)::ss) in
      f line;
      loop (s::acc) ss tt
    | _ -> failwith "iter_distribute_comb: invalid list length"
  in
  loop [] ss tt

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

let find_opt_of_iter (type a) (f : a -> bool) it col =
  let exception Found of a in
  try it (fun e -> if f e then raise_notrace (Found e)) col; None with
  | Found e -> Some e

let find_opt_of_iter2 (type a b) (f : a -> b -> bool) it col =
  let exception Found of (a*b) in
  try it (fun x y -> if f x y then raise_notrace (Found (x,y))) col; None with
  | Found e -> Some e
