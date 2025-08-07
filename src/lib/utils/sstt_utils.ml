
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

let take_one lst =
  let[@tail_mod_cons] rec loop acc = function
      [] -> []
    | e :: lst -> (e, List.rev_append acc lst)::loop(e::acc) lst
  in loop [] lst

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


let rec map_split f l =
  match l with
    [] -> [], []
  | e :: ll -> let a, b = f e in
    let lla, llb = map_split f ll in
    a::lla, b :: llb

let mapn default f lst =
  let rec aux f lst =
    match lst with
    | []::_ -> []
    | _ ->
      let hds, tls = map_split (function (e::l) -> e, l | _ -> assert false) lst in
      (f hds)::(aux f tls)
  in
  if lst = [] then default () else aux f lst

let subsets lst =
  let rec aux acc s1 s2 lst =
    match lst with
    | [] -> (List.rev s1, List.rev s2)::acc
    | e::lst ->
      let acc' = aux acc s1 (e::s2) lst in
      aux acc' (e::s1) s2 lst
  in
  aux [] [] [] lst

let map_among_others' f lst =
  let rec aux left right =
    match right with
      [] -> []
    | c :: right -> (f left c right)::(aux (c::left) right)
  in
  aux [] lst

let partitions n lst =
  let rec aux part lst =
    match lst with
    | [] -> [List.map List.rev part]
    | e::lst ->
      let parts = part |> map_among_others' (fun left s right -> List.rev_append left ((e::s) :: right)) in
      parts |> List.concat_map (fun part -> aux part lst)
  in
  aux (List.init n (fun _ -> [])) lst

(*
  fold_distribute_comb f comb acc [x1;x2;...;xn] [y1;y2;...;yn]
  computes
  let acc = f acc [comb x1 y1; x2; ...; xn] in
  let acc = f acc [x1; comb x2 y2; ...; xn] in
  ...
  let acc = f [x1;x2; ....; comb xn yn] in
  acc

*)

let fold_distribute_comb f comb accv ss tt  =
  let rec loop accl ss tt accv =
    match ss, tt with
    | [], [] -> accv
    | s::ss, t::tt ->
      let line = List.rev_append accl ((comb s t)::ss) in
      let accv' = f accv line in
      loop (s::accl) ss tt accv'
    | _ -> failwith "forall_distribute_comb: invalid list length"
  in
  loop [] ss tt accv
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

let find_map_among_others f lst =
  let rec loop acc = function
    | [] -> None
    | e :: l -> 
      match f e (List.rev_append acc l) with
        None ->  loop (e::acc) l
      | o -> o
  in loop [] lst

let find_among_others pred lst =
  find_map_among_others (fun e l  -> if pred e l then Some (e, l) else None) lst

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

let[@inline always] fcup ~empty ~any ~cup t1 t2 =
  if t1 == any || t1 == t2 || t2 == empty then t1
  else if t2 == any || t1 == empty then t2
  else cup t1 t2

let[@inline always] fcap ~empty ~any ~cap t1 t2 =
  if t1 == empty || t1 == t2 || t2 == any then t1
  else if t2 == empty || t1 == any then t2
  else cap t1 t2

let[@inline always] fneg ~empty ~any ~neg t =
  if t == empty then any
  else if t == any then empty
  else neg t

let[@inline always] fdiff ~empty ~any ~diff t1 t2 =
  if t1 == empty || t1 == t2 || t2 == any then empty
  else if t2 == empty then t1
  else diff t1 t2

let[@inline always] fdiff_neg ~empty ~any ~neg ~diff t1 t2 =
  if t1 == empty || t1 == t2 || t2 == any then empty
  else if t2 == empty then t1
  else if t1 == any then neg t2
  else diff t1 t2
