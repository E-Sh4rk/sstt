
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
  aux [] lst

let filter_among_others pred lst =
  lst |> fold_acc_rem (fun c acc rem ->
    if pred c (List.rev_append acc rem) then c::acc else acc)
  |> List.rev

let map_among_others f lst =
  lst |> fold_acc_rem (fun c acc rem ->
    (f c (List.rev_append acc rem))::acc)
  |> List.rev

let merge_when_possible merge_opt lst =
  let rec find_map_in_tail acc e l =
    match l with
    | [] -> None
    | e' :: l -> match merge_opt e e' with
        None -> find_map_in_tail (e' :: acc) e l
      | Some a -> Some (a :: List.rev_append acc l)
  in
  let rec aux lst =
    match lst with
      [] -> []
    | e :: lst -> match find_map_in_tail [] e lst with
        None -> e :: aux lst
      | Some l -> aux l
  in aux lst

(* Base case of set-theoretic operations. *)
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
