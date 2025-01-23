open Sigs

module Interval = struct
  type t = Z.t option * Z.t option
  let mk lb ub = 
    if Z.leq lb ub then Some lb, Some ub
    else raise (Invalid_argument "Lower bound is greater than upper bound")
  let mk' lb ub =
    match lb, ub with
    | None, None -> None, None
    | Some lb, None -> Some lb, None
    | None, Some ub -> None, Some ub
    | Some lb, Some ub -> mk lb ub
  let mk_singl i = mk i i
  let any = None, None
  let get t = t

  let cmp_lb o1 o2 =
    match o1, o2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some lb1, Some lb2 -> Z.compare lb1 lb2
  let cmp_ub o1 o2 =
    match o1, o2 with
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some lb1, Some lb2 -> Z.compare lb1 lb2
  let compare (lb1, ub1) (lb2,ub2) =
    cmp_lb lb1 lb2 |> Utils.ccmp
    cmp_ub ub1 ub2
  let equal t1 t2 = compare t1 t2 = 0

  let min_lb o1 o2 =
    match o1, o2 with
    | None, _ | _, None -> None
    | Some lb1, Some lb2 when Z.compare lb1 lb2 <= 0 -> Some lb1
    | Some _, Some lb2 -> Some lb2
  let max_ub o1 o2 =
    match o1, o2 with
    | None, _ | _, None -> None
    | Some lb1, Some lb2 when Z.compare lb1 lb2 <= 0 -> Some lb2
    | Some lb1, Some _ -> Some lb1
  let max_lb o1 o2 =
    match o1, o2 with
    | None, o | o, None -> o
    | Some lb1, Some lb2 when Z.compare lb1 lb2 <= 0 -> Some lb2
    | Some lb1, Some _ -> Some lb1
  let min_ub o1 o2 =
    match o1, o2 with
    | None, o | o, None -> o
    | Some lb1, Some lb2 when Z.compare lb1 lb2 <= 0 -> Some lb1
    | Some _, Some lb2 -> Some lb2
  let inter (lb1,ub1) (lb2,ub2) =
    try Some (mk' (max_lb lb1 lb2) (min_ub ub1 ub2))
    with Invalid_argument _ -> None
  let combine (lb1, ub1) (lb2, ub2) =
    let inter_lb = max_lb lb1 lb2 in
    let inter_ub = min_ub ub1 ub2 in
    let neighbor =
      match inter_lb, inter_ub with
      | None, _ | _, None -> true
      | Some lb, Some ub -> Z.compare lb (Z.succ ub) <= 0
    in
    if neighbor then
      Some (min_lb lb1 lb2, max_ub ub1 ub2)
    else
      None
  let pp fmt (o1,o2) =
    match o1, o2 with
    | None, None -> Format.fprintf fmt "(..)"
    | Some i1, None -> Format.fprintf fmt "(%a..)" Z.pp_print i1
    | None, Some i2 -> Format.fprintf fmt "(..%a)" Z.pp_print i2
    | Some i1, Some i2 -> Format.fprintf fmt "(%a..%a)" Z.pp_print i1 Z.pp_print i2
end

module Make(N:Node) = struct
  module Atom = Interval
  type node = N.t
  module ISet = Set.Make(Interval)
  type t = ISet.t

  let empty () = ISet.empty
  let any () = ISet.singleton Interval.any
  let mk i = ISet.singleton i
  let get t = ISet.elements t

  let normalize t =
    let rec try_combine acc lst =
      match acc, lst with
      | acc, [] -> acc
      | [], cur::lst -> try_combine [cur] lst
      | last::acc, cur::lst ->
        begin match Interval.combine last cur with
        | None -> try_combine (cur::last::acc) lst
        | Some i -> try_combine (i::acc) lst
        end
    in
    t |> ISet.elements |> try_combine [] |> ISet.of_list
  let of_list lst = lst |> ISet.of_list |> normalize
  let mk' = of_list

  let neg t =
    let exception Empty in
    let ub next =
      match next with
      | [] -> None
      | (Some lb,_)::_ -> Some (Z.pred lb)
      | (None, _)::_ -> raise Empty
    in
    let lb prev =
      match prev with
      | [] -> None
      | (_,Some ub)::_ -> Some (Z.succ ub)
      | (_, None)::_ -> raise Empty
    in
    let rec aux prev next =
      let cur =
        try [lb prev, ub next] with Empty -> []
      in
      match next with
      | [] -> cur
      | hd::next -> cur@(aux (hd::prev) next)
    in
    aux [] (ISet.elements t) |> ISet.of_list
  let cup t1 t2 = ISet.union t1 t2 |> normalize
  let cap t1 t2 =
    Utils.carthesian_product (ISet.elements t1) (ISet.elements t2)
    |> List.filter_map (fun (i1, i2) -> Interval.inter i1 i2) |> of_list
  let diff t1 t2 = cap t1 (neg t2)

  let is_empty = ISet.is_empty

  let direct_nodes _ = []
  let map_nodes _ t = t
  let simplify t = t

  let compare = ISet.compare
  let equal = ISet.equal
end



