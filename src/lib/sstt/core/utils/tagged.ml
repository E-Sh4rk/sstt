open Sigs
open Utils

module type Tag = sig
  include Comparable
end

module type TaggedComp = sig
  type t
  type node
  module Tag : Tag
  val any : Tag.t -> t
  val empty : Tag.t -> t
  val tag : t -> Tag.t
  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val map_nodes : (node -> node) -> t -> t
  val direct_nodes : t -> node list
  val simplify : t -> t
  val is_empty : t -> bool
  include Comparable with type t := t
end

module Make(C : TaggedComp) = struct
  module TMap = Map.Make(C.Tag)

  type t = { map : (C.Tag.t * C.t) list ; others : bool }

  let mk a =
    let t = C.tag a in
    { map = [t, a] ; others = false }

  let of_comp_list l =
    l
    |> List.map (fun a -> (C.tag a, a))
    |> List.sort_uniq (fun (ta, _) (tb, _) -> C.Tag.compare ta tb)

  let map_on_snd f l = List.map (fun (e, a) -> (e, f a)) l
  let snd_map f l = List.map (fun (_, a) -> f a) l

  let of_components (ts, others) =
    let map = of_comp_list ts in
    { map ; others }
  let construct (pos, cs) =
    if pos then
      let map = of_comp_list cs in
      { map ; others=false }
    else
      let map = of_comp_list cs |> map_on_snd C.neg in
      { map ; others=true }

  let any = { map = [] ; others = true }
  let empty = { map = [] ; others = false }

  let cap t1 t2 =
    let others = t1.others && t2.others in
    let rec loop l1 l2 =
      match l1, l2 with
      | [], _ -> if t1.others then l2 else []
      | _, [] -> if t2.others then l1 else []
      | ((e1, a1) as v1):: ll1, ((e2, a2) as v2) :: ll2 ->
        let c = C.Tag.compare e1 e2 in
        if c < 0 then if t2.others then v1 :: loop ll1 l2 else loop ll1 l2
        else if c > 0 then if t1.others then v2 :: loop l1 ll2 else loop l1 ll2
        else (e1, C.cap a1 a2):: loop ll1 ll2
    in
    { map = loop t1.map t2.map; others }
  let cup t1 t2 =
    let others = t1.others || t2.others in
    let rec loop l1 l2 =
      match l1, l2 with
      | [], _ -> if t1.others then [] else l2
      | _, [] -> if t2.others then [] else l1
      | ((e1, a1) as v1):: ll1, ((e2, a2) as v2) :: ll2 ->
        let c = C.Tag.compare e1 e2 in
        if c < 0 then if t2.others then loop ll1 l2 else v1 :: loop ll1 l2
        else if c > 0 then if t1.others then loop l1 ll2 else v2 :: loop l1 ll2
        else (e1, C.cup a1 a2):: loop ll1 ll2
    in
    { map = loop t1.map t2.map; others }

  let neg t =
    let others = not t.others in
    let map = map_on_snd C.neg t.map in
    { map ; others }
  let diff t1 t2 = cap t1 (neg t2)

  let is_empty t =
    not t.others && List.for_all (fun (_, a) -> C.is_empty a) t.map

  let direct_nodes t =
    List.concat_map (fun (_, t) -> C.direct_nodes t) t.map

  let map_nodes f t =
    let map = map_on_snd (C.map_nodes f) t.map in
    { map ; others=t.others }

  let simplify t =
    let t_is_empty t = C.is_empty t in
    let t_is_any t = C.neg t |> C.is_empty in
    let map = map_on_snd C.simplify t.map in
    let p = if t.others then t_is_any else t_is_empty in
    let map = List.filter (fun (_, t) -> p t |> not) map in
    { map ; others = t.others }

  let components t =
    let cs = List.map snd t.map in
    (cs, t.others)

  let destruct t =
    if t.others then
      (false, snd_map C.neg t.map)
    else
      (true, List.map snd t.map)

  let get tag t =
    match List.find_map (fun (e, a) -> if C.Tag.equal e tag then Some a else None) t.map with
    | Some a -> a
    | None when t.others -> C.any tag
    | None -> C.empty tag

  let map f t =
    let map = map_on_snd f t.map in
    { t with map }

  let equal t1 t2 =
    Bool.equal t1.others t2.others &&
    try
      List.for_all2 (fun (e1, a1) (e2, a2) -> C.Tag.equal e1 e2 && C.equal a1 a2) t1.map t2.map
    with _ -> false
  let compare t1 t2 =
    Bool.compare t1.others t2.others |> ccmp
      (List.compare (fun (e1, a1) (e2, a2) -> C.Tag.compare e1 e2 |> ccmp C.compare a1 a2)) t1.map t2.map
end
