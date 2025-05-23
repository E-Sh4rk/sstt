open Sigs
open Sstt_utils

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

  type t = { map : C.t TMap.t ; others : bool }

  let mk a =
    let t = C.tag a in
    { map = TMap.singleton t a ; others = false }
  let of_components (ts, others) =
    let map = ts |> List.map (fun a -> (C.tag a, a)) |> TMap.of_list in
    { map ; others }
  let construct (pos, cs) =
    if pos then
      let map = cs |> List.map (fun a -> (C.tag a, a)) |> TMap.of_list in
      { map ; others=false }
    else
      let map = cs |> List.map (fun a -> (C.tag a, C.neg a)) |> TMap.of_list in
      { map ; others=true }

  let any = { map = TMap.empty ; others = true }
  let empty = { map = TMap.empty ; others = false }

  let cap t1 t2 =
    let others = t1.others && t2.others in
    let map = TMap.merge (fun _ o1 o2 ->
        match o1, o2 with
        | None, None -> None
        | Some t1, None -> if t2.others then Some t1 else None
        | None, Some t2 -> if t1.others then Some t2 else None
        | Some t1, Some t2 -> Some (C.cap t1 t2)
      ) t1.map t2.map in
    { map ; others }
  let cup t1 t2 =
    let others = t1.others || t2.others in
    let map = TMap.merge (fun _ o1 o2 ->
        match o1, o2 with
        | None, None -> None
        | Some t1, None -> if t2.others then None else Some t1
        | None, Some t2 -> if t1.others then None else Some t2
        | Some t1, Some t2 -> Some (C.cup t1 t2)
      ) t1.map t2.map in
    { map ; others }
  let neg t =
    let others = not t.others in
    let map = TMap.map C.neg t.map in
    { map ; others }
  let diff t1 t2 =
    let others = t1.others && not t2.others in
    let map = TMap.merge (fun _ o1 o2 ->
        match o1, o2 with
        | None, None -> None
        | Some t1, None -> if not t2.others then Some t1 else None
        | None, Some t2 -> if t1.others then Some (C.neg t2) else None
        | Some t1, Some t2 -> Some (C.diff t1 t2)
      ) t1.map t2.map in
    { map ; others }

  let is_empty t =
    not t.others &&
    TMap.for_all (fun _ a -> C.is_empty a) t.map

  let direct_nodes t = t.map |> TMap.bindings |>
                       List.map (fun (_,t) -> C.direct_nodes t) |>
                       List.concat

  let map_nodes f t =
    let map = TMap.map (C.map_nodes f) t.map in
    { map ; others=t.others }

  let simplify t =
    let t_is_empty t = C.is_empty t in
    let t_is_any t = C.neg t |> C.is_empty in
    let map = TMap.map C.simplify t.map in
    let p = if t.others then t_is_any else t_is_empty in
    let map = TMap.filter (fun _ t -> p t |> not) map in
    { map ; others = t.others }

  let components t =
    let cs = TMap.bindings t.map |> List.map snd in
    (cs, t.others)

  let destruct t =
    if t.others then
      (false, TMap.bindings t.map |> List.map snd |> List.map C.neg)
    else
      (true, TMap.bindings t.map |> List.map snd)

  let get tag t =
    match TMap.find_opt tag t.map with
    | Some a -> a
    | None when t.others -> C.any tag
    | None -> C.empty tag

  let map f t =
    let map = TMap.map f t.map in
    { t with map }

  let equal t1 t2 =
    t1.others = t2.others &&
    TMap.equal C.equal t1.map t2.map
  let compare t1 t2 =
    compare t1.others t2.others |> ccmp
      (TMap.compare C.compare) t1.map t2.map
end
