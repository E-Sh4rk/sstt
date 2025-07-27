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

  type t = { map : C.t list; others : bool }
  (* When others is [true], missing tags are mapped to Any
     When others is [false], missing tags are mapped to Empty
  *)

  let mk a =
    { map = [a] ; others = false }

  let of_comp_list =
    List.sort_uniq (fun a b -> C.(Tag.compare (tag a) (tag b)))

  let of_components (ts, others) =
    let map = of_comp_list ts in
    { map ; others }
  let construct (pos, cs) =
    if pos then
      let map = of_comp_list cs in
      { map ; others=false }
    else
      let map = of_comp_list cs |> List.map C.neg in
      { map ; others=true }

  let any = { map = [] ; others = true }
  let empty = { map = [] ; others = false }

  let neg t =
    let others = not t.others in
    let map = List.map C.neg t.map in
    { map ; others }

  let [@inline always] (@?) o l =
    match o with
      None -> l
    | Some e -> e :: l

  let[@inline always] op empty1 empty2 missing1 missing2 comb12 combo t1 t2 =
    let rec loop l1 l2 =
      match l1, l2 with
      | [], _ -> empty1 l2
      | _, [] -> empty2 l1
      | a1 :: ll1, a2 :: ll2 ->
        let e1 = C.tag a1 in
        let e2 = C.tag a2 in
        let c = C.Tag.compare e1 e2 in
        if c < 0 then (missing2 a1) @? loop ll1 l2
        else if c > 0 then (missing1 a2) @? loop l1 ll2
        else (comb12 a1 a2)::loop ll1 ll2
    in
    let others = combo t1.others t2.others in
    { map = loop t1.map t2.map; others }

  let cst_nil _ = []
  let cst_none _ = None
  let cap t1 t2 =
    op 
      (if t1.others then Fun.id else cst_nil)
      (if t2.others then Fun.id else cst_nil)
      (if t1.others then Option.some else cst_none)
      (if t2.others then Option.some else cst_none)
      C.cap
      (&&) t1 t2
  let cup t1 t2 =
    op 
      (if t1.others then cst_nil else Fun.id)
      (if t2.others then cst_nil else Fun.id)
      (if t1.others then cst_none else Option.some)
      (if t2.others then cst_none else Option.some)
      C.cup
      (||) t1 t2
  let diff t1 t2 =
    op 
      (if t1.others then List.map C.neg else cst_nil)
      (if not t2.others then Fun.id else cst_nil)
      (if t1.others then (fun x -> Some (C.neg x)) else cst_none)
      (if not t2.others then Option.some else cst_none)
      C.diff
      (fun b1 b2 -> b1 && not b2) t1 t2
  let is_empty t =
    not t.others && List.for_all C.is_empty t.map

  let direct_nodes t =
    List.concat_map C.direct_nodes t.map

  let map_nodes f t =
    let map = List.map (C.map_nodes f) t.map in
    { map ; others=t.others }

  let simplify t =
    let t_is_empty t = C.is_empty t in
    let t_is_any t = C.neg t |> C.is_empty in
    let map = List.map C.simplify t.map in
    let p = if t.others then t_is_any else t_is_empty in
    let map = List.filter (fun t -> p t |> not) map in
    { map ; others = t.others }

  let components t = (t.map, t.others)

  let destruct t =
    if t.others then
      (false, List.map C.neg t.map)
    else
      (true,  t.map)

  let rec find_tag tag l =
    match l with
      [] -> None
    | a :: ll -> 
      let c = C.Tag.compare (C.tag a) tag in
      if c < 0 then find_tag tag ll
      else if c > 0 then None
      else Some a
  let get tag t =
    match find_tag tag t.map with
    | Some a -> a
    | None -> 
      if t.others then C.any tag
      else  C.empty tag

  let map f t =
    let map = List.map f t.map in
    { t with map }

  let equal t1 t2 =
    Bool.equal t1.others t2.others &&
    try
      List.for_all2 C.equal t1.map t2.map
    with _ -> false
  let compare t1 t2 =
    Bool.compare t1.others t2.others |> ccmp
      (List.compare C.compare) t1.map t2.map
end
