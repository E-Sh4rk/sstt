open Sigs
open Sstt_utils

module Make(N:Node) = struct
  module Intervals = Intervals
  module Enums = Enums
  module Arrows = Arrows.Make(N)
  module Records = Records.Make(N)
  module Tuples = Tuples.Make(N)
  module Tags = Tags.Make(N)

  type component =
    | Intervals of Intervals.t
    | Enums of Enums.t
    | Arrows of Arrows.t
    | Records of Records.t
    | Tuples of Tuples.t
    | Tags of Tags.t

  type t = {
    intervals : Intervals.t;
    enums : Enums.t ;
    arrows : Arrows.t ;
    records : Records.t ;
    tuples : Tuples.t ;
    tags : Tags.t ;
    others : bool
  }
  type node = N.t

  let any = {
    enums = Enums.any ;
    tags = Tags.any  ;
    tuples = Tuples.any ;
    arrows = Arrows.any ;
    records = Records.any ;
    intervals = Intervals.any ;
    others = true
  }

  let empty = {
    enums = Enums.empty ;
    tags = Tags.empty ;
    tuples = Tuples.empty ;
    arrows = Arrows.empty ;
    records = Records.empty ;
    intervals = Intervals.empty ;
    others = false
  }

  let hash t =
    Hash.(mix3    (Bool.hash t.others)
            (mix3 (Enums.hash t.enums) (Intervals.hash t.intervals) (Arrows.hash t.arrows))    
            (mix3 (Records.hash t.records) (Tuples.hash t.tuples) (Tags.hash t.tags)))
  let mk_enums a = { empty with enums = a }
  let mk_tags a = { empty with tags = a }
  let mk_arrows a = { empty with arrows = a }
  let mk_tuples a = { empty with tuples = a }
  let mk_records a = { empty with records = a }
  let mk_intervals a = { empty with intervals = a }
  let mk_others b = { empty with others=b }

  let mk_enum a = Enums.mk a |> mk_enums
  let mk_tagcomp a = Tags.mk_comp a |> mk_tags
  let mk_tag a = Tags.mk a |> mk_tags
  let mk_arrow a = Arrows.mk a |> mk_arrows
  let mk_tuplecomp a = Tuples.mk_comp a |> mk_tuples
  let mk_tuple a = Tuples.mk a |> mk_tuples
  let mk_record a = Records.mk a |> mk_records
  let mk_interval a = Intervals.mk a |> mk_intervals

  let get_enums t = t.enums
  let get_tags t = t.tags
  let get_arrows t = t.arrows
  let get_tuples t = t.tuples
  let get_records t = t.records
  let get_intervals t = t.intervals
  let get_others t = t.others

  let components t =
    [ Enums t.enums ; Arrows t.arrows ; Intervals t.intervals ;
      Tags t.tags ; Tuples t.tuples ; Records t.records ], t.others
  let set_component t comp =
    match comp with
    | Enums enums -> { t with enums }
    | Arrows arrows -> { t with arrows }
    | Intervals intervals -> { t with intervals }
    | Tags tags -> { t with tags }
    | Tuples tuples -> { t with tuples }
    | Records records -> { t with records }
  let of_component = set_component empty
  let of_components (lst, others) = List.fold_left set_component { empty with others } lst

  let unop fenu ftag ftup farr frec fint fo t = 
    let enums = fenu t.enums in
    let tags = ftag t.tags in
    let tuples = ftup t.tuples in
    let arrows = farr t.arrows in
    let records = frec t.records in
    let intervals = fint t.intervals in
    let others = fo t.others in
    if enums == t.enums && tags == t.tags && tuples == t.tuples && arrows = t.arrows &&
       records == t.records && intervals == t.intervals && others == t.others
    then t
    else
      { enums; tags; tuples; arrows; records; intervals; others }
      
  let binop fato ftag ftup farr frec fint fo t1 t2 = {
    enums = fato t1.enums t2.enums ;
    tags = ftag t1.tags t2.tags ;
    tuples = ftup t1.tuples t2.tuples ;
    arrows = farr t1.arrows t2.arrows ;
    records = frec t1.records t2.records ;
    intervals = fint t1.intervals t2.intervals ;
    others = fo t1.others t2.others
  }

  let cap = fcap ~empty ~any ~cap:(binop Enums.cap Tags.cap Tuples.cap Arrows.cap Records.cap Intervals.cap (&&))
  let cup = fcup ~empty ~any ~cup:(binop Enums.cup Tags.cup Tuples.cup Arrows.cup Records.cup Intervals.cup (||))
  let diff = fdiff ~empty ~any ~diff:(binop Enums.diff Tags.diff Tuples.diff Arrows.diff Records.diff Intervals.diff
                                            (fun a b -> a && not b))
  let neg = fneg ~empty ~any ~neg:(unop Enums.neg Tags.neg Tuples.neg Arrows.neg Records.neg Intervals.neg not)
  let is_empty t = t == empty ||
                   not t.others &&
                   Enums.is_empty t.enums &&
                   Intervals.is_empty t.intervals &&
                   Tags.is_empty t.tags &&
                   Tuples.is_empty t.tuples &&
                   Arrows.is_empty t.arrows &&
                   Records.is_empty t.records

  let direct_nodes t =
    [ Enums.direct_nodes t.enums ;
      Tags.direct_nodes t.tags ;
      Tuples.direct_nodes t.tuples ;
      Arrows.direct_nodes t.arrows ;
      Records.direct_nodes t.records ;
      Intervals.direct_nodes t.intervals
    ] |> List.concat
  let direct_row_vars t = Records.direct_row_vars t.records

  let construct (pos, cs) =
    let res = of_components (cs, false) in
    if pos then res else neg res

  let destruct t =
    let pos, t = not t.others, if t.others then neg t else t in
    (pos, components t |> fst)
  
  let simplify = unop Enums.simplify Tags.simplify Tuples.simplify
      Arrows.simplify Records.simplify Intervals.simplify Fun.id
  let map_nodes f = unop (Enums.map_nodes f) (Tags.map_nodes f) (Tuples.map_nodes f)
      (Arrows.map_nodes f) (Records.map_nodes f) (Intervals.map_nodes f) Fun.id
  let substitute s t =
    let records = Records.substitute s t.records in
    if records == t.records then t else { t with records }

  let compare t1 t2 =
    Bool.compare t1.others t2.others |> ccmp
      Enums.compare t1.enums t2.enums |> ccmp
      Intervals.compare t1.intervals t2.intervals |> ccmp
      Tags.compare t1.tags t2.tags |> ccmp
      Tuples.compare t1.tuples t2.tuples |> ccmp
      Arrows.compare t1.arrows t2.arrows |> ccmp
      Records.compare t1.records t2.records

  let equal t1 t2 =
    Bool.equal t1.others t2.others &&
    Enums.equal t1.enums t2.enums &&
    Intervals.equal t1.intervals t2.intervals &&
    Tags.equal t1.tags t2.tags &&
    Tuples.equal t1.tuples t2.tuples &&
    Arrows.equal t1.arrows t2.arrows &&
    Records.equal t1.records t2.records
end
