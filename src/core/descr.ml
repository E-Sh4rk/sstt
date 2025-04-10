open Sigs
open Sstt_utils

module Make(N:Node) = struct
  module Arrows = Arrows.Make(N)
  module Atoms = Atoms
  module Intervals = Intervals
  module Records = Records.Make(N)
  module Tags = Tags.Make(N)
  module Tuples = Tuples.Make(N)

  type component =
    | Atoms of Atoms.t
    | Arrows of Arrows.t
    | Intervals of Intervals.t
    | Records of Records.t
    | Tags of Tags.t
    | Tuples of Tuples.t

  type t = Tdefs.descr

  let empty = Tdefs.empty_descr
  let any = Tdefs.any_descr

  let mk_atoms a = { empty with atoms = a }
  let mk_tags a = { empty with tags = a }
  let mk_arrows a = { empty with arrows = a }
  let mk_tuples a = { empty with tuples = a }
  let mk_records a = { empty with records = a }
  let mk_intervals a = { empty with intervals = a }

  let mk_atom a = Atoms.mk a |> mk_atoms
  let mk_tagcomp a = Tags.mk_comp a |> mk_tags
  let mk_tag a = Tags.mk a |> mk_tags
  let mk_arrow a = Arrows.mk a |> mk_arrows
  let mk_tuplecomp a = Tuples.mk_comp a |> mk_tuples
  let mk_tuple a = Tuples.mk a |> mk_tuples
  let mk_record a = Records.mk a |> mk_records
  let mk_interval a = Intervals.mk a |> mk_intervals

  let get_atoms t = t.Tdefs.atoms
  let get_tags t = t.Tdefs.tags
  let get_arrows t = t.Tdefs.arrows
  let get_tuples t = t.Tdefs.tuples
  let get_records t = t.Tdefs.records
  let get_intervals t = t.Tdefs.intervals

  let components t =
    Tdefs.[ Atoms t.atoms ; Arrows t.arrows ; Intervals t.intervals ;
            Tags t.tags ; Tuples t.tuples ; Records t.records ]
  let set_component t comp =
    match comp with
    | Atoms atoms -> { t with Tdefs.atoms }
    | Arrows arrows -> { t with Tdefs.arrows }
    | Intervals intervals -> { t with Tdefs.intervals }
    | Tags tags -> { t with Tdefs.tags }
    | Tuples tuples -> { t with Tdefs.tuples }
    | Records records -> { t with Tdefs.records }
  let of_component = set_component empty
  let of_components = List.fold_left set_component empty

  let unop fato ftag ftup farr frec fint t = Tdefs.{
      atoms = fato t.atoms ;
      tags = ftag t.tags ;
      tuples = ftup t.tuples ;
      arrows = farr t.arrows ;
      records = frec t.records ;
      intervals = fint t.intervals
    }

  let binop fato ftag ftup farr frec fint t1 t2 = Tdefs.{
      atoms = fato t1.atoms t2.atoms ;
      tags = ftag t1.tags t2.tags ;
      tuples = ftup t1.tuples t2.tuples ;
      arrows = farr t1.arrows t2.arrows ;
      records = frec t1.records t2.records ;
      intervals = fint t1.intervals t2.intervals
    }

  let cap = binop Atoms.cap Tags.cap Tuples.cap Arrows.cap Records.cap Intervals.cap
  let cup = binop Atoms.cup Tags.cup Tuples.cup Arrows.cup Records.cup Intervals.cup
  let diff = binop Atoms.diff Tags.diff Tuples.diff Arrows.diff Records.diff Intervals.diff
  let neg = unop Atoms.neg Tags.neg Tuples.neg Arrows.neg Records.neg Intervals.neg

  let is_empty t =
    let open Tdefs in
    Atoms.is_empty t.atoms &&
    Intervals.is_empty t.intervals &&
    Tags.is_empty t.tags &&
    Tuples.is_empty t.tuples &&
    Arrows.is_empty t.arrows &&
    Records.is_empty t.records

  let direct_nodes t =
    Tdefs.[ Atoms.direct_nodes t.atoms ;
            Tags.direct_nodes t.tags ;
            Tuples.direct_nodes t.tuples ;
            Arrows.direct_nodes t.arrows ;
            Records.direct_nodes t.records ;
            Intervals.direct_nodes t.intervals
          ] |> List.concat

  let simplify = unop Atoms.simplify Tags.simplify Tuples.simplify
      Arrows.simplify Records.simplify Intervals.simplify
  let map_nodes f = unop (Atoms.map_nodes f) (Tags.map_nodes f) (Tuples.map_nodes f)
      (Arrows.map_nodes f) (Records.map_nodes f) (Intervals.map_nodes f)

  let compare t1 t2 =
    let open Tdefs in
    Atoms.compare t1.atoms t2.atoms |> ccmp
      Intervals.compare t1.intervals t2.intervals |> ccmp
      Tags.compare t1.tags t2.tags |> ccmp
      Tuples.compare t1.tuples t2.tuples |> ccmp
      Arrows.compare t1.arrows t2.arrows |> ccmp
      Records.compare t1.records t2.records

  let equal t1 t2 =
    let open Tdefs in
    Atoms.equal t1.atoms t2.atoms &&
    Intervals.equal t1.intervals t2.intervals &&
    Tags.equal t1.tags t2.tags &&
    Tuples.equal t1.tuples t2.tuples &&
    Arrows.equal t1.arrows t2.arrows &&
    Records.equal t1.records t2.records
end
