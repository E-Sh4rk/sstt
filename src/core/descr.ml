open Sigs
open Sstt_utils

module Make(N:Node) = struct
  module Arrows = Arrows.Make(N)
  module Atoms = Atoms.Make(N)
  module Intervals = Intervals.Make(N)
  module Records = Records.Make(N)
  module Tuples = Tuples.Make(N)

  type component =
  | Atoms of Atoms.t
  | Arrows of Arrows.t
  | Intervals of Intervals.t
  | Records of Records.t
  | Tuples of Tuples.t

  type t = {
    atoms : Atoms.t ;
    tuples : Tuples.t ;
    arrows : Arrows.t ;
    records : Records.t ;
    intervals : Intervals.t
  }
  type node = N.t

  let any () = {
    atoms = Atoms.any () ;
    tuples = Tuples.any () ;
    arrows = Arrows.any () ;
    records = Records.any () ;
    intervals = Intervals.any ()
  }

  let empty () = {
    atoms = Atoms.empty () ;
    tuples = Tuples.empty () ;
    arrows = Arrows.empty () ;
    records = Records.empty () ;
    intervals = Intervals.empty ()
  }

  let mk_atoms a = { (empty ()) with atoms = a }
  let mk_arrows a = { (empty ()) with arrows = a }
  let mk_tuples a = { (empty ()) with tuples = a }
  let mk_records a = { (empty ()) with records = a }
  let mk_intervals a = { (empty ()) with intervals = a }

  let mk_atom a = Atoms.mk a |> mk_atoms
  let mk_arrow a = Arrows.mk a |> mk_arrows
  let mk_products a = Tuples.mk_products a |> mk_tuples
  let mk_product a = Tuples.mk_product a |> mk_tuples
  let mk_record a = Records.mk a |> mk_records
  let mk_interval a = Intervals.mk a |> mk_intervals

  let get_atoms t = t.atoms
  let get_arrows t = t.arrows
  let get_tuples t = t.tuples
  let get_records t = t.records
  let get_intervals t = t.intervals

  let components t =
    [ Atoms t.atoms ; Arrows t.arrows ; Intervals t.intervals ;
      Tuples t.tuples ; Records t.records ]
  let set_component t comp =
    match comp with
    | Atoms atoms -> { t with atoms }
    | Arrows arrows -> { t with arrows }
    | Intervals intervals -> { t with intervals }
    | Tuples tuples -> { t with tuples }
    | Records records -> { t with records }
  let of_component = set_component (empty ())
  let of_components = List.fold_left set_component (empty ())

  let unop fato ftup farr frec fint t = {
    atoms = fato t.atoms ;
    tuples = ftup t.tuples ;
    arrows = farr t.arrows ;
    records = frec t.records ;
    intervals = fint t.intervals
  }

  let binop fato ftup farr frec fint t1 t2 = {
    atoms = fato t1.atoms t2.atoms ;
    tuples = ftup t1.tuples t2.tuples ;
    arrows = farr t1.arrows t2.arrows ;
    records = frec t1.records t2.records ;
    intervals = fint t1.intervals t2.intervals
  }

  let cap = binop Atoms.cap Tuples.cap Arrows.cap Records.cap Intervals.cap
  let cup = binop Atoms.cup Tuples.cup Arrows.cup Records.cup Intervals.cup
  let diff = binop Atoms.diff Tuples.diff Arrows.diff Records.diff Intervals.diff
  let neg = unop Atoms.neg Tuples.neg Arrows.neg Records.neg Intervals.neg

  let is_empty t =
    Atoms.is_empty t.atoms &&
    Tuples.is_empty t.tuples &&
    Arrows.is_empty t.arrows &&
    Records.is_empty t.records &&
    Intervals.is_empty t.intervals

  let direct_nodes t =
    [ Atoms.direct_nodes t.atoms ;
      Tuples.direct_nodes t.tuples ;
      Arrows.direct_nodes t.arrows ;
      Records.direct_nodes t.records ;
      Intervals.direct_nodes t.intervals
    ] |> List.concat

  let simplify = unop Atoms.simplify Tuples.simplify
    Arrows.simplify Records.simplify Intervals.simplify
  let map_nodes f = unop (Atoms.map_nodes f) (Tuples.map_nodes f)
    (Arrows.map_nodes f) (Records.map_nodes f) (Intervals.map_nodes f)

  let compare t1 t2 =
    Atoms.compare t1.atoms t2.atoms |> ccmp
    Tuples.compare t1.tuples t2.tuples |> ccmp
    Arrows.compare t1.arrows t2.arrows |> ccmp
    Records.compare t1.records t2.records |> ccmp
    Intervals.compare t1.intervals t2.intervals

  let equal t1 t2 =
    Atoms.equal t1.atoms t2.atoms &&
    Tuples.equal t1.tuples t2.tuples &&
    Arrows.equal t1.arrows t2.arrows &&
    Records.equal t1.records t2.records &&
    Intervals.equal t1.intervals t2.intervals
end
