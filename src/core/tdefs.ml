open Base

module Tag = Id.NamedIdentifier()
module TagMap = Map.Make (Tag)
module IntMap = Map.Make (Int)

type node = {
  id : int ;
  neg : node ;
  mutable def : vdescr option ;
  mutable simplified : bool ;
}

and vdescr = (Var.t, descr) Bdd.t

and descr = {
  atoms : atoms;
  intervals : intervals;
  arrows : arrows;
  records : records;
  tuples : tuples;
  tags : tags;
}

and atoms = Atoms.t

and intervals = Intervals.t

and arrows = (arrow_atom, bool) Bdd.t
and arrow_atom = node * node

and records = (record_atom, bool) Bdd.t
and record_atom = { bindings : onode LabelMap.t ; opened : bool }
and record_atom' = { bindings : onode LabelMap.t ; opened : bool; required : LabelSet.t option }
and onode = node * bool

and 'map tagged = { map : 'map; others : bool }

and tuples = tuple_comp IntMap.t tagged
and tuple_comp = int * (tuple_atom, bool) Bdd.t
and tuple_atom = node list

and tags = tag_comp TagMap.t tagged
and tag_comp = Tag.t * (tag_atom, bool) Bdd.t
and tag_atom = Tag.t * node

let empty_descr = {
  atoms = Atoms.empty;
  intervals = Intervals.empty;
  arrows = Bdd.Leaf false;
  records = Bdd.Leaf false;
  tuples = {map = IntMap.empty; others = false };
  tags = { map = TagMap.empty; others = false }
}
let any_descr = {
  atoms = Atoms.any;
  intervals = Intervals.any;
  arrows = Bdd.Leaf true;
  records = Bdd.Leaf true;
  tuples = {map = IntMap.empty; others = true };
  tags = { map = TagMap.empty; others = true }
}

let empty_vdescr : vdescr = Bdd.Leaf empty_descr
let any_vdescr : vdescr = Bdd.Leaf any_descr


module N  = struct
  type t = node

  let neg n = n.neg
  let equal n1 n2 = n1.id = n2.id
  let hash n = Int.hash n.id
  let compare n1 n2 = Int.compare n1.id n2.id


  let next_id =
    let c = ref (~-1) in
    fun () -> c := !c + 1 ; !c
  let mk () =
    let id1 = next_id () in
    let id2 = next_id () in
    let rec t =
      {
        id = id1 ;
        def = None ;
        simplified = false ;
        neg = {
          id = id2 ;
          def = None ;
          simplified = false ;
          neg = t
        }
      }
    in
    t

  let define simplified t pos neg = 
    t.def <- Some pos ;
    if simplified then t.simplified <- true ;
    t.neg.def <- Some neg ;
    if simplified then t.neg.simplified <- true

  let has_def t = t.def |> Option.is_some
  let def t = t.def |> Option.get

  let is_simplified t = t.simplified

end

let empty_node =
  let t = N.mk () in
  N.define true t empty_vdescr any_vdescr; t
let any_node = empty_node.neg
