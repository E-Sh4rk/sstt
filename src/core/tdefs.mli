module Tag : Id.NamedIdentifier
module TagMap : sig include module type of Map.Make (Tag) end
module IntMap : sig include module type of Map.Make (Int) end

type node 
and vdescr = (Base.Var.t, descr) Bdd.t
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
and record_atom =  { bindings : onode Base.LabelMap.t ; opened : bool }
and record_atom' =  { bindings : onode Base.LabelMap.t ; opened : bool; required : Base.LabelSet.t option }
and onode = node * bool

and 'map tagged = { map : 'map; others : bool }

and tuples = tuple_comp IntMap.t tagged
and tuple_comp = int * (tuple_atom, bool) Bdd.t
and tuple_atom = node list

and tags = tag_comp TagMap.t tagged
and tag_comp = Tag.t * (tag_atom, bool) Bdd.t
and tag_atom = Tag.t * node

val empty_descr : descr

val any_descr : descr

val empty_vdescr : vdescr

val any_vdescr : vdescr

val empty_node : node

val any_node : node

val empty_onode : onode

val any_onode : onode

val absent_onode : onode

module N : 
sig
  type t = node

  val equal : node -> node -> bool
  val compare : node -> node -> int
  val hash : node -> int

  val neg : t -> t

  val mk : unit -> node

  val define : bool -> node -> vdescr -> vdescr -> unit

  val def : node -> vdescr
  val has_def : node -> bool

  val is_simplified : node -> bool

end
