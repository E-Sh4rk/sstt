open Base

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type TyBase = sig
  type t
  type node

  val any : unit -> t
  val empty : unit -> t

  include Comparable with type t := t
end

module type SetTheoretic = sig
  type t
  val cap : t -> t -> t
  val cup : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val conj : t list -> t
  val disj : t list -> t

  val is_empty : t -> bool
  val is_any : t -> bool
  val leq : t -> t -> bool
  val equiv : t -> t -> bool
  val disjoint : t -> t -> bool
end

(* DNF *)

module type Dnf = sig
  type atom
  type atom'
  type leaf
  type t = (atom list * atom list * leaf) list
  type t' = (atom' * leaf) list

  val simplify : t -> t
  val combine : t -> t'
end

module type NeverAtom = sig
  type t = |
  include Comparable with type t := t
end
module NeverAtom : NeverAtom = struct
  type t = |
  let absurd x = match (x:t) with _ -> .
  let equal = absurd
  let compare = absurd
end

(* Atoms *)

module type AtomAtom = Id.NamedIdentifier

module type Atoms = sig
  include TyBase
  module Atom : AtomAtom
  val mk : Atom.t -> t
  val get : t -> bool * Atom.t list
  val mk' : bool * Atom.t list -> t
  val pp : Format.formatter -> t -> unit
end

(* Intervals *)

module type IntervalAtom = sig
  type t
  val mk : Z.t -> Z.t -> t
  val mk' : Z.t option -> Z.t option -> t
  val mk_singl : Z.t -> t
  val get : t -> Z.t option * Z.t option
  include Comparable with type t := t
  val pp : Format.formatter -> t -> unit
end

module type Intervals = sig
  include TyBase
  module Atom : IntervalAtom
  val mk : Atom.t -> t
  val get : t -> Atom.t list
  val mk' : Atom.t list -> t
end

(* Arrows *)

module type ArrowAtom = sig
  type node
  type t = node * node
  include Comparable with type t := t
end

module type Arrows = sig
  include TyBase
  module Atom : ArrowAtom with type node := node
  module Dnf : Dnf with type atom = Atom.t and type atom' = NeverAtom.t and type leaf = bool
  val mk : Atom.t -> t
  val dnf : t -> Dnf.t
  val of_dnf : Dnf.t -> t
end

(* Records *)

module type OTy = sig
  type node
  include TyBase with type node := node and type t = node * bool
  include SetTheoretic with type t := t
  val absent : unit -> t
  val is_absent : t -> bool
end

module type RecordAtom = sig
  type node
  module OTy : OTy with type node := node
  type t = { bindings : OTy.t LabelMap.t ; opened : bool }
  val dom : t -> LabelSet.t
  val find : Label.t -> t -> OTy.t
  val to_tuple : Label.t list -> t -> OTy.t list
  include Comparable with type t := t
end

module type RecordAtom' = sig
  type node
  module OTy : OTy with type node := node
  type kind = Opened | Closed | OpenedStrict of LabelSet.t
  type t = { bindings : OTy.t LabelMap.t ; kind : kind }
  val dom : t -> LabelSet.t
  val find : Label.t -> t -> OTy.t
  include Comparable with type t := t
end

module type Records = sig
  include TyBase
  module Atom : RecordAtom with type node := node
  module Atom' : RecordAtom' with type node := node
  module Dnf : Dnf with type atom = Atom.t and type atom' = Atom'.t and type leaf = bool
  val mk : Atom.t -> t
  val dnf : t -> Dnf.t
  val of_dnf : Dnf.t -> t
end

(* Tuples *)

module type TupleAtom = sig
  type node
  type t = node list
  include Comparable with type t := t
end

module type Products = sig
  include TyBase
  module Atom : TupleAtom with type node := node
  module Dnf : Dnf with type atom = Atom.t and type atom' = Atom.t and type leaf = bool
  val any : int -> t
  val empty : int -> t
  val mk : Atom.t -> t
  val len : t -> int
  val dnf : t -> Dnf.t
  val of_dnf : int -> Dnf.t -> t
end

module type Tuples = sig
  include TyBase
  module Products : Products with type node := node
  val mk_product : Products.Atom.t -> t
  val mk_products : Products.t -> t
  val components : t -> Products.t list * bool
  val get : int -> t -> Products.t
  val mk : Products.t list * bool -> t
  val map : (Products.t -> Products.t) -> t -> t
end

(* Descr *)

module type Descr = sig
  include TyBase

  module Arrows : Arrows with type node := node
  module Atoms : Atoms with type node := node
  module Intervals : Intervals with type node := node
  module Records : Records with type node := node
  module Tuples : Tuples with type node := node

  type component =
  | Atoms of Atoms.t
  | Arrows of Arrows.t
  | Intervals of Intervals.t
  | Records of Records.t
  | Tuples of Tuples.t

  val mk_atom : Atoms.Atom.t -> t
  val mk_atoms : Atoms.t -> t
  val mk_product : Tuples.Products.Atom.t -> t
  val mk_products : Tuples.Products.t -> t
  val mk_tuples : Tuples.t -> t
  val mk_arrow : Arrows.Atom.t -> t
  val mk_arrows : Arrows.t -> t
  val mk_record : Records.Atom.t -> t
  val mk_records : Records.t -> t
  val mk_interval : Intervals.Atom.t -> t
  val mk_intervals : Intervals.t -> t

  val get_atoms : t -> Atoms.t
  val get_tuples : t -> Tuples.t
  val get_arrows : t -> Arrows.t
  val get_records : t -> Records.t
  val get_intervals : t -> Intervals.t

  val components : t -> component list
  val set_component : t -> component -> t
  val of_component : component -> t
  val of_components : component list -> t
end

(* VDescr *)

module type VDescr = sig
  include TyBase

  module Descr : Descr with type node := node
  module Dnf : Dnf with type atom = Var.t and type atom' = NeverAtom.t and type leaf = Descr.t

  val mk_var : Var.t -> t
  val mk_descr : Descr.t -> t
  val get_descr : t -> Descr.t
  val map_descr : (Descr.t -> Descr.t) -> t -> t

  val dnf : t -> Dnf.t
  val of_dnf : Dnf.t -> t
end

(* Nodes *)

module type Node = sig
  type t
  include TyBase with type t:=t and type node:=t

  module VDescr : VDescr with type node := t

  val def : t -> VDescr.t
  val of_def : VDescr.t -> t

  val mk_var : Var.t -> t
  val mk_descr : VDescr.Descr.t -> t
  val get_descr : t -> VDescr.Descr.t

  include SetTheoretic with type t := t
  val with_own_cache : ('a -> 'b) -> 'a -> 'b

  val dependencies : t -> t list
  val vars : t -> VarSet.t
  val vars_toplevel : t -> VarSet.t

  val from_eqs : (Var.t * t) list -> t list
  val substitute : t VarMap.t -> t -> t
  val simplify : t -> unit

  val hash : t -> int
end

(* Ty *)

module type Ty = sig
  type t
  include TyBase with type t:=t and type node:=t

  module VDescr : VDescr with type node := t

  val any : t
  val empty : t

  val def : t -> VDescr.t
  val of_def : VDescr.t -> t

  val mk_var : Var.t -> t
  val mk_descr : VDescr.Descr.t -> t
  val get_descr : t -> VDescr.Descr.t

  include SetTheoretic with type t := t

  val dependencies : t -> t list
  val vars : t -> VarSet.t
  val vars_toplevel : t -> VarSet.t

  val from_eqs : (Var.t * t) list -> t list
  val substitute : t VarMap.t -> t -> t

  val hash : t -> int
end