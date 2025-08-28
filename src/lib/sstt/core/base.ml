
(** @canonical Sstt.Label *)
module Label = Id.NamedIdentifier()
(** Labels used for field names in records. *)

(** @canonical Sstt.LabelSet *)
module LabelSet = Set.Make(Label)
(** Sets of labels. *)

(** @canonical Sstt.LabelMap *)
module LabelMap = Map.Make(Label)
(** Maps indexed by labels. *)


(** @canonical Sstt.Tag *)
module Tag : sig
  include Id.NamedIdentifier
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool }

  val mk' : string -> prop -> t
  val properties : t -> prop
end = struct
  module I = Id.NamedIdentifier()
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool }

  type t = I.t * prop
  let default_prop = Monotonic { preserves_cap=true ; preserves_cup=true }
  let mk name =  (I.mk name, default_prop)
  let mk' name prop =  (I.mk name, prop)
  let name (i,_) = I.name i
  let properties (_,p) = p
  let hash (i,_) = I.hash i
  let compare (i1,_) (i2,_) = I.compare i1 i2
  let equal (i1,_) (i2,_) = I.equal i1 i2
  let pp fmt (i,_) = Format.fprintf fmt "%a" I.pp i
  let pp_unique fmt (i,_) = Format.fprintf fmt "%a" I.pp i
end
(** Identifiers used for tagged type. *)


(** @canonical Sstt.Enum *)
module Enum = Id.NamedIdentifier()
(** Identifiers used for enums type. *)


(** @canonical Sstt.Var *)
module Var = Id.NamedIdentifier()
(** Type variables. *)

(** @canonical Sstt.VarSet *)
module VarSet = Set.Make(Var)
(** Sets of type variables. *)

(** @canonical Sstt.VarMap *)
module VarMap = Map.Make(Var)
(** Maps indexed by type variables. *)