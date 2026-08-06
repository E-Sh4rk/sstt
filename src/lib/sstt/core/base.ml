
(** @canonical Sstt.Label *)
module Label = Id.NamedIdentifier()
(** Labels used for field names in records. *)

(** @canonical Sstt.Tag *)
module Tag : sig
  include Id.NamedIdentifier

  (** The properties of the interpretation of a tag. A tagged type
      {m \texttt{tag}(s)} denotes the image of [s] by a function
      {m f_\texttt{tag}} that is specific to the tag; these properties describe
      that function, and are what makes it possible to compare two types
      tagged with the same tag.

      - [NoProperty]: nothing is known about {m f_\texttt{tag}}, so
        {m \texttt{tag}(s)} and {m \texttt{tag}(s')} can only be related when
        [s] and [s'] are equivalent.
      - [Monotonic]: {m f_\texttt{tag}} is monotonic, that is, [s <= s']
        implies {m \texttt{tag}(s) \leq \texttt{tag}(s')}. In addition,
        [preserves_cup] (resp. [preserves_cap]) states that it distributes over
        unions (resp. intersections), that is,
        {m \texttt{tag}(s\cup s') \equiv \texttt{tag}(s)\cup\texttt{tag}(s')}
        (resp. with {m \cap}), and [preserves_extremum] states that it
        preserves the corresponding extremum, that is, {m \texttt{tag}(}{%html: <span
        style='font-size:large'>𝟘</span>%}{m )} is empty (if [preserves_cap] is true) and
        {m \texttt{tag}(}{%html: <span style='font-size:large'>𝟙</span>%}{m )}
        contains every value tagged with this tag (if [preserves_cup] is true).
  *)
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool ; preserves_extremum:bool }

  val mk' : string -> prop -> t
  (** [mk' name prop] makes a new tag of name [name] whose interpretation
      satisfies [prop]. As {!mk}, it generates a fresh tag even if another tag
      has the same name. *)

  val properties : t -> prop
  (** [properties tag] returns the properties of the interpretation of [tag].
      Tags built with {!mk} are given the properties of the identity, that is,
      [Monotonic] with all three fields set to [true]. *)
end = struct
  module I = Id.NamedIdentifier()
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool ; preserves_extremum:bool }

  type t = I.t * prop
  let default_prop = Monotonic { preserves_cap=true ; preserves_cup=true ; preserves_extremum=true }
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


(** @canonical Sstt.RowVar *)
module RowVar = Id.NamedIdentifier()
(** Row variables. *)

(** @canonical Sstt.RowVarSet *)
module RowVarSet = Set.Make(RowVar)
(** Sets of row variables. *)

(** @canonical Sstt.RowVarMap *)
module RowVarMap = Map.Make(RowVar)
(** Maps indexed by row variables. *)


(** @canonical Sstt.MixVarSet *)
module MixVarSet = Id.MixSet(VarSet)(RowVarSet)
(** Sets of type and row variables. *)

(** @canonical Sstt.MixVarMap *)
module MixVarMap = Id.MixMap(VarMap)(RowVarMap)
(** Maps indexed by type and row variables. *)