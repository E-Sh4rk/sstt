(** Handling of identifiers (e.g. type names)
  Identifier are qualified names (e.g. to denote a type defined in a particular
  name space.)
*)

(** {2 Identifiers }*)

module Id = Ns.QName

type id = Id.t

val mk : string -> id
val to_string : id -> string
val print : Format.formatter -> id -> unit

module IdSet : SortedList.S with type Elem.t = Id.t
(** Sets of identifiers, implemented as sorted lists. This representation is
    used when structural equality of equal sets is needed.
*)

module IdMap = IdSet.Map
(** Maps using identifiers as keys, implemented as sorted lists of pairs. This
      representation is used when structural equality of equal sets is needed.
*)

module Env : Map.S with type key = Id.t
(** Maps using identifiers as keys, using OCaml standard Map module.
*)

(** {3 Convenience aliases }*)

type 'a id_map = 'a IdMap.map
type fv = IdSet.t

(** {2 Labels}

   Labels are internalised identifiers, that is, identifiers
   represented by a unique integer. These are used e.g. for the internal
   representation of record fields.
*)

module Label = Ns.Label
module LabelSet : SortedList.S with type Elem.t = Ns.Label.t
module LabelMap = LabelSet.Map

type label = Ns.Label.t
type 'a label_map = 'a LabelMap.map
