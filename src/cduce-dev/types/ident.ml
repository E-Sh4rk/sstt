(** Handling of identifiers (e.g. type names)
  Identifier are qualified names (e.g. to denote a type defined in a particular
  name space.)
*)

module Id = Ns.QName

type id = Id.t

let mk s : id = (Ns.empty, Encodings.Utf8.mk s)
let to_string = Id.to_string
let print = Id.print

module IdSet = SortedList.Make (Id)
module IdMap = IdSet.Map
module Env = Map.Make (Id)

type 'a id_map = 'a IdMap.map
type fv = IdSet.t

(* TODO: put following decl somewhere else *)
module Label = Ns.Label
module LabelSet = SortedList.Make (Ns.Label)
module LabelMap = LabelSet.Map

type label = Ns.Label.t
type 'a label_map = 'a LabelMap.map
