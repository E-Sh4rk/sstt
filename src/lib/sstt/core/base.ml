
(** @canonical Sstt.Label *)
module Label = Id.NamedIdentifier()
(** Labels used for field names in records. *)

(** @canonical Sstt.LabelSet *)
module LabelSet = Set.Make(Label)
(** Sets of labels. *)

(** @canonical Sstt.LabelMap *)
module LabelMap = Map.Make(Label)
(** Maps indexed by labels. *)

(** @canonical Sstt.Var *)
module Var = Id.NamedIdentifier()
(** Type variables. *)

(** @canonical Sstt.VarSet *)
module VarSet = Set.Make(Var)
(** Sets of type variables. *)

(** @canonical Sstt.VarMap *)
module VarMap = Map.Make(Var)
(** Maps indexed by type variables. *)