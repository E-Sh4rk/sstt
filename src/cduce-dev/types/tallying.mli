val tallying :
  ?var_order:Var.t list -> Var.Set.t -> (Types.t * Types.t) list -> Subst.t list
(** [tallying delta types] retuns a list of substitutions each of which is a
solution to the {o tallying problem}. Given the list (s_i, t_i) of types,
each substitution sigma is such that s_i@sigma < t_i@sigma for all i.
The set [delta] represent variables that cannot be instanciated (monomorphic
variables).
*)

val test_tallying : ?var_order:Var.t list -> Var.Set.t -> (Types.t * Types.t)
list -> bool (** [test_tallying delta types] returns [true] if and only if there
  exists a solution to the {o tallying problem} without returning it. It is
  equivalent to testing whether the result of [tallying delta types] is the
  empty list, but is faster (stops at the first substitution found, if any
  without putting it in normal form).
*)

val apply_full : Var.Set.t -> Types.t -> Types.t -> Types.t option
(** apply_raw s t returns the 4-tuple (subst,ss, tt, res) where
   subst is the set of substitution that make the application succeed,
   ss and tt are the expansions of s and t corresponding to that substitution
   and res is the type of the result of the application *)

val apply_raw :
  Var.Set.t ->
  Types.t ->
  Types.t ->
  (Subst.t list * Types.t * Types.t * Types.t) option

val squareapply :
  Var.Set.t -> Types.t -> Types.t -> (Subst.t list * Types.t) option
