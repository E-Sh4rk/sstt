(** Type transformation and simplification *)

open Core


val transform : (VDescr.t -> VDescr.t) -> Ty.t -> Ty.t
(** [transform f ty] returns the type obtained by applying [f] on the full
      descriptor of [ty], and on the full descriptor of every node in the result
      recursively. It uses a cache to avoid calling [f] twice on the same
      descriptor.

    Note that the function will may not terminate if [f] creates arbitrarily
    many new descriptors. For instance, if [f] is the function that maps
    singleton intervals to their successors: 
    {math 
    \texttt{f : } (n\texttt{..}n)\mapsto (n+1\texttt{..}n+1)
    }
    then when applied to a type {m t\equiv\texttt{(0..0)}}, [transform f t] will
    not terminate.
*)

val simplify : ?normalize:(Ty.t -> Ty.t) -> Ty.t -> Ty.t
(** [simplify ?normalize ty] returns a type equivalent to [ty] but where
    atoms of all components have been merged together when possible.
    If [?normalize] is provided, atoms of DNFs that become redundant
    when [normalize] is applied to the type are filtered out.
*)