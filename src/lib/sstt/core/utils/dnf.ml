open Sigs
open Sstt_utils

(* General components *)

module type Comp = sig
  type atom
  type leaf

  val atom_is_valid : atom -> bool
  val leaf_is_empty : leaf -> bool (* Can be approximate *)
  val leq : (atom,leaf) dnf -> (atom,leaf) dnf -> bool
end

module type Dnf = sig
  type atom
  type leaf
  type t = (atom,leaf) dnf
  val simplify : t -> t
  val import : t -> t
  val export : t -> t
end

module Make(C:Comp) : Dnf with type atom := C.atom and type leaf := C.leaf = struct
  type t = (C.atom,C.leaf) dnf

  let normalize dnf =
    let must_keep_line (ps,ns,l) =
      if List.for_all C.atom_is_valid ps &&
         List.for_all C.atom_is_valid ns
      then C.leaf_is_empty l |> not
      else invalid_arg "DNF has invalid atoms."
    in
    List.filter must_keep_line dnf

  let simplify dnf =
    (* Remove useless clauses that may be generated from the BDD *)
    let dnf = dnf |> map_among_others (fun (cp, cn, l) c_others ->
        let cp = cp |> filter_among_others (fun _ cp_others ->
            C.leq ((cp_others, cn, l)::c_others) dnf |> not
          ) in
        let cn = cn |> filter_among_others (fun _ cn_others ->
            C.leq ((cp, cn_others, l)::c_others) dnf |> not
          ) in
        (cp, cn, l)
      )
    in
    (* Remove useless summands (must be done AFTER clauses simplification) *)
    dnf |> filter_among_others (fun c c_others ->
        C.leq (c::c_others) c_others |> not
      )
  
  let import, export = normalize, normalize
end

module type OptComp = sig
  include Comp
  type atom'

  val any' : atom'
  val to_atom : atom' -> atom list * atom list
  val to_atom' : atom * bool -> atom' list
  val combine : atom' -> atom' -> atom' option
end

module type Dnf' = sig
  include Dnf
  type atom'
  type t' = (atom' * leaf) list
  val simplify' : t' -> t'
  val import' : t' -> t
  val export' : t -> t'
end

module Make'(C:OptComp) : Dnf' with type atom:=C.atom and type atom':=C.atom'
                                and type leaf:=C.leaf = struct
  include Make(C)

  type t' = (C.atom' * C.leaf) list

  let to_dnf' dnf =
    let rec aux c =
      match c with
      | [] -> [C.any']
      | [(a, b)] -> C.to_atom' (a, b)
      | (a, b)::c ->
        let a' = C.to_atom' (a, b) in
        let c' = aux c in
        cartesian_product a' c' |> List.filter_map (fun (a, a') -> C.combine a a')
    in
    let dnf = dnf |> List.map (fun (cp, cn, l) ->
        let cp = cp |> List.map (fun a -> (a, true)) in
        let cn = cn |> List.map (fun a -> (a, false)) in
        cp@cn, l
      ) in
    dnf |> List.concat_map (fun (c,l) -> aux c |> List.map (fun c -> (c,l)))

  let conv (a',l) = let ps,ns = C.to_atom a' in (ps,ns,l)
  let to_dnf dnf' = List.map conv dnf'

  let leq t1 t2 = C.leq (List.map conv t1) (List.map conv t2)
  let simplify' t =
    (* Remove useless summands *)
    t |> filter_among_others (fun a a_others ->
        leq (a::a_others) a_others |> not
      )

  let import' cdnf = to_dnf cdnf |> import
  let export' dnf = export dnf |> to_dnf'
end

(* Leaf components *)

module type LComp = sig
  type atom

  val atom_is_valid : atom -> bool
  val leq : (atom,bool) dnf -> (atom,bool) dnf -> bool
end

module type LOptComp = sig
  include LComp
  type atom'

  val any' : atom'
  val to_atom : atom' -> atom list * atom list
  val to_atom' : atom * bool -> atom' list
  val combine : atom' -> atom' -> atom' option
end

module LMake(C:LComp) = struct

  include Make(struct
    include C
    type leaf = bool
    let leaf_is_empty b = not b
  end)

  type t = C.atom ldnf

  let rm_leaf dnf = List.map (fun (ps,ns,_) -> (ps,ns)) dnf
  let add_leaf dnf = List.map (fun (ps,ns) -> (ps,ns,true)) dnf

  let import dnf = add_leaf dnf |> import
  let export dnf = export dnf |> rm_leaf
  let simplify dnf = add_leaf dnf |> simplify |> rm_leaf
end

module LMake'(C:LOptComp) = struct

  include Make'(struct
    include C
    type leaf = bool
    let leaf_is_empty b = not b
  end)

  type t' = C.atom' cdnf

  let rm_leaf dnf = List.map (fun (ps,ns,_) -> (ps,ns)) dnf
  let add_leaf dnf = List.map (fun (ps,ns) -> (ps,ns,true)) dnf
  let rm_leaf' cdnf = List.map fst cdnf
  let add_leaf' cdnf = List.map (fun a -> (a,true)) cdnf

  let import dnf = add_leaf dnf |> import
  let export dnf = export dnf |> rm_leaf
  let simplify dnf = add_leaf dnf |> simplify |> rm_leaf
  let import' cdnf = add_leaf' cdnf |> import'
  let export' dnf = export' dnf |> rm_leaf'
  let simplify' dnf = add_leaf' dnf |> simplify' |> rm_leaf'
end
