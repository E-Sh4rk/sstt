open Sigs
open Sstt_utils

module type Atom = sig
  type leaf
  type t
  
  val undesirable_leaf : leaf -> bool
  val leq : (t list * t list * leaf) list -> (t list * t list * leaf) list -> bool
end

module type Atom' = sig
  type leaf
  type t
  type t'
  
  val to_t : t' -> t list * t list
  val to_t' : t * bool -> t' list
  val combine : t' -> t' -> t' option
end

module Make(A:Atom)(N:Node) = struct
  type atom = A.t
  type leaf = A.leaf
  type t = (atom list * atom list * leaf) list

  let mk dnf = dnf |> List.filter (fun (_,_,d) -> A.undesirable_leaf d |> not)

  let simplify dnf =
    (* Remove useless clauses that may be generated from the BDD *)
    let dnf = dnf |> map_among_others (fun (cp, cn, l) c_others ->
      let cp = cp |> filter_among_others (fun _ cp_others ->
        A.leq ((cp_others, cn, l)::c_others) dnf |> not
      ) in
      let cn = cn |> filter_among_others (fun _ cn_others ->
        A.leq ((cp, cn_others, l)::c_others) dnf |> not
      ) in
      (cp, cn, l)
    )
    in
    (* Remove useless summands (must be done AFTER clauses simplification) *)
    dnf |> filter_among_others (fun c c_others ->
      A.leq (c::c_others) c_others |> not
    )

  let mk t = N.with_own_cache mk t
  let simplify t = N.with_own_cache simplify t
end

module Make'(A:Atom)(A':Atom' with type t=A.t and type leaf=A.leaf)(N:Node) = struct
  type atom = A'.t'
  type leaf = A.leaf
  type t = (atom * leaf) list

  let combine any dnf =
    let rec aux c =
      match c with
      | [] -> [any]
      | [(a, b)] -> A'.to_t' (a, b)
      | (a, b)::c ->
        let a' = A'.to_t' (a, b) in
        let c' = aux c in
        carthesian_product a' c' |> List.filter_map (fun (a, a') -> A'.combine a a')
    in
    let dnf = dnf |> List.map (fun (cp, cn, l) ->
      let cp = cp |> List.map (fun a -> (a, true)) in
      let cn = cn |> List.map (fun a -> (a, false)) in
      cp@cn, l
    ) in
    dnf |> List.map (fun (c,l) -> aux c |> List.map (fun c -> (c,l))) |> List.concat
  let from_dnf any dnf = combine any dnf

  let conv (a',l) = let ps,ns = A'.to_t a' in (ps,ns,l)
  let leq t1 t2 = A.leq (List.map conv t1) (List.map conv t2)
  let simplify t =
    (* Remove useless summands *)
    t |> filter_among_others (fun a a_others ->
      leq (a::a_others) a_others |> not
    )
  
  let to_dnf dnf' = List.map conv dnf'

  let from_dnf any t = N.with_own_cache (from_dnf any) t
  let to_dnf t = N.with_own_cache to_dnf t
  let simplify t = N.with_own_cache simplify t
end
