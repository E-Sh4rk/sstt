open Sigs
open Sstt_utils.Utils

module type Atom = sig
  type leaf
  type t
  type dnf = (t list * t list * leaf) list
  type t'
  type dnf' = (t' * leaf) list
  
  val undesirable_leaf : leaf -> bool
  val leq : dnf -> dnf -> bool
  val to_s : t * bool -> t' list (* Result MUST NOT contain empty atoms *)
  val combine : t' -> t' -> t' option (* MUST return None if the result is empty *)
end

module Make(A:Atom)(N:Node) = struct
  type atom = A.t
  type atom' = A.t'
  type leaf = A.leaf
  type t = A.dnf
  type t' = A.dnf'

  let mk dnf = dnf |> List.filter (fun (_,_,d) -> A.undesirable_leaf d |> not)

  let simplify dnf =
    (* Remove useless summands (useless if the BDD is simplified...) *)
    dnf |> filter_among_others (fun (cp, cn, l) c_others ->
      A.leq ((cp, cn, l)::c_others) c_others |> not
    )
    (* Remove useless clauses that may be generated from the BDD *)
    |> map_among_others (fun (cp, cn, l) c_others ->
      let cp = cp |> filter_among_others (fun _ cp_others ->
        A.leq ((cp_others, cn, l)::c_others) dnf |> not
      ) in
      let cn = cn |> filter_among_others (fun _ cn_others ->
        A.leq ((cp, cn_others, l)::c_others) dnf |> not
      ) in
      (cp, cn, l)
    )

  let combine dnf =
    let rec aux c =
      match c with
      | [] -> []
      | (a, b)::c ->
        let a' = A.to_s (a, b) in
        let c' = aux c in
        carthesian_product a' c' |> List.filter_map (fun (a, a') -> A.combine a a')
    in
    let dnf = dnf |> List.map (fun (cp, cn, l) ->
      let cp = cp |> List.map (fun a -> (a, true)) in
      let cn = cn |> List.map (fun a -> (a, false)) in
      cp@cn, l
    ) in
    dnf |> List.map (fun (c,l) -> aux c |> List.map (fun c -> (c,l))) |> List.concat

  let mk t = N.with_own_cache mk t
  let simplify t = N.with_own_cache simplify t
  let combine t = N.with_own_cache combine t
end
