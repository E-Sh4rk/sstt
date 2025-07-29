open Sigs
open Sstt_utils

module Atom(N:Node) = struct
  type node = N.t
  type t = node list
  let tag t = List.length t
  let map_nodes f t = List.map f t
  let direct_nodes t = t
  let simplify t = t
  let is_empty t = List.exists N.is_empty t
  let equal t1 t2 =
    List.equal N.equal t1 t2
  let compare t1 t2 =
    List.compare N.compare t1 t2
end

module MakeC(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)
  module Index = Int

  type t = int * Bdd.t
  type node = N.t

  let any n = n, Bdd.any
  let empty n = n, Bdd.empty

  let mk a = Atom.tag a, Bdd.singleton a

  let index (tag,_) = tag
  let len = index

  let check_length len len' =
    if Index.equal len len' |> not then
      raise (Invalid_argument "Heterogeneous tuple lengths.")

  let cap (len1, t1) (len2, t2) = check_length len1 len2 ; len1, Bdd.cap t1 t2
  let cup (len1, t1) (len2, t2) = check_length len1 len2 ; len1, Bdd.cup t1 t2
  let neg (len, t) = len, Bdd.neg t
  let diff (len1, t1) (len2, t2) = check_length len1 len2 ; len1, Bdd.diff t1 t2

  let conj n ps =
    let init = fun () -> List.init n (fun _ -> N.any) in
    mapn init N.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> N.empty) in
    mapn init N.disj ps

  let forall_distribute_diff f ss tt =
    try
      iter_distribute_comb (fun e -> if not (f e) then raise Exit) N.diff ss tt;
      true
    with Exit -> false

  let rec psi ss ts =
    List.exists N.is_empty ss || (* optimisation *)
    match ts with
    | [] -> false
    | tt::ts ->
      forall_distribute_diff (fun ss -> psi ss ts) ss tt

  let is_clause_empty (ps,ns,b) =
    if b then
      match ps, ns with
      | [], [] -> false
      | a::_, _ | [], a::_ ->
        let n = List.length a in
        if n = 0 then
          ns <> [] (* optimisations in psi are not compatible with n = 0 *)
        else
          psi (conj n ps) ns
    else true
  let is_empty' t = Bdd.for_all_lines is_clause_empty t
  let is_empty (_,t) = is_empty' t

  let leq t1 t2 = Bdd.diff t1 t2 |> is_empty'
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module DnfAtom = struct
    type leaf = bool
    type t = Atom.t

    let undesirable_leaf = not
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
  end
  module DnfAtom' = struct
    type leaf = bool
    type t = Atom.t
    type t' = Atom.t

    let to_t a = [a], []
    let to_t' (ns,b) =
      let any_tuple n = List.init n (fun _ -> N.any) in
      let rec aux ns =
        match ns with
        | [] -> []
        | n::ns ->
          let this = (N.neg n)::(any_tuple (List.length ns)) in
          let others = aux ns |> List.map (fun s -> N.any::s) in
          this::others
      in
      if b then [ns] else aux ns
    let to_t' (a,b) = to_t' (a,b) |> List.filter (fun a -> Atom.is_empty a |> not)
    let combine ns1 ns2 =
      let res = List.map2 N.cap ns1 ns2 in
      if Atom.is_empty res then None else Some res
  end
  module Dnf' = DNF.Make'(DnfAtom)(DnfAtom')(N)
  module Dnf = DNF.Make(DnfAtom)(N)

  let dnf (_,t) = Bdd.dnf t |> Dnf.mk
  let dnf' (n,t) = dnf (n,t) |> Dnf'.from_dnf (List.init n (fun _ -> N.any))
  let of_dnf tag dnf =
    dnf |> List.iter (fun (ps,ns,_) ->
        ps |> List.iter (fun a -> check_length tag (Atom.tag a)) ;
        ns |> List.iter (fun a -> check_length tag (Atom.tag a))
      ) ;
    tag, Dnf.mk dnf |> Bdd.of_dnf
  let of_dnf' tag dnf' = of_dnf tag (Dnf'.to_dnf dnf')

  let direct_nodes (_,t) = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f (tag,t) = tag, Bdd.map_nodes (Atom.map_nodes f) t

  let simplify (tag,t) = (tag,Bdd.simplify equiv t)

  let equal (_,t1) (_,t2) = Bdd.equal t1 t2
  let compare (_,t1) (_,t2) = Bdd.compare t1 t2
end

module Make(N:Node) = struct
  module Comp = MakeC(N)
  include Indexed.Make (Comp)
  let mk_comp p = mk p
  let mk a = mk (Comp.mk a)
end
