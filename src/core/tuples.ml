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
  module Tag = Int

  type t = int * Bdd.t
  type node = N.t

  let any n = n, Bdd.any ()
  let empty n = n, Bdd.empty ()

  let mk a = Atom.tag a, Bdd.singleton a

  let tag (n,_) = n
  let len = tag

  let check_tag n n' =
    if Tag.equal n n' |> not then
      raise (Invalid_argument "Heterogeneous product lengths.")

  let cap (n, t1) (n', t2) = check_tag n n' ; n, Bdd.cap t1 t2
  let cup (n, t1) (n', t2) = check_tag n n' ; n, Bdd.cup t1 t2
  let neg (n, t) = n, Bdd.neg t
  let diff (n, t1) (n', t2) = check_tag n n' ; n, Bdd.diff t1 t2

  let conj n ps =
    let init = fun () -> List.init n (fun _ -> N.any ()) in
    mapn init N.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> N.empty ()) in
    mapn init N.disj ps

  let rec distribute_diff ss tt =
    match ss, tt with
    | [], [] -> []
    | s::ss, t::tt ->
      let res1 = distribute_diff ss tt
      |> List.map (fun ss -> s::ss) in
      let res2 = (N.diff s t)::ss in
      res2::res1
    | _, _ -> assert false
  let rec psi n ss ts =
    if List.exists2 N.leq ss (disj n ts) |> not then false (* optimisation *)
    else match ts with
    | [] -> (* List.exists N.is_empty ss *) true
    | tt::ts ->
      List.exists N.is_empty ss || (* optimisation *)
      distribute_diff ss tt |> List.for_all (fun ss -> psi n ss ts)
  let is_clause_empty (ps,ns,b) =
    if b then
      match ps@ns with
      | [] -> false
      | a::_ ->
        let n = List.length a in
        psi n (conj n ps) ns
    else true
  let is_empty' t =
    Bdd.dnf t |> List.for_all is_clause_empty
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
      let rec aux ns =
        match ns with
        | [] -> []
        | n::ns -> ((N.neg n)::ns)::(aux ns |> List.map (fun s -> n::s))
      in
      if b then [ns] else aux ns
    let to_t' (a,b) = to_t' (a,b) |> List.filter (fun a -> Atom.is_empty a |> not)
    let combine ns1 ns2 =
      let res = List.map2 N.cap ns1 ns2 in
      if Atom.is_empty res then None else Some res
  end
  module Dnf' = Dnf.Make'(DnfAtom)(DnfAtom')(N)
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf (_,t) = Bdd.dnf t |> Dnf.mk
  let dnf' (n,t) = dnf (n,t) |> Dnf'.from_dnf (List.init n (fun _ -> N.any ()))
  let of_dnf tag dnf =
    dnf |> List.iter (fun (ps,ns,_) ->
      ps |> List.iter (fun a -> check_tag tag (Atom.tag a)) ;
      ns |> List.iter (fun a -> check_tag tag (Atom.tag a))
      ) ;
    tag, Dnf.mk dnf |> Bdd.of_dnf
  let of_dnf' tag dnf' = of_dnf tag (Dnf'.to_dnf dnf')

  let direct_nodes (_,t) = Bdd.atoms t |> List.map Atom.direct_nodes |> List.concat
  let map_nodes f (n,t) = n, Bdd.map_nodes (Atom.map_nodes f) t

  let simplify (n,t) = (n,Bdd.simplify equiv t)

  let equal (_,t1) (_,t2) = Bdd.equal t1 t2
  let compare (_,t1) (_,t2) = Bdd.compare t1 t2
end

module Make(N:Node) = struct
  module Products = MakeC(N)
  include Tagcomp.Make(N)(Products)

  let mk_product a = mk (Products.mk a)
  let mk_products p = mk p
end
