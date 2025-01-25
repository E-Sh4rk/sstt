open Sigs
open Sstt_utils

module Atom(N:Node) = struct
  type node = N.t
  type t = node list
  let map = List.map
  let nodes t = t
  let simplify t = t
  let is_empty t = List.exists N.is_empty t
  let equal t1 t2 =
    List.equal N.equal t1 t2
  let compare t1 t2 =
    List.compare N.compare t1 t2
end

module MakeT(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)

  type t = int * Bdd.t
  type node = N.t

  let any n = n, Bdd.any ()
  let empty n = n, Bdd.empty ()

  let mk a = List.length a, Bdd.singleton a

  let len (n,_) = n

  let check_len n n' =
    if n <> n' then raise (Invalid_argument "Heterogeneous product lengths.")

  let cap (n, t1) (n', t2) = check_len n n' ; n, Bdd.cap t1 t2
  let cup (n, t1) (n', t2) = check_len n n' ; n, Bdd.cup t1 t2
  let neg (n, t) = n, Bdd.neg t
  let diff (n, t1) (n', t2) = check_len n n' ; n, Bdd.diff t1 t2

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
  let of_dnf n dnf =
    dnf |> List.iter (fun (ps,ns,_) ->
      ps |> List.iter (fun a -> check_len n (List.length a)) ;
      ns |> List.iter (fun a -> check_len n (List.length a))
      ) ;
    n, Dnf.mk dnf |> Bdd.of_dnf
  let of_dnf' n dnf' = of_dnf n (Dnf'.to_dnf dnf')

  let direct_nodes (_,t) = Bdd.atoms t |> List.map Atom.nodes |> List.concat
  let map_nodes f (n,t) = n, Bdd.map_nodes (Atom.map f) t

  let simplify (n,t) = (n,Bdd.simplify equiv t)

  let equal (_,t1) (_,t2) = Bdd.equal t1 t2
  let compare (_,t1) (_,t2) = Bdd.compare t1 t2
end

module Make(N:Node) = struct
  module T = MakeT(N)
  module Products = T
  module IMap = Map.Make(Int)

  type node = N.t
  type t = { map : T.t IMap.t ; others : bool }

  let mk_product a =
    let n = List.length a in
    { map = IMap.singleton n (T.mk a) ; others = false }
  let mk_products p =
    let n = T.len p in
    { map = IMap.singleton n p ; others = false }
  let mk (ps, others) =
    let map = ps |> List.map (fun p -> (T.len p, p)) |> IMap.of_list in
    { map ; others }
  let any () = { map = IMap.empty ; others = true }
  let empty () = { map = IMap.empty ; others = false }

  let cap t1 t2 =
    let others = t1.others && t2.others in
    let map = IMap.merge (fun _ o1 o2 ->
      match o1, o2 with
      | None, None -> None
      | Some t1, None -> if t2.others then Some t1 else None
      | None, Some t2 -> if t1.others then Some t2 else None
      | Some t1, Some t2 -> Some (T.cap t1 t2)
    ) t1.map t2.map in
    { map ; others }
  let cup t1 t2 =
    let others = t1.others || t2.others in
    let map = IMap.merge (fun _ o1 o2 ->
      match o1, o2 with
      | None, None -> None
      | Some t1, None -> if t2.others then None else Some t1
      | None, Some t2 -> if t1.others then None else Some t2
      | Some t1, Some t2 -> Some (T.cup t1 t2)
    ) t1.map t2.map in
    { map ; others }
  let neg t =
    let others = not t.others in
    let map = IMap.map T.neg t.map in
    { map ; others }
  let diff t1 t2 =
    let others = t1.others && not t2.others in
    let map = IMap.merge (fun _ o1 o2 ->
      match o1, o2 with
      | None, None -> None
      | Some t1, None -> if not t2.others then Some t1 else None
      | None, Some t2 -> if t1.others then Some (T.neg t2) else None
      | Some t1, Some t2 -> Some (T.diff t1 t2)
    ) t1.map t2.map in
    { map ; others }

  let is_empty t =
    not t.others &&
    IMap.for_all (fun _ a -> T.is_empty a) t.map

  let direct_nodes t = t.map |> IMap.bindings |>
    List.map (fun (_,t) -> T.direct_nodes t) |>
    List.concat

  let map_nodes f t =
    let map = IMap.map (Products.map_nodes f) t.map in
    { map ; others=t.others }

  let simplify t =
    let t_is_empty t = T.is_empty t in
    let t_is_any t = T.neg t |> T.is_empty in
    let map = IMap.map T.simplify t.map in
    let p = if t.others then t_is_any else t_is_empty in
    let map = IMap.filter (fun _ t -> p t |> not) map in
    { map ; others = t.others }

  let components t =
    let prods = IMap.bindings t.map |> List.map snd in
    (prods, t.others)

  let get i t =
    match IMap.find_opt i t.map with
    | Some prod -> prod
    | None when t.others -> Products.any i
    | None -> Products.empty i

  let map f t =
    let map = IMap.map f t.map in
    { t with map }

  let equal t1 t2 =
    t1.others = t2.others &&
    IMap.equal T.equal t1.map t2.map
  let compare t1 t2 =
    compare t1.others t2.others |> ccmp
    (IMap.compare T.compare) t1.map t2.map
end