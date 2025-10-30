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

  let hash = Hash.list N.hash
  let tname = "Tuples.Atom"
end

module MakeC(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)
  module Index = struct
    include Int
    let tname = "Int"
  end

  type t = int * Bdd.t

  let tname = "TupleComp"

  type node = N.t
  let hash (len, t) = Hash.mix (Hash.int len) (Bdd.hash t)

  let any n = n, Bdd.any
  let empty n = n, Bdd.empty

  let mk a = Atom.tag a, Bdd.singleton a

  let index (tag,_) = tag
  let len = index

  let check_length len len' =
    if Index.equal len len' |> not then
      invalid_arg "Heterogeneous tuple lengths."

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

  let rec psi acc ss ts =
    List.exists N.is_empty ss ||
    match ts with
    | [] -> false
    | tt::ts ->
      fold_distribute_comb (fun acc ss -> acc && psi acc ss ts) N.diff acc ss tt
  let is_clause_empty n (ps,ns,b) =
    not b || psi true (conj n ps) ns
  let is_empty (n,t) = Bdd.for_all_lines (is_clause_empty n) t

  let leq n t1 t2 = is_empty (n, Bdd.diff t1 t2)
  let equiv n t1 t2 = leq n t1 t2 && leq n t2 t1

  let dnf_funs n =
    let module Comp = struct
      type atom = Atom.t
      type atom' = Atom.t

      let atom_is_valid lst = List.length lst = n
      let leq t1 t2 = leq n (Bdd.of_dnf t1) (Bdd.of_dnf t2)
      let any' = List.init n (fun _ -> N.any)

      let to_atom a = [a], []
      let to_atom' (ns,b) =
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
      let to_atom' (a,b) = to_atom' (a,b) |> List.filter (fun a -> Atom.is_empty a |> not)
      let combine ns1 ns2 =
        let res = List.map2 N.cap ns1 ns2 in
        if Atom.is_empty res then None else Some res
    end in
    let module Dnf = Dnf.LMake'(Comp) in
    Dnf.export, Dnf.import, Dnf.simplify, Dnf.export', Dnf.import', Dnf.simplify'

  let dnf (n, bdd) =
    let (export,_,simplify,_,_,_) = dnf_funs n in
    N.with_own_cache (fun bdd -> Bdd.dnf bdd |> export |> simplify) bdd
  let of_dnf n dnf =
    let (_,import,_,_,_,_) = dnf_funs n in
    N.with_own_cache (fun dnf -> n, import dnf |> Bdd.of_dnf) dnf
  let dnf' (n, bdd) =
    let (_,_,_,export',_,simplify') = dnf_funs n in
    N.with_own_cache (fun bdd -> Bdd.dnf bdd |> export' |> simplify') bdd
  let of_dnf' n dnf =
    let (_,_,_,_,import',_) = dnf_funs n in
    N.with_own_cache (fun dnf -> n, import' dnf |> Bdd.of_dnf) dnf

  let direct_nodes (_,t) = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f (tag,t) = tag, Bdd.map_nodes (Atom.map_nodes f) t

  let simplify ((tag,t) as n) =
    let t' = Bdd.simplify (equiv tag) t in
    if t == t' then n else (tag, t')

  let equal (_,t1) (_,t2) = Bdd.equal t1 t2
  let compare (_,t1) (_,t2) = Bdd.compare t1 t2
end

module Make(N:Node) = struct
  module Comp = MakeC(N)
  include Indexed.Make (Comp)
  let mk_comp p = mk p
  let mk a = mk (Comp.mk a)
end
