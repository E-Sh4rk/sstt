open Base
open Sigs
open Sstt_utils

module Atom(N:Node) = struct
  type node = N.t
  type t = Tag.t * node
  let tag (tag,_) = tag
  let map_nodes f (t,n) = t, f n
  let direct_nodes (_,n) = [n]
  let simplify t = t
  let is_empty (_,n) = N.is_empty n
  let equal (t1,n1) (t2,n2) =
    Tag.equal t1 t2 && N.equal n1 n2
  let compare (t1,n1) (t2,n2) =
    Tag.compare t1 t2 |> ccmp
      N.compare n1 n2
  let hash (t, n) = Hash.mix (Tag.hash t) (N.hash n)
end

module MakeC(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)
  module Index = Tag

  type t = Tag.t * Bdd.t
  type node = N.t

  let hash (tag, t) = Hash.mix (Tag.hash tag) (Bdd.hash t)
  let any n = n, Bdd.any
  let empty n = n, Bdd.empty

  let mk a = Atom.tag a, Bdd.singleton a

  let tag (tag,_) = tag
  let index = tag

  let check_tag tag tag' =
    if Tag.equal tag tag' |> not then invalid_arg "Heterogeneous tags."

  let cap (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.cap t1 t2
  let cup (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.cup t1 t2
  let neg (tag, t) = tag, Bdd.neg t
  let diff (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.diff t1 t2

  let ty_of_clause (ps,ns) =
    let p = ps |> List.map snd |> N.conj in
    let n = ns |> List.map snd |> List.map N.neg |> N.conj in
    N.cap p n
  let is_clause_empty (ps,ns,b) =
    not b || ty_of_clause (ps,ns) |> N.is_empty
  let is_empty (_,bdd) = bdd |> Bdd.for_all_lines is_clause_empty

  let leq tag t1 t2 = is_empty (tag, Bdd.diff t1 t2)
  let equiv tag t1 t2 = leq tag t1 t2 && leq tag t2 t1

  let dnf_funs tag =
    let module Comp = struct
      type atom = Atom.t
      let atom_is_valid (t,_) = Tag.equal t tag
      let leq t1 t2 = leq tag (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    end in
    let module Dnf = Dnf.LMake(Comp) in
    Dnf.export, Dnf.import, Dnf.simplify

  let dnf (tag, bdd) =
    let (export,_,simplify) = dnf_funs tag in
    N.with_own_cache (fun bdd -> Bdd.dnf bdd |> export |> simplify) bdd
  let of_dnf tag dnf =
    let (_,import,_) = dnf_funs tag in
    N.with_own_cache (fun dnf -> tag, import dnf |> Bdd.of_dnf) dnf

  let as_atom (tag,t) =
    tag, dnf (tag,t) |> List.map ty_of_clause |> N.disj

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
  include Indexed.Make(Comp)

  let mk_comp p = mk p
  let mk a = mk (Comp.mk a)
end
