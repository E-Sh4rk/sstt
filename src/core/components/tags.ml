open Sigs
open Sstt_utils

module Tag = Id.NamedIdentifier()

module Atom(N:Node) = struct
  module Tag = Tag
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
end

module MakeC(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)
  module Tag = Atom.Tag

  type t = Tag.t * Bdd.t
  type node = N.t

  let any n = n, Bdd.any
  let empty n = n, Bdd.empty

  let mk a = Atom.tag a, Bdd.singleton a

  let tag (tag,_) = tag

  let check_tag tag tag' =
    if Tag.equal tag tag' |> not then
      raise (Invalid_argument "Heterogeneous tags.")

  let cap (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.cap t1 t2
  let cup (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.cup t1 t2
  let neg (tag, t) = tag, Bdd.neg t
  let diff (tag1, t1) (tag2, t2) = check_tag tag1 tag2 ; tag1, Bdd.diff t1 t2

  let ty_of_clause (ps,ns) =
    let p = ps |> List.map snd |> N.conj in
    let n = ns |> List.map snd |> List.map N.neg |> N.conj in
    N.cap p n
  let is_clause_empty (ps,ns,b) =
    if b then ty_of_clause (ps,ns) |> N.is_empty
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
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf (_,t) = Bdd.dnf t |> Dnf.mk
  let of_dnf tag dnf =
    dnf |> List.iter (fun (ps,ns,_) ->
      ps |> List.iter (fun a -> check_tag tag (Atom.tag a)) ;
      ns |> List.iter (fun a -> check_tag tag (Atom.tag a))
      ) ;
    tag, Dnf.mk dnf |> Bdd.of_dnf

  let as_atom (tag,t) =
    let merge_line (ps,ns,_) = ty_of_clause (ps,ns) in
    tag, dnf (tag,t) |> List.map merge_line |> N.disj

  let direct_nodes (_,t) = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f (tag,t) = tag, Bdd.map_nodes (Atom.map_nodes f) t

  let simplify (tag,t) = (tag,Bdd.simplify equiv t)

  let equal (_,t1) (_,t2) = Bdd.equal t1 t2
  let compare (_,t1) (_,t2) = Bdd.compare t1 t2
end

module Make(N:Node) = struct
  module TagComp = MakeC(N)
  include Tagged.Make(TagComp)

  let mk_comp p = mk p
  let mk a = mk (TagComp.mk a)
end
