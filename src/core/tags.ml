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

(* TODO: factorize with Tuples.MakeC *)

module MakeC(N:Node) = struct
  module Atom = Atom(N)
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)
  module Tag = Atom.Tag

  type t = Tag.t * Bdd.t
  type node = N.t

  let any n = n, Bdd.any ()
  let empty n = n, Bdd.empty ()

  let mk a = Atom.tag a, Bdd.singleton a

  let tag (n,_) = n

  let check_tag n n' =
    if Tag.equal n n' |> not then
      raise (Invalid_argument "Heterogeneous tags.")

  let cap (n, t1) (n', t2) = check_tag n n' ; n, Bdd.cap t1 t2
  let cup (n, t1) (n', t2) = check_tag n n' ; n, Bdd.cup t1 t2
  let neg (n, t) = n, Bdd.neg t
  let diff (n, t1) (n', t2) = check_tag n n' ; n, Bdd.diff t1 t2

  let is_clause_empty (ps,ns,b) =
    if b then
      let p = ps |> List.map snd |> N.conj in
      let n = ns |> List.map snd |> List.map N.neg |> N.conj in
      N.cap p n |> N.is_empty
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
    let to_t' ((tag,n),b) =
      if b then [(tag,n)] else [(tag,N.neg n)]
    let to_t' (a,b) = to_t' (a,b) |> List.filter (fun a -> Atom.is_empty a |> not)
    let combine (tag1,n1) (tag2,n2) =
      check_tag tag1 tag2 ;
      let res = (tag1, N.cap n1 n2) in
      if Atom.is_empty res then None else Some res
  end
  module Dnf' = Dnf.Make'(DnfAtom)(DnfAtom')(N)
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf (_,t) = Bdd.dnf t |> Dnf.mk
  let dnf' (n,t) = dnf (n,t) |> Dnf'.from_dnf (n,N.any ())
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
  module TagComp = MakeC(N)
  include Tagcomp.Make(N)(TagComp)

  let mk_tag a = mk (TagComp.mk a)
  let mk_tagcomp p = mk p
end
