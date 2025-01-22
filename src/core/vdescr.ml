open Base
open Sigs

module Atom = struct
  include Var
  let simplify t = t
end

module Make(N:Node) = struct
  module Descr = Descr.Make(N)
  module Bdd = Bdd.Make(Atom)(Descr)

  type t = Bdd.t
  type node = N.t

  let any () = Bdd.any ()
  let empty () = Bdd.empty ()

  let mk_var a = Bdd.singleton a
  let mk_descr d = Bdd.mk_leaf d
  let get_descr t = Bdd.leaves t |> List.fold_left Descr.cup (Descr.empty ())

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let direct_vars t = Bdd.atoms t

  let is_empty t =
    Bdd.leaves t |> List.for_all Descr.is_empty

  let direct_nodes t =
    Bdd.leaves t |> List.map Descr.direct_nodes |> List.concat

  let map_descr f t =
    Bdd.map_leaves f t
  
  let map_nodes f =
    map_descr (Descr.map_nodes f)

  let substitute s t =
    let f v =
      match VarMap.find_opt v s with
      | None -> Bdd.singleton v
      | Some t -> t
    in
    Bdd.substitute f t

  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module DnfAtom = struct
    type leaf = Descr.t
    type t = Var.t
    type t' = NeverAtom.t
    type dnf = (t list * t list * leaf) list
    type dnf' = (t' * leaf) list

    let undesirable_leaf l = Descr.equal l (Descr.empty ())
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let to_s _ = raise (Invalid_argument "Vars cannot be combined.")
    let combine t = match (t:NeverAtom.t) with _ -> .
  end
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf t = Bdd.dnf t |> Dnf.mk
  let of_dnf dnf = Dnf.mk dnf |> Bdd.of_dnf

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
end