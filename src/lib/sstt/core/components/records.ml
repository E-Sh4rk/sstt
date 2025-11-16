open Base
open Sigs
open Sstt_utils

module MakeLabelMap
  (FTy:Polymorphic' with type var = RowVar.t and module VarMap = RowVarMap and module VarSet = RowVarSet)
  (N:Node) = Hash.MapList(Label)(FTy)

module Atom
  (FTy:Polymorphic' with type var = RowVar.t and module VarMap = RowVarMap and module VarSet = RowVarSet)
  (N:Node) = struct

  module LabelMap = MakeLabelMap(FTy)(N)
  type node = N.t
  type t = { bindings : LabelMap.t ; tail : FTy.t }

  let hash t =
    Hash.(mix (FTy.hash t.tail) (LabelMap.hash t.bindings))
  let map_nodes f t =
    { bindings = LabelMap.map (FTy.map_nodes f) t.bindings ;
      tail = FTy.map_nodes f t.tail }
  let direct_nodes t =
    let nb = t.bindings |> LabelMap.values |> List.concat_map FTy.direct_nodes in
    let nt = FTy.direct_nodes t.tail in
    nt@nb
  let dom t = LabelMap.dom t.bindings

  let default t f =
    match f with
      Some f -> f
    | None -> t.tail
  let find lbl t = default t (LabelMap.find_opt lbl t.bindings)
  let to_tuple dom t = LabelMap.values_for_domain dom t.tail t.bindings

  module LabelSet = LabelMap.Set
  let substitute s t =
    let tail = FTy.substitute (RowVarMap.map (fun r -> r.tail) s) t.tail in
    let b1 = LabelMap.bindings t.bindings |> List.map (fun (lbl, f) ->
      lbl, FTy.substitute (RowVarMap.map (find lbl) s) f
      ) in
    let l = RowVarMap.bindings s |>
      List.fold_left (fun acc (_,r) -> LabelSet.union acc (dom r)) LabelSet.empty in
    let l = LabelSet.diff l (dom t) in
    let b2 = LabelSet.elements l |> List.map (fun lbl ->
      lbl, FTy.substitute (RowVarMap.map (find lbl) s) t.tail
      ) in
    { bindings = LabelMap.of_list (b1@b2) ; tail }

  let vars_toplevel t =
    t.bindings |> LabelMap.bindings |> List.fold_left (fun acc (_,f) ->
      RowVarSet.union acc (FTy.direct_vars f)) (FTy.direct_vars t.tail)

  let simplify t =
    let is_default = FTy.equiv t.tail in
    let bindings = t.bindings |> LabelMap.filter_map
      (fun _ f -> if is_default f then None else Some (FTy.simplify f)) in
    let tail = FTy.simplify t.tail in
    if bindings == t.bindings && tail == t.tail then t else { bindings ; tail }
    
  let equal t1 t2 =
    FTy.equal t1.tail t2.tail &&
    LabelMap.equal t1.bindings t2.bindings
  let compare t1 t2 =
    FTy.compare t1.tail t2.tail |> ccmp
      LabelMap.compare t1.bindings t2.bindings
end

module Atom'
  (FTy:Polymorphic' with type var = RowVar.t and module VarMap = RowVarMap and module VarSet = RowVarSet)
  (N:Node) = struct

  module LabelMap = MakeLabelMap(FTy)(N)
  module LabelSet = LabelMap.Set
  type node = N.t
  type t = { bindings : LabelMap.t ; tail : FTy.t ;
             exists : (LabelSet.t * FTy.t) list }

  let hash t =
    Hash.(mix3 (FTy.hash t.tail) (LabelMap.hash t.bindings)
    (Hash.list (fun (ls,f) -> Hash.mix
      (LabelSet.elements ls |> Hash.list Label.hash) (FTy.hash f)) t.exists))

  let dom t = LabelMap.dom t.bindings

  let default t f =
    match f with
      Some f -> f
    | None -> t.tail

  let find lbl t = default t (LabelMap.find_opt lbl t.bindings)
  let simplify t =
    let is_default = FTy.equiv t.tail in
    let bindings = t.bindings |> LabelMap.filter_map
      (fun _ f -> if is_default f then None else Some (FTy.simplify f)) in
    let tail = FTy.simplify t.tail in
    let dom = dom t in
    let exists = t.exists
      |> List.filter (fun (ls,f) -> not
        (LabelSet.diff dom ls |> LabelSet.elements |> List.exists
          (fun l -> FTy.leq (find l t) f)
        || FTy.leq t.tail f))
      |> List.map (fun (ls,f) ->
        let ls = ls |> LabelSet.filter (fun l -> FTy.disjoint (find l t) f |> not) in
        (ls, FTy.simplify f)
      )
      |> merge_when_possible (fun (ls1,f1) (ls2,f2) ->
        if LabelSet.subset ls2 ls1 && FTy.leq f1 f2 then Some (ls1,f1)
        else if LabelSet.subset ls1 ls2 && FTy.leq f2 f1
        then Some (ls2,f2) else None)
    in
    if bindings == t.bindings && tail == t.tail && exists == t.exists
    then t else { bindings ; tail ; exists }

  let is_empty t =
    let is_empty_binding _ f = FTy.is_empty f in
    let dom = dom t in
    let is_empty_exist (ls,f) =
      FTy.disjoint t.tail f &&
      LabelSet.diff dom ls |> LabelSet.elements |> List.for_all
        (fun l -> FTy.disjoint (find l t) f)
    in
    t.exists |> List.exists is_empty_exist ||
    LabelMap.exists is_empty_binding t.bindings ||
    FTy.is_empty t.tail

  let equal t1 t2 =
    FTy.equal t1.tail t2.tail &&
    List.equal (fun (ls1,f1) (ls2,f2) -> LabelSet.equal ls1 ls2 && FTy.equal f1 f2)
      t1.exists t2.exists &&
    LabelMap.equal t1.bindings t2.bindings
  let compare t1 t2 =
    FTy.compare t1.tail t2.tail |> ccmp
      (List.compare (fun (ls1,f1) (ls2,f2) ->
        LabelSet.compare ls1 ls2 |> ccmp FTy.compare f1 f2))
      t1.exists t2.exists |> ccmp
        LabelMap.compare t1.bindings t2.bindings
end

module Make(N:Node) = struct
  module FTy = Fields.Make(N)
  module Atom = Atom(FTy)(N)
  module Atom' = Atom'(FTy)(N)

  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)

  type t = Bdd.t
  type node = N.t

  let any = Bdd.any
  let empty = Bdd.empty

  let mk a = Bdd.singleton a

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let conj n ps =
    let init = fun () -> List.init n (fun _ -> FTy.any) in
    mapn init FTy.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> FTy.empty) in
    mapn init FTy.disj ps
  let dnf_line_to_types (ps, ns) =
    let open Atom in
     let line_dom line acc =
      List.fold_left
        (fun acc a -> LabelMap.Set.union acc (dom a))
        acc line
    in
    let dom = line_dom ns (line_dom ps LabelMap.Set.empty) in
    let p = ps |> List.map (Atom.to_tuple dom) |> conj (LabelSet.cardinal dom) in
    let tl = ps |> List.map (fun r -> r.Atom.tail) |> FTy.conj in
    let tests = ns |> List.map (fun r -> r.Atom.tail, Atom.to_tuple dom r) in
    (tl, p), tests

  let rec psi acc ss ts =
    List.exists FTy.is_empty ss ||
    match ts with
    | [] -> false
    | tt::ts ->
      if List.exists2 FTy.disjoint ss tt then psi acc ss ts
      else fold_distribute_comb (fun acc ss -> acc && psi acc ss ts) FTy.diff acc ss tt
  let is_clause_empty (ps,ns,b) =
    if b then
      let (tl,p), ns = dnf_line_to_types (ps, ns) in
      FTy.is_empty tl ||
      let ns = ns |> List.filter_map
        (fun (tl',n) -> if FTy.leq tl tl' then Some n else None) in
      psi true p ns
    else true
  let is_empty t = t |> Bdd.for_all_lines is_clause_empty

  let leq t1 t2 = Bdd.diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module Comp = struct
    type atom = Atom.t
    type atom' = Atom'.t

    let atom_is_valid _ = true
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let any' = { Atom'.bindings=Atom'.LabelMap.empty ; tail=FTy.any ; exists=[] }

    let to_atom a' =
      let open Atom' in
      let ns = a'.exists |> List.map (fun (ls,f) ->
        let bindings = LabelMap.constant ls FTy.any in
        {Atom.bindings=bindings ; Atom.tail=FTy.neg f})
      in
      let ps = [{Atom.bindings=a'.bindings ; Atom.tail=a'.tail}] in
      ps, ns
    let to_atom' (a, b) =
      let open Atom' in
      if b then
        [ { bindings=a.Atom.bindings ; tail=a.Atom.tail ; exists=[] } ]
      else
        let not_binding acc l f =
          { bindings=LabelMap.singleton l (FTy.neg f) ;
            tail=FTy.any ;
            exists=[] } :: acc
        in
        let res = LabelMap.fold not_binding [] a.Atom.bindings in
        let tl = { bindings=LabelMap.empty ; tail=FTy.any ;
                  exists=[Atom.dom a, FTy.neg a.Atom.tail] } in
        tl::res
    let to_atom' (a,b) =
      to_atom' (a,b) |> List.filter (fun a -> Atom'.is_empty a |> not)
      |> List.map Atom'.simplify
    let combine s1 s2 =
      let open Atom' in
      let bindings = LabelMap.merge (fun _ f1 f2 ->
          Some (FTy.cap (default s1 f1) (default s2 f2))
        ) s1.bindings s2.bindings
      in
      let tail = FTy.cap s1.tail s2.tail in
      let exists = s1.exists@s2.exists in
      let res = { bindings ; tail ; exists } in
      if is_empty res then None else Some (simplify res)
  end
  module Dnf = Dnf.LMake'(Comp)
  let dnf t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export |> Dnf.simplify) t
  let dnf' t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export' |> Dnf.simplify') t
  let of_dnf dnf = N.with_own_cache (fun dnf -> Dnf.import dnf |> Bdd.of_dnf) dnf
  let of_dnf' dnf' = N.with_own_cache (fun dnf' -> Dnf.import' dnf' |> Bdd.of_dnf) dnf'

  let direct_nodes t = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f t = Bdd.map_nodes (Atom.map_nodes f) t
  let map f t = Bdd.map_nodes f t
  let direct_row_vars t = Bdd.atoms t |> List.fold_left
    (fun acc a -> RowVarSet.union acc (Atom.vars_toplevel a)) RowVarSet.empty

  let simplify t = Bdd.simplify equiv t
  let substitute s t = Bdd.map_nodes (Atom.substitute s) t

  let equal = Bdd.equal
  let compare = Bdd.compare
  let hash = Bdd.hash
end