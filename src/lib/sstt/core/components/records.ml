open Base
open Sigs
open Sstt_utils

module Atom
  (FTy:Polymorphic' with type var = RowVar.t and module VarMap = RowVarMap and module VarSet = RowVarSet)
  (N:Node) = struct

  type node = N.t
  type t = { bindings : FTy.t LabelMap.t ; tail : FTy.t }

  let hash t = (* This hashing is not incremental and could hurt performances
                  if we make heavy use of records (t.bindings is traversed by
                  the polymorphic Hash function) *)
    Hash.mix (FTy.hash t.tail) (Hashtbl.hash t.bindings)
  let map_nodes f t =
    { bindings = LabelMap.map (FTy.map_nodes f) t.bindings ;
      tail = FTy.map_nodes f t.tail }
  let direct_nodes t =
    let nb = t.bindings |> LabelMap.bindings
    |> List.map snd |> List.concat_map FTy.direct_nodes
    in
    let nt = FTy.direct_nodes t.tail in
    nt@nb
  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings with
    | Some f -> f
    | None -> t.tail
  let to_tuple dom t = dom |> List.map (fun l -> find l t)
  let to_tuple_with_tail dom t =
    (t.tail)::(to_tuple dom t)

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
    LabelMap.equal FTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    FTy.compare t1.tail t2.tail |> ccmp
      (LabelMap.compare FTy.compare) t1.bindings t2.bindings
end

module Atom'
  (FTy:Polymorphic' with type var = RowVar.t and module VarMap = RowVarMap and module VarSet = RowVarSet)
  (N:Node) = struct

  type node = N.t
  type t = { bindings : FTy.t LabelMap.t ; tail : FTy.t ;
             exists : (LabelSet.t * FTy.t) list }
  let hash t =
    Hash.mix3 (FTy.hash t.tail) (Hashtbl.hash t.bindings)
    (Hash.list (fun (ls,f) -> Hash.mix
      (LabelSet.elements ls |> Hash.list Label.hash) (FTy.hash f)) t.exists)

  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings with
    | Some f -> f
    | None -> t.tail
  let simplify t =
    let is_default = FTy.equiv t.tail in
    let bindings = t.bindings |> LabelMap.filter_map
      (fun _ f -> if is_default f then None else Some (FTy.simplify f)) in
    let tail = FTy.simplify t.tail in
    let dom = dom t in
    let exists = t.exists
      |> List.filter (fun (ls,f) -> not
        (LabelSet.diff dom ls |> LabelSet.exists (fun l -> FTy.leq (find l t) f)
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
    let is_empty_binding (_,f) = FTy.is_empty f in
    let dom = dom t in
    let is_empty_exist (ls,f) =
      FTy.disjoint t.tail f &&
      LabelSet.diff dom ls |> LabelSet.for_all (fun l -> FTy.disjoint (find l t) f)
    in
    t.exists |> List.exists is_empty_exist ||
    LabelMap.bindings t.bindings |> List.exists is_empty_binding ||
    FTy.is_empty t.tail

  let equal t1 t2 =
    FTy.equal t1.tail t2.tail &&
    List.equal (fun (ls1,f1) (ls2,f2) -> LabelSet.equal ls1 ls2 && FTy.equal f1 f2)
      t1.exists t2.exists &&
    LabelMap.equal FTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    FTy.compare t1.tail t2.tail |> ccmp
      (List.compare (fun (ls1,f1) (ls2,f2) ->
        LabelSet.compare ls1 ls2 |> ccmp FTy.compare f1 f2))
      t1.exists t2.exists |> ccmp
        (LabelMap.compare FTy.compare) t1.bindings t2.bindings
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
  let dnf_line_to_tuple (ps, ns) =
    let dom = List.fold_left
        (fun acc a -> LabelSet.union acc (Atom.dom a))
        LabelSet.empty (ps@ns) |> LabelSet.to_list
    in
    let ps, ns =
      ps |> List.map (Atom.to_tuple_with_tail dom),
      ns |> List.map (Atom.to_tuple_with_tail dom) in
    (ps, ns), List.length dom + 1

  let rec psi acc ss ts =
    List.exists FTy.is_empty ss ||
    match ts with
    | [] -> false
    | tt::ts ->
      fold_distribute_comb (fun acc ss -> acc && psi acc ss ts) FTy.diff acc ss tt
  let is_clause_empty (ps,ns,b) =
    if b then
      let (ps, ns), n = dnf_line_to_tuple (ps, ns) in
      psi true (conj n ps) ns
    else true
  let is_empty t = t |> Bdd.for_all_lines is_clause_empty

  let leq t1 t2 = Bdd.diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module Comp = struct
    type atom = Atom.t
    type atom' = Atom'.t

    let atom_is_valid _ = true
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let any' = { Atom'.bindings=LabelMap.empty ; tail=FTy.any ; exists=[] }

    let to_atom a' =
      let open Atom' in
      let ns = a'.exists |> List.map (fun (ls,f) ->
        let bindings = LabelSet.elements ls |>
          List.map (fun l -> l, FTy.any) |> LabelMap.of_list in
        {Atom.bindings=bindings ; Atom.tail=FTy.neg f})
      in
      let ps = [{Atom.bindings=a'.bindings ; Atom.tail=a'.tail}] in
      ps, ns
    let to_atom' (a,b) =
      let open Atom' in
      let not_binding (l,f) =
        { bindings=LabelMap.singleton l (FTy.neg f) ; tail=FTy.any ; exists=[] }
      in
      if b then
        [ { bindings=a.Atom.bindings ; tail=a.Atom.tail ; exists=[] } ]
      else
        let res = a.Atom.bindings |> LabelMap.bindings |> List.map not_binding in
        let tl = { bindings=LabelMap.empty ; tail=FTy.any ;
                  exists=[Atom.dom a, FTy.neg a.Atom.tail] } in
        tl::res
    let to_atom' (a,b) =
      to_atom' (a,b) |> List.filter (fun a -> Atom'.is_empty a |> not)
      |> List.map Atom'.simplify
    let combine s1 s2 =
      let open Atom' in
      let dom = LabelSet.union (dom s1) (dom s2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
          (lbl, FTy.cap (find lbl s1) (find lbl s2))
        ) |> LabelMap.of_list in
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