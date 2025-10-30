open Base
open Sigs
open Sstt_utils

module OTy(N:Node) = struct
  type node = N.t
  type t = node * bool

  let any = (N.any, true)
  let empty = (N.empty, false)
  let absent = (N.empty, true)
  let required t = (t, false)
  let optional t = (t, true)
  let get (t,_) = t

  let cap (n1, b1) (n2, b2) = (N.cap n1 n2, b1 && b2)
  let cap = fcap ~empty ~any ~cap
  let cup (n1, b1) (n2, b2) = (N.cup n1 n2, b1 || b2)
  let cup = fcup ~empty ~any ~cup
  let diff (n1, b1) (n2, b2) = (N.diff n1 n2, b1 && not b2)
  let diff = fdiff ~empty ~any ~diff
  let neg (n, b) = (N.neg n, not b)
  let neg = fneg ~empty ~any ~neg
  let conj lst =
    let ns, bs = List.split lst in
    (N.conj ns, List.fold_left (&&) true bs)
  let disj lst =
    let ns, bs = List.split lst in
    (N.disj ns, List.fold_left (||) false bs)

  let is_empty (n,b) = not b && N.is_empty n
  let is_any (n,b) = b && N.is_any n
  let is_absent (n,b) = b && N.is_empty n
  let is_optional (_,b) = b
  let is_required (_,b) = not b
  let leq (n1,b1) (n2,b2) = (not b1 || b2) && N.leq n1 n2
  let equiv (n1,b1) (n2,b2) = b1 = b2 && N.equiv n1 n2
  let disjoint (n1,b1) (n2,b2) = not (b1 && b2) && N.disjoint n1 n2

  let equal (n1,b1) (n2,b2) = b1 = b2 && N.equal n1 n2
  let compare (n1,b1) (n2,b2) = compare b1 b2 |> ccmp N.compare n1 n2

  let map_nodes f (n,b) = (f n, b)

  let hash (n, b) = Hash.(mix (bool b) (N.hash n))
end

module Atom(N:Node) = struct
  module OTy = OTy(N)

  type node = N.t
  type nonrec oty = node * bool
  type t = { bindings : oty LabelMap.t ; opened : bool }

  let hash t = (* This hashing is not incremental and could hurt performances
                  if we make heavy use of records (t.bindings is traversed by
                  the polymorphic Hash function) *)
    Hash.(mix (bool t.opened) (Hashtbl.hash t.bindings))
  let map_nodes f t =
    { t with bindings = LabelMap.map (OTy.map_nodes f) t.bindings }

  let direct_nodes t =
    t.bindings |> LabelMap.bindings |> List.map (fun (_,(n,_)) -> n)
  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings, t.opened with
    | Some on, _ -> on
    | None, true -> OTy.any
    | None, false -> OTy.absent
  let to_tuple dom t = dom |> List.map (fun l -> find l t)
  let to_tuple_with_default dom t =
    if t.opened then
      OTy.any::(to_tuple dom t)
    else
      OTy.absent::(to_tuple dom t)
  let simplify t =
    let is_default =
      if t.opened then OTy.is_any
      else OTy.is_absent
    in
    let bindings = LabelMap.filter (fun _ on -> not (is_default on)) t.bindings in
    if bindings == t.bindings then t else { t with bindings }
  let equal t1 t2 =
    t1.opened = t2.opened &&
    LabelMap.equal OTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare t1.opened t2.opened |> ccmp
      (LabelMap.compare OTy.compare) t1.bindings t2.bindings

  let tname = "Records.dAtom"
end

module Atom'(N:Node) = struct
  module OTy = OTy(N)

  type node = N.t
  type nonrec oty = node * bool
  type t = { bindings : oty LabelMap.t ; opened : bool ; required : LabelSet.t option }
  let hash t = (* Same remark as OTY.hash *)
    Hash.(mix3 (bool t.opened) (Hashtbl.hash t.bindings) (Hashtbl.hash t.required))

  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings with
    | Some on -> on
    | None when t.opened -> OTy.any
    | None -> OTy.absent
  let simplify t =
    let is_default =
      if t.opened then OTy.is_any
      else OTy.is_absent
    in
    let bindings = LabelMap.filter (fun _ on -> not (is_default on)) t.bindings in

    let required =
      match t.required with
      | None -> None
      | Some lbls ->
        if bindings |> LabelMap.exists (fun l on -> LabelSet.mem l lbls |> not && OTy.is_required on)
        then None
        else Some (lbls |> LabelSet.filter (fun l -> find l t |> OTy.is_absent |> not))
    in
    if bindings == t.bindings && required == t.required then t else
      { t with bindings ; required }
  let is_empty t =
    let is_empty_binding (_,on) = OTy.is_empty on in
    let required_ok =
      match t.required with
      | None -> true
      | Some _ when t.opened -> true
      | Some req ->
        t.bindings |> LabelMap.exists
          (fun l o -> LabelSet.mem l req |> not && OTy.is_absent o |> not)
    in
    not required_ok ||
    LabelMap.bindings t.bindings |> List.exists is_empty_binding
  let equal t1 t2 =
    t1.opened = t2.opened &&
    Option.equal LabelSet.equal t1.required t2.required &&
    LabelMap.equal OTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare t1.opened t2.opened |> ccmp
      (Option.compare LabelSet.compare) t1.required t2.required |> ccmp
      (LabelMap.compare OTy.compare) t1.bindings t2.bindings
end

module Make(N:Node) = struct
  module Atom = Atom(N)
  module Atom' = Atom'(N)

  module ON = OTy(N)
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
    let init = fun () -> List.init n (fun _ -> ON.any) in
    mapn init ON.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> ON.empty) in
    mapn init ON.disj ps
  let dnf_line_to_tuple (ps, ns) =
    let dom = List.fold_left
        (fun acc a -> LabelSet.union acc (Atom.dom a))
        LabelSet.empty (ps@ns) |> LabelSet.to_list
    in
    let ps, ns =
      ps |> List.map (Atom.to_tuple_with_default dom),
      ns |> List.map (Atom.to_tuple_with_default dom) in
    (ps, ns), List.length dom + 1

  let rec psi acc ss ts =
    List.exists ON.is_empty ss ||
    match ts with
    | [] -> false
    | tt::ts ->
      fold_distribute_comb (fun acc ss -> acc && psi acc ss ts) ON.diff acc ss tt
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
    let any' = { Atom'.bindings=LabelMap.empty ; opened=true ; required=None }

    let to_atom a' =
      let open Atom' in
      let ns =
        match a'.required with
        | None -> []
        | Some lbls ->
          let bindings =
            lbls |> LabelSet.elements |> List.map (fun l -> (l,ON.any))
            |> LabelMap.of_list
          in
          [{Atom.bindings=bindings ; Atom.opened=false}]
      in
      let ps = [{Atom.bindings=a'.bindings ; Atom.opened=a'.opened}] in
      ps, ns
    let to_atom' (a,b) =
      let open Atom' in
      let not_binding (l,on) =
        { bindings=LabelMap.singleton l (ON.neg on) ; opened=true ; required=None }
      in
      if b then
        [ { bindings=a.Atom.bindings ; opened=a.Atom.opened ; required=None } ]
      else
        let res = a.Atom.bindings |> LabelMap.bindings |> List.map not_binding in
        if a.Atom.opened then res
        else { bindings=a.Atom.bindings ; opened=true ; required=Some (Atom.dom a) }::res
    let to_atom' (a,b) =
      to_atom' (a,b) |> List.filter (fun a -> Atom'.is_empty a |> not)
      |> List.map Atom'.simplify
    let combine s1 s2 =
      let open Atom' in
      let dom = LabelSet.union (dom s1) (dom s2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
          (lbl, ON.cap (find lbl s1) (find lbl s2))
        ) |> LabelMap.of_list in
      let opened = s1.opened && s2.opened in
      let required =
        match s1.required, s2.required with
        | None, None -> None
        | Some r, None | None, Some r -> Some r
        | Some r1, Some r2 -> Some (LabelSet.union r1 r2)
      in
      let res = { bindings ; opened ; required } in
      if is_empty res then None else Some (simplify res)
  end
  module Dnf = Dnf.LMake'(Comp)
  let dnf t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export |> Dnf.simplify) t
  let dnf' t = N.with_own_cache (fun t -> Bdd.dnf t |> Dnf.export' |> Dnf.simplify') t
  let of_dnf dnf = N.with_own_cache (fun dnf -> Dnf.import dnf |> Bdd.of_dnf) dnf
  let of_dnf' dnf' = N.with_own_cache (fun dnf' -> Dnf.import' dnf' |> Bdd.of_dnf) dnf'

  let direct_nodes t = Bdd.atoms t |> List.concat_map Atom.direct_nodes
  let map_nodes f t = Bdd.map_nodes (Atom.map_nodes f) t

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
  let hash = Bdd.hash
end