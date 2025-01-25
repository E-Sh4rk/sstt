open Base
open Sigs
open Sstt_utils

module OTy(N:Node) = struct
  type node = N.t
  type t = node * bool

  let any () = (N.any (), true)
  let empty () = (N.empty (), false)
  let absent () = (N.empty (), true)

  let cap (n1, b1) (n2, b2) =
    (N.cap n1 n2, b1 && b2)
  let cup (n1, b1) (n2, b2) =
    (N.cup n1 n2, b1 || b2)
  let diff (n1, b1) (n2, b2) =
    (N.diff n1 n2, b1 && not b2)
  let neg (n, b) = (N.neg n, not b)
  let conj lst =
    let ns, bs = List.split lst in
    (N.conj ns, List.fold_left (&&) true bs)
  let disj lst =
    let ns, bs = List.split lst in
    (N.disj ns, List.fold_left (||) false bs)

  let is_empty (n,b) = not b && N.is_empty n
  let is_any (n,b) = b && N.is_any n
  let is_absent (n,b) = b && N.is_empty n
  let leq (n1,b1) (n2,b2) = (not b1 || b2) && N.leq n1 n2
  let equiv (n1,b1) (n2,b2) = b1 = b2 && N.equiv n1 n2
  let disjoint (n1,b1) (n2,b2) = not (b1 && b2) && N.disjoint n1 n2

  let equal (n1,b1) (n2,b2) = b1 = b2 && N.equal n1 n2
  let compare (n1,b1) (n2,b2) = compare b1 b2 |> ccmp N.compare n1 n2
end

module Atom(N:Node) = struct
  module Label = Label
  module OTy = OTy(N)

  type node = N.t
  type t = { bindings : OTy.t LabelMap.t ; opened : bool }
  let map f t =
    { t with bindings = LabelMap.map (fun (n,b) -> (f n, b)) t.bindings }
  let nodes t =
    t.bindings |> LabelMap.bindings |> List.map (fun (_,(n,_)) -> n)
  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings, t.opened with
    | Some on, _ -> on
    | None, true -> OTy.any ()
    | None, false -> OTy.absent ()
  let to_tuple dom t =
    let bindings = dom |> List.map (fun l -> find l t) in
    if t.opened then (OTy.any ())::bindings else (OTy.absent ())::bindings
  let simplify t =
    let not_any _ on = OTy.is_any on |> not in
    let not_absent _ on = OTy.is_absent on |> not in
    if t.opened then
      { t with bindings = LabelMap.filter not_any t.bindings }
    else
      { t with bindings = LabelMap.filter not_absent t.bindings }
  let equal t1 t2 =
    t1.opened = t2.opened &&
    LabelMap.equal OTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare t1.opened t2.opened |> ccmp
    (LabelMap.compare OTy.compare) t1.bindings t2.bindings
end

module Atom'(N:Node) = struct
  module Label = Label
  module OTy = OTy(N)

  type node = N.t
  type kind = Opened | Closed | OpenedStrict of LabelSet.t
  type t = { bindings : OTy.t LabelMap.t ; kind : kind }
  let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
  let opened t =
    match t.kind with
    | Closed -> false
    | Opened | OpenedStrict _ -> true
  let find lbl t =
    match LabelMap.find_opt lbl t.bindings with
    | Some on -> on
    | None when opened t -> OTy.any ()
    | None -> OTy.absent ()
  let simplify_bindings t =
    let not_any _ on = OTy.is_any on |> not in
    let not_absent _ on = OTy.is_absent on |> not in
    if opened t then
      { t with bindings = LabelMap.filter not_any t.bindings }
    else
      { t with bindings = LabelMap.filter not_absent t.bindings }
  let simplify_kind t =
    match t.kind with
    | Opened | Closed -> t
    | OpenedStrict lbls ->
      if t.bindings |> LabelMap.exists
        (fun l (_,b) -> LabelSet.mem l lbls |> not && not b)
      then
        { t with kind = Opened }
      else
        let lbls = lbls |> LabelSet.filter (fun l ->
          match LabelMap.find_opt l t.bindings with
          | None -> true
          | Some on -> OTy.is_absent on |> not
        ) in
        { t with kind = OpenedStrict lbls }      
  let is_empty t =
    let is_empty_binding (_,on) = OTy.is_empty on in
    List.exists is_empty_binding (LabelMap.bindings t.bindings)      
  let equal t1 t2 =
    t1.kind = t2.kind &&
    LabelMap.equal OTy.equal t1.bindings t2.bindings
  let compare t1 t2 =
    compare t1.kind t2.kind |> ccmp
    (LabelMap.compare OTy.compare) t1.bindings t2.bindings
end

module Make(N:Node) = struct
  module Atom = Atom(N)
  module Atom' = Atom'(N)

  module ON = Atom.OTy
  module Bdd = Bdd.Make(Atom)(Bdd.BoolLeaf)

  type t = Bdd.t
  type node = N.t

  let any () = Bdd.any ()
  let empty () = Bdd.empty ()

  let mk a = Bdd.singleton a

  let cap = Bdd.cap
  let cup = Bdd.cup
  let neg = Bdd.neg
  let diff = Bdd.diff

  let conj n ps =
    let init = fun () -> List.init n (fun _ -> ON.any ()) in
    mapn init ON.conj ps
  let disj n ps =
    let init = fun () -> List.init n (fun _ -> ON.empty ()) in
    mapn init ON.disj ps

  let rec distribute_diff ss tt =
    match ss, tt with
    | [], [] -> []
    | s::ss, t::tt ->
      let res1 = distribute_diff ss tt
      |> List.map (fun ss -> s::ss) in
      let res2 = (ON.diff s t)::ss in
      res2::res1
    | _, _ -> assert false
  let rec psi n ss ts =
    if List.exists2 ON.leq ss (disj n ts) |> not then false (* optimisation *)
    else match ts with
    | [] -> (* List.exists ON.is_empty ss *) true
    | tt::ts ->
      List.exists ON.is_empty ss || (* optimisation *)
      distribute_diff ss tt |> List.for_all (fun ss -> psi n ss ts)
  let is_clause_empty (ps,ns,b) =
    if b then
      let dom = List.fold_left
        (fun acc a -> LabelSet.union acc (Atom.dom a))
        LabelSet.empty (ps@ns) |> LabelSet.to_list in
      let ps, ns =
        ps |> List.map (Atom.to_tuple dom),
        ns |> List.map (Atom.to_tuple dom) in
      (* We reuse the same algorithm as for tuples *)
      match ps@ns with
      | [] -> false
      | a::_ ->
        let n = List.length a in
        psi n (conj n ps) ns
    else true
  let is_empty t =
    Bdd.dnf t |> List.for_all is_clause_empty

  let leq t1 t2 = Bdd.diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1

  module DnfAtom = struct
    type leaf = bool
    type t = Atom.t
    type t' = Atom'.t
    type dnf = (t list * t list * leaf) list
    type dnf' = (t' * leaf) list

    let undesirable_leaf = not
    let leq t1 t2 = leq (Bdd.of_dnf t1) (Bdd.of_dnf t2)
    let to_t' (a,b) =
      let open Atom' in
      let not_binding (l,on) =
        { bindings = LabelMap.singleton l (ON.neg on) ; kind = Opened }
      in
      if b then
        [ { bindings=a.Atom.bindings ; kind=(if a.Atom.opened then Opened else Closed) } ]
      else
        let res = a.Atom.bindings |> LabelMap.bindings |> List.map not_binding in
        if a.Atom.opened then res
        else
          { bindings = a.Atom.bindings ; kind=OpenedStrict (Atom.dom a) }::res
    let to_t' (a,b) = to_t' (a,b) |> List.filter (fun a -> Atom'.is_empty a |> not)
    let combine s1 s2 =
      let open Atom' in
      let bindings = LabelMap.merge (fun _ b1 b2 ->
        match b1, b2 with
        | None, None -> None
        | Some on, None when s2.kind = Closed -> Some (ON.cap on (ON.absent ()))
        | Some on, None -> Some on
        | None, Some on when s1.kind = Closed -> Some (ON.cap on (ON.absent ()))
        | None, Some on -> Some on
        | Some on1, Some on2 -> Some (ON.cap on1 on2)
      ) s1.bindings s2.bindings in
      let s1 = { s1 with bindings } |> Atom'.simplify_kind in
      let s2 = { s2 with bindings } |> Atom'.simplify_kind in
      let res = match s1.kind, s2.kind with
      | Opened, k | k, Opened -> Some { bindings ; kind=k }
      | Closed, Closed -> Some { bindings ; kind=Closed }
      | Closed, OpenedStrict _ | OpenedStrict _, Closed -> None
      | OpenedStrict ls1, OpenedStrict ls2 ->
        Some { bindings ; kind=OpenedStrict (LabelSet.union ls1 ls2) }
      in
      match res with
      | None -> None
      | Some res when Atom'.is_empty res -> None
      | Some res -> Some (Atom'.simplify_bindings res)
  end
  module Dnf = Dnf.Make(DnfAtom)(N)

  let dnf t = Bdd.dnf t |> Dnf.mk
  let of_dnf dnf = Dnf.mk dnf |> Bdd.of_dnf

  let direct_nodes t = Bdd.atoms t |> List.map Atom.nodes |> List.concat
  let map_nodes f t = Bdd.map_nodes (Atom.map f) t

  let simplify t = Bdd.simplify equiv t

  let equal = Bdd.equal
  let compare = Bdd.compare
end