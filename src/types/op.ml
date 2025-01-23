open Core

exception EmptyAtom

module Arrows = struct
  type t = Arrows.t

  let dom t =
    let summand_dom (ps,_,_) = ps |> List.map fst |> Ty.disj in
    Arrows.dnf t |> Arrows.Dnf.simplify |> List.map summand_dom |> Ty.conj

  let apply t s =
    if Ty.is_empty s then Ty.empty else
      let dnf = Arrows.dnf t |> Arrows.Dnf.simplify in
      dnf |> List.map begin
        fun (ps,_,_) ->
          let rec certain_outputs current_set ps =
            let t = List.map fst current_set |> Ty.conj in
            if Ty.leq s t then [List.map snd current_set |> Ty.conj]
            else begin
              let aux (e,current_set) = certain_outputs current_set (e::ps) in
              Utils.take_one current_set |> List.map aux |> List.flatten
            end
        in
        certain_outputs ps [] |> Ty.conj
      end |> Ty.disj

  let worra t out =
    if Ty.is_empty out then Ty.empty else
      let dnf = Arrows.dnf t |> Arrows.Dnf.simplify in
      dnf |> List.map begin
        fun (ps,_,_) ->
          let rec impossible_inputs current_set ps =
            let t = List.map snd current_set |> Ty.conj in
            if Ty.leq out (Ty.neg t) then [List.map fst current_set |> Ty.conj]
            else begin
              let aux (e,ps) = impossible_inputs (e::current_set) ps in
              Utils.take_one ps |> List.map aux |> List.flatten
            end
          in
          impossible_inputs [] ps |> Ty.disj |> Ty.neg
      end |> Ty.disj
end

module Products = struct
  type t = Products.t

  let approx t =
    let union = Products.dnf t |> Products.Dnf.combine in
    Utils.mapn (fun _ -> raise EmptyAtom) Ty.disj (List.map fst union)

  let proj i t =
    let union = Products.dnf t |> Products.Dnf.combine in
    union |> List.map fst |> List.map (fun lst -> List.nth lst i) |> Ty.disj

  let merge t1 t2 =
    let n = (Products.len t1) + (Products.len t2) in
    try
      let a1, a2 = approx t1, approx t2 in
      a1@a2 |> Products.mk
    with EmptyAtom -> Products.empty n
end

module Records = struct
  type t = Records.t

  let approx t =
    let open Records.Atom in
    let union = Records.dnf t |> Records.Dnf.combine in
    let union = union |> List.map fst |> List.map (function
      | { Records.Atom'.bindings ; Records.Atom'.kind=Opened }
      | { Records.Atom'.bindings ; Records.Atom'.kind=OpenedStrict _ } ->
        { bindings ; opened=true }
      | { Records.Atom'.bindings ; Records.Atom'.kind=Closed } ->
        { bindings ; opened=false }
      ) in
    let union_a a1 a2 =
      let dom = LabelSet.union (dom a1) (dom a2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
        (lbl, OTy.cup (find lbl a1) (find lbl a2))
      ) |> LabelMap.of_list in
      { bindings ; opened = a1.opened || a2.opened }
    in
    match union with
    | [] -> raise EmptyAtom
    | hd::tl -> List.fold_left union_a hd tl

  let proj label t =
    let union = Records.dnf t |> Records.Dnf.combine in
    union |> List.map fst |> List.map (Records.Atom'.find label)
    |> Records.Atom'.OTy.disj

  let merge t1 t2 =
    let open Records.Atom in
    try
      let a1, a2 = approx t1, approx t2 in
      let dom = LabelSet.union (dom a1) (dom a2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
        let oty1, oty2 = find lbl a1, find lbl a2 in
        let oty = if snd oty2 then OTy.cup oty1 (fst oty2, false) else oty2 in
        (lbl, oty)
      ) |> LabelMap.of_list in
      { bindings ; opened = a1.opened && a2.opened } |> Records.mk
    with EmptyAtom -> Records.empty ()

  let remove t lbl =
    let open Records.Atom in
    try
      let a = approx t in
      let bindings = a.bindings |> LabelMap.add lbl (OTy.absent ()) in
      { a with bindings } |> Records.mk
    with EmptyAtom -> Records.empty ()

end
