open Core
open Sstt_utils

exception EmptyAtom

module Arrows = struct
  type t = Arrows.t

  let dom t =
    let summand_dom (ps,_) = ps |> List.map fst |> Ty.disj in
    Arrows.dnf t |> List.map summand_dom |> Ty.conj

  let apply t s =
    Arrows.dnf t |> List.map begin
      fun (ps,_) ->
        let rec certain_outputs current_set ps =
          let t = List.map fst current_set |> Ty.disj in
          if Ty.leq s t then [List.map snd current_set |> Ty.disj]
          else begin
            let aux (e,ps) = certain_outputs (e::current_set) ps in
            take_one ps |> List.concat_map aux
          end
        in
        certain_outputs [] ps |> Ty.conj
    end |> Ty.disj

  let worra t out =
    Arrows.dnf t |> List.map begin
      fun (ps,_) ->
        let rec impossible_inputs current_set ps =
          let t = List.map snd current_set |> Ty.conj in
          if Ty.leq out (Ty.neg t) then [List.map fst current_set |> Ty.conj]
          else begin
            let aux (e,ps) = impossible_inputs (e::current_set) ps in
            take_one ps |> List.concat_map aux
          end
        in
        impossible_inputs [] ps |> Ty.disj |> Ty.neg
    end |> Ty.disj
end

module TupleComp = struct
  type t = TupleComp.t
  type atom = TupleComp.Atom.t

  let as_union t = TupleComp.dnf' t

  let of_union n lst = TupleComp.of_dnf' n lst

  let approx t =
    mapn (fun _ -> raise EmptyAtom) Ty.disj (as_union t)

  let proj i t =
    as_union t |> List.map (fun lst -> 
        match List.nth_opt lst i with
          Some v -> v
        | None -> invalid_arg "Op.TupleComp.proj") |> Ty.disj

  let merge a1 a2 = a1@a2
end

module Records = struct
  type t = Records.t
  type atom = Records.Atom.t

  let as_union t =
    let open Records.Atom in
    Records.dnf' t |> List.map (fun t ->
        { bindings=t.Records.Atom'.bindings ; opened=t.opened }
      )

  let of_union lst =
    Records.of_dnf (List.map (fun atom -> [atom],[]) lst)

  let approx t =
    let open Records.Atom in
    let union_a a1 a2 =
      let dom = LabelSet.union (dom a1) (dom a2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
          (lbl, Ty.F.cup (find lbl a1) (find lbl a2))
        ) |> LabelMap.of_list in
      { bindings ; opened = a1.opened || a2.opened }
    in
    match as_union t with
    | [] -> raise EmptyAtom
    | hd::tl -> List.fold_left union_a hd tl

  let proj label t =
    as_union t |> List.map (Records.Atom.find label) |> Ty.F.disj

  let merge a1 a2 =
    let open Records.Atom in
    let dom = LabelSet.union (dom a1) (dom a2) in
    let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
        let oty1, oty2 = find lbl a1, find lbl a2 in
        let ty2, absty2 = Ty.F.destruct oty2 in
        let oty = if absty2 then Ty.F.(cup oty1 (required ty2)) else oty2 in
        (lbl, oty)
      ) |> LabelMap.of_list in
    { bindings ; opened = a1.opened || a2.opened } |> Records.mk

  let remove a lbl =
    let open Records.Atom in
    let bindings = a.bindings |> LabelMap.add lbl (Ty.F.absent) in
    { a with bindings } |> Records.mk

end

module TagComp = struct
  type t = TagComp.t
  type atom = TagComp.Atom.t

  let is_identity t =
    let p = TagComp.tag t |> Tag.properties in
    match p with
    | Tag.NoProperty -> false
    | Tag.Monotonic m -> m.preserves_cap && m.preserves_cup

  let as_atom t =
    if is_identity t |> not then
      invalid_arg "Tag component must satisfy is_identity." ;
    let ty_of_clause (ps,ns) =
      let p = ps |> List.map snd |> Ty.conj in
      let n = ns |> List.map snd |> List.map Ty.neg |> Ty.conj in
      Ty.cap p n
    in
    TagComp.tag t, TagComp.dnf t |> List.map ty_of_clause |> Ty.disj
end
