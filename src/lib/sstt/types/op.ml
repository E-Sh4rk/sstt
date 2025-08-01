open Core
open Sstt_utils

exception EmptyAtom

module Arrows = struct
  type t = Arrows.t

  let dom t =
    let summand_dom (ps,_,_) = ps |> List.map fst |> Ty.disj in
    Arrows.dnf t |> Arrows.Dnf.simplify |> List.map summand_dom |> Ty.conj

  let apply t s =
    let dnf = Arrows.dnf t |> Arrows.Dnf.simplify in
    dnf |> List.map begin
      fun (ps,_,_) ->
        let rec certain_outputs current_set ps =
          let t = List.map fst current_set |> Ty.disj in
          if Ty.leq s t then [List.map snd current_set |> Ty.disj]
          else begin
            let aux (e,ps) = certain_outputs (e::current_set) ps in
            take_one ps |> List.map aux |> List.flatten
          end
        in
        certain_outputs [] ps |> Ty.conj
    end |> Ty.disj

  let worra t out =
    let dnf = Arrows.dnf t |> Arrows.Dnf.simplify in
    dnf |> List.map begin
      fun (ps,_,_) ->
        let rec impossible_inputs current_set ps =
          let t = List.map snd current_set |> Ty.conj in
          if Ty.leq out (Ty.neg t) then [List.map fst current_set |> Ty.conj]
          else begin
            let aux (e,ps) = impossible_inputs (e::current_set) ps in
            take_one ps |> List.map aux |> List.flatten
          end
        in
        impossible_inputs [] ps |> Ty.disj |> Ty.neg
    end |> Ty.disj
end

module TupleComp = struct
  type t = TupleComp.t
  type atom = TupleComp.Atom.t

  let as_union t =
    TupleComp.dnf' t |> TupleComp.Dnf'.simplify |> List.map fst

  let of_union n lst =
    TupleComp.of_dnf' n (List.map (fun atom -> atom, true) lst)

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
    Records.dnf' t |> Records.Dnf'.simplify |> List.map fst |>
    List.map (fun t ->
        { bindings=t.Records.Atom'.bindings ; opened=t.opened }
      )

  let of_union lst =
    Records.of_dnf (List.map (fun atom -> [atom],[],true) lst)

  let approx t =
    let open Records.Atom in
    let union_a a1 a2 =
      let dom = LabelSet.union (dom a1) (dom a2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
          (lbl, Ty.O.cup (find lbl a1) (find lbl a2))
        ) |> LabelMap.of_list in
      { bindings ; opened = a1.opened || a2.opened }
    in
    match as_union t with
    | [] -> raise EmptyAtom
    | hd::tl -> List.fold_left union_a hd tl

  let proj label t =
    as_union t |> List.map (Records.Atom.find label) |> Ty.O.disj

  let merge a1 a2 =
    let open Records.Atom in
    let dom = LabelSet.union (dom a1) (dom a2) in
    let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
        let oty1, oty2 = find lbl a1, find lbl a2 in
        let oty = if snd oty2 then Ty.O.cup oty1 (fst oty2, false) else oty2 in
        (lbl, oty)
      ) |> LabelMap.of_list in
    { bindings ; opened = a1.opened && a2.opened } |> Records.mk

  let remove a lbl =
    let open Records.Atom in
    let bindings = a.bindings |> LabelMap.add lbl (Ty.O.absent) in
    { a with bindings } |> Records.mk

end
