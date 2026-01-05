open Core
open Sstt_utils

exception EmptyAtom

module Arrows = struct
  type t = Arrows.t

  let dom t =
    let summand_dom (ps,_) = ps |> List.map fst |> Ty.disj in
    Arrows.dnf t |> List.map summand_dom |> Ty.conj

  let apply t s =
    let not_disjoint (d,_) = Ty.disjoint d s |> not in
    let not_redundant (d,_) (d',_) = Ty.leq (Ty.cap d s) (Ty.cap d' s) |> not in
    Arrows.dnf t |> List.map begin
      fun (ps,_) ->
        let rec certain_outputs current_set ps =
          let t = List.map fst current_set |> Ty.disj in
          if Ty.leq s t then [List.map snd current_set |> Ty.disj]
          else if Ty.leq s (t::(List.map fst ps) |> Ty.disj) |> not then []
          else begin match ps with
            | [] -> []
            | p::ps ->
              (certain_outputs (p::current_set) (List.filter (not_redundant p) ps))@
              (certain_outputs current_set ps)
          end
        in
        ps |> List.filter not_disjoint |> certain_outputs [] |> Ty.conj
    end |> Ty.disj

  let worra t out =
    let not_useless (_,c) = Ty.disjoint out (Ty.neg c) |> not in
    let not_redundant (_,c) (_,c') = Ty.leq (Ty.diff out c) (Ty.diff out c') |> not in
    Arrows.dnf t |> List.map begin
      fun (ps,_) ->
        let rec impossible_inputs current_set ps =
          let t = List.map snd current_set |> Ty.conj in
          if Ty.disjoint out t then [List.map fst current_set |> Ty.conj]
          else if Ty.disjoint out (t::(List.map snd ps) |> Ty.conj) |> not then []
          else begin match ps with
            | [] -> []
            | p::ps ->
              (impossible_inputs (p::current_set) (List.filter (not_redundant p) ps))@
              (impossible_inputs current_set ps)
          end
        in
        ps |> List.filter not_useless |> impossible_inputs [] |> Ty.disj |> Ty.neg
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

module Records' = struct
  module Atom = struct
    module LabelMap = Map.Make(Label)
    type t = { bindings : Ty.F.t LabelMap.t ; tail : Ty.F.t }
    let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
    let find lbl t =
      match LabelMap.find_opt lbl t.bindings with
      | Some f -> f
      | None -> t.tail
  end

  type t = Records.t
  type atom = Atom.t

  let as_union t =
    Records.dnf' t |> List.map (fun t ->
        let bindings = t.Records.Atom'.bindings |> LabelMap.bindings
        |> List.map (fun (lbl,f) -> lbl, f) |> Atom.LabelMap.of_list in
        { Atom.bindings; tail=t.tail }
      )

  let conv_atom { Atom.bindings ; tail } =
    let bindings = bindings |> Atom.LabelMap.bindings
        |> List.map (fun (lbl,f) -> lbl, f) |> LabelMap.of_list in
    { Records.Atom.bindings ; tail=tail }

  let of_union lst =
    Records.of_dnf (List.map (fun a -> [conv_atom a],[]) lst)
  let of_atom a = [[conv_atom a],[]] |> Records.of_dnf

  let approx t =
    let open Atom in
    let union_a a1 a2 =
      let dom = LabelSet.union (Atom.dom a1) (Atom.dom a2) in
      let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
          (lbl, Ty.F.cup (Atom.find lbl a1) (Atom.find lbl a2))
        ) |> LabelMap.of_list in
      { Atom.bindings ; Atom.tail = Ty.F.cup a1.tail a2.tail }
    in
    match as_union t with
    | [] -> raise EmptyAtom
    | hd::tl -> List.fold_left union_a hd tl

  let proj label t =
    as_union t |> List.map (Atom.find label) |> Ty.F.disj

  let merge a1 a2 =
    let open Atom in
    let dom = LabelSet.union (Atom.dom a1) (Atom.dom a2) in
    let is_opt f = Ty.F.get_descr f |> Ty.O.is_optional in
    let present = Ty.F.mk_descr (Ty.O.required Ty.any) in
    let bindings = dom |> LabelSet.to_list |> List.map (fun lbl ->
        let oty1, oty2 = Atom.find lbl a1, Atom.find lbl a2 in
        let oty = if is_opt oty2 then
          Ty.F.cup oty1 (Ty.F.cap oty2 present)
        else oty2 in
        (lbl, oty)
      ) |> LabelMap.of_list in
    let tail = if is_opt a2.tail then
      Ty.F.cup a1.tail (Ty.F.cap a2.tail present)
    else a2.tail in
    { bindings ; tail } |> of_atom

  let remove a lbl =
    let open Atom in
    let bindings = a.Atom.bindings |> LabelMap.add lbl (Ty.O.absent |> Ty.F.mk_descr) in
    { bindings ; tail=a.tail } |> of_atom

end

module Records = struct
  module Atom = struct
    module LabelMap = Map.Make(Label)
    type t = { bindings : Ty.O.t LabelMap.t ; tail : Ty.O.t }
    let dom t = LabelMap.bindings t.bindings |> List.map fst |> LabelSet.of_list
    let find lbl t =
      match LabelMap.find_opt lbl t.bindings with
      | Some f -> f
      | None -> t.tail
  end

  type t = Records.t
  type atom = Atom.t

  let ignore_fields { Records'.Atom.bindings ; tail } =
    let open Records'.Atom in
    let bindings = LabelMap.map Ty.F.get_descr bindings in
    let tail = Ty.F.get_descr tail in
    { Atom.bindings ; tail }
  let mk_fields { Atom.bindings ; tail } =
    let open Atom in
    let bindings = LabelMap.map Ty.F.mk_descr bindings in
    let tail = Ty.F.mk_descr tail in
    { Records'.Atom.bindings ; tail }

  let as_union t =
    Records'.as_union t |> List.map ignore_fields
  let of_union lst =
    List.map mk_fields lst |> Records'.of_union
  let of_atom a = mk_fields a |> Records'.of_atom

  let approx t = Records'.approx t |> ignore_fields

  let proj label t = Records'.proj label t |> Ty.F.get_descr

  let merge a1 a2 =
    Records'.merge (mk_fields a1) (mk_fields a2)

  let remove a lbl =
    Records'.remove (mk_fields a) lbl
end

module TagComp = struct
  type t = TagComp.t
  type atom = TagComp.Atom.t

  let is_identity t =
    let p = TagComp.tag t |> Tag.properties in
    match p with
    | Tag.NoProperty -> false
    | Tag.Monotonic m -> m.preserves_cap && m.preserves_cup

  let preserves_cap t =
    let p = TagComp.tag t |> Tag.properties in
    match p with
    | Tag.NoProperty -> false
    | Tag.Monotonic m -> m.preserves_cap

  let preserves_cup t =
    let p = TagComp.tag t |> Tag.properties in
    match p with
    | Tag.NoProperty -> false
    | Tag.Monotonic m -> m.preserves_cup

  let as_union t =
    if preserves_cap t |> not then
      invalid_arg "Tag component must satisfy preserves_cap." ;
    let tag = TagComp.tag t in
    let ty_of_clause (ps,ns) =
      let p = ps |> List.map snd |> Ty.conj in
      let n = ns |> List.map snd |> List.map Ty.neg |> Ty.conj in
      tag, Ty.cap p n
    in
    TagComp.dnf t |> List.map ty_of_clause

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
