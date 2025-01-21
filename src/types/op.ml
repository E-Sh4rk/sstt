open Core
module D = Descr

module Arrows = struct
  type t = D.Arrows.t

  let dom t =
    let summand_dom (ps,_,_) = ps |> List.map fst |> Ty.disj in
    D.Arrows.dnf t |> D.Arrows.Dnf.simplify |> List.map summand_dom |> Ty.conj

  let apply t s =
    let dnf = D.Arrows.dnf t |> D.Arrows.Dnf.simplify in
    dnf |> List.map begin
      fun (ps,_,_) ->
        let rec possible_outputs current_set ps =
          let t = List.map fst current_set |> Ty.conj in
          if Ty.leq s t then [List.map snd current_set |> Ty.conj]
          else begin
            let aux (e,current_set) = possible_outputs current_set (e::ps) in
            Utils.take_one current_set |> List.map aux |> List.flatten
          end
      in
      possible_outputs ps [] |> Ty.conj
    end |> Ty.disj

  let worra t out =
    let dnf = D.Arrows.dnf t |> D.Arrows.Dnf.simplify in
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
  type t = D.Tuples.Products.t

  let proj i t =
    let union = D.Tuples.Products.dnf t |> D.Tuples.Products.Dnf.combine in
    union |> List.map fst |> List.map (fun lst -> List.nth lst i) |> Ty.disj
end

module Records = struct
  type t = D.Records.t

  let proj label t =
    let union = D.Records.dnf t |> D.Records.Dnf.combine in
    union |> List.map fst |> List.map (D.Records.Atom'.find label)
    |> D.Records.Atom'.OTy.disj
end
