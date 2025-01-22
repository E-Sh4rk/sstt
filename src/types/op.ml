open Core

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
  type t = Tuples.Products.t

  let proj i t =
    let union = Tuples.Products.dnf t |> Tuples.Products.Dnf.combine in
    union |> List.map fst |> List.map (fun lst -> List.nth lst i) |> Ty.disj
end

module Records = struct
  type t = Records.t

  let proj label t =
    let union = Records.dnf t |> Records.Dnf.combine in
    union |> List.map fst |> List.map (Records.Atom'.find label)
    |> Records.Atom'.OTy.disj
end
