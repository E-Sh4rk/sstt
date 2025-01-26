open Sstt_core

module VDMap = Map.Make(VDescr)

type ctx = {
  mutable cache : Var.t VDMap.t ;
  mutable eqs : (Var.t * Ty.t) list
}

let simpl t =
  let ctx = {
    cache = VDMap.empty ;
    eqs = []
  } in
  let rec aux t =
    let vd = Ty.def t in
    match VDMap.find_opt vd ctx.cache with
    | Some v -> v
    | None ->
      let v = Var.mk "" in
      ctx.cache <- VDMap.add vd v ctx.cache ;
      let vd = simpl_vdescr vd in
      ctx.eqs <- (v, Ty.of_def vd)::ctx.eqs ;
      v
  and simpl_vdescr vd =
    vd |> VDescr.dnf |> VDescr.Dnf.simplify
    |> List.map (fun (ps,ns,l) -> (ps,ns,simpl_descr l))
    |> VDescr.of_dnf
  and simpl_descr d =
    let open Descr in
    d |> components |> List.map (function
      | Intervals i -> Intervals i
      | Atoms a -> Atoms a
      | Arrows a -> Arrows (simpl_arrows a)
      | Tuples t -> Tuples (simpl_tuples t)
      | Records r -> Records (simpl_records r)
    ) |>  Descr.of_components
  and simpl_arrows _ =
    failwith "TODO"
  and simpl_records _ =
    failwith "TODO"
  and simpl_tuples t =
    let (comps, others) = Tuples.components t in
    (List.map simpl_products comps, others) |> Tuples.of_components
  and simpl_products _ =
    failwith "TODO"
  in
  let v = aux t in
  let res = Ty.of_eqs ctx.eqs |> VarMap.of_list in
  VarMap.find v res
