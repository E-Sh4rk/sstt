open Sstt_core
open Sstt_utils

let list_tag = TagComp.Tag.mk "list"

let tag t =
  (list_tag, t) |> Descr.mk_tag |> Ty.mk_descr

let cons hd tl = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr |> tag

let nil = [] |> Descr.mk_tuple |> Ty.mk_descr |> tag

let any =
  let v = Var.mk "list" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd

let extract ty =
  (* let pair = TupleComp.any 2 |> Descr.mk_tuplecomp |> Ty.mk_descr in
  let nil = TupleComp.any 0 |> Descr.mk_tuplecomp |> Ty.mk_descr in *)
  let (_,any) = any |> Ty.get_descr |> Descr.get_tags
  |> Tags.get list_tag |> TagComp.as_atom in
  (* if Ty.leq ty (Ty.cup pair nil) then *)
  if Ty.leq ty any then
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps = Tuples.get 0 tuples |> Op.TupleComp.as_union in
    let cons_comps = Tuples.get 2 tuples |> Op.TupleComp.as_union in
    Some (nil_comps@cons_comps |> List.map (List.map (fun ty -> Printer.CTPLeaf ty)))
  else None

let transform_tstruct tstruct =
  match tstruct with
  | Printer.TSNode _ -> assert false
  | Printer.TSDef (_, union) ->
    let print_line fmt l =
      match l with
      | [] -> Format.fprintf fmt "[]"
      | [Printer.TPLeaf elt; Printer.TPLeaf tl] ->
        Format.fprintf fmt "%a::%a" Printer.print_descr' elt Printer.print_descr' tl
      | _ -> assert false
    in
    let printer fmt =
      Format.fprintf fmt "(%a)" (print_seq print_line " | ") union
    in
    Printer.PCustom printer

let transform (d,_) = (* TODO: do the map_t printer-side *)
  match d with
  | Printer.PCustomTag (tag, tstruct) when TagComp.Tag.equal tag list_tag ->
    transform_tstruct tstruct
  | d -> d

let post_process t =
  Printer.map_t transform t

let printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(list_tag, extract)] ;
  Printer.post = post_process
  }
