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

let extract comp =
  let ty = Tags.mk_comp comp |> Descr.mk_tags |> Ty.mk_descr in
  if Ty.leq ty any then
    let (_,param) = TagComp.as_atom comp in
    let tuples = Ty.get_descr param |> Descr.get_tuples in
    let nil_comps = Tuples.get 0 tuples |> Op.TupleComp.as_union in
    let cons_comps = Tuples.get 2 tuples |> Op.TupleComp.as_union in
    Some (nil_comps@cons_comps)
  else
    None

let transform_cons (d,_) =
  match d with
  | Printer.PCustomTag (tag, union) when TagComp.Tag.equal tag list_tag ->
    let print_line fmt l =
      match l with
      | [] -> Format.fprintf fmt "[]"
      | [elt;tl] -> Format.fprintf fmt "%a::%a" Printer.print_descr' elt Printer.print_descr' tl
      | _ -> assert false
    in
    let printer fmt =
      Format.fprintf fmt "(%a)" (print_seq print_line " | ") union
    in
    Printer.PCustom printer
  | d -> d

let post_process t =
  Printer.map_t transform_cons t

let printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(list_tag, extract)] ;
  Printer.post = post_process
  }
