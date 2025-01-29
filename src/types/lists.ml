open Sstt_core

let cons_tag = TagComp.Tag.mk "::"
let nil_atom = Atoms.Atom.mk "[]"

let cons hd tl =
  let param = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr in
  (cons_tag, param) |> Descr.mk_tag |> Ty.mk_descr

let nil = nil_atom |> Descr.mk_atom |> Ty.mk_descr

let any =
  let v = Var.mk "list" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd

let params_of_cons comp =
  let ty = Tags.mk_comp comp |> Descr.mk_tags |> Ty.mk_descr in
  if Ty.leq ty any then
    let (_,param) = TagComp.as_atom comp in
    let pair_comp = Ty.get_descr param |> Descr.get_tuples
      |> Tuples.get 2 in
    Some [ Op.TupleComp.proj 0 pair_comp ; Op.TupleComp.proj 1 pair_comp ]
  else
    None

let transform_cons (d,_) =
  match d with
  | Printer.PCustomTag (tag, [elt;tl]) when TagComp.Tag.equal tag cons_tag ->
    let printer fmt =
      Format.fprintf fmt "(%a)::%a" Printer.print_descr' elt Printer.print_descr' tl
    in
    Printer.PCustom printer
  | d -> d

let post_process t =
  Printer.map_t transform_cons t

let printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(cons_tag, params_of_cons)] ;
  Printer.post = post_process
  }
