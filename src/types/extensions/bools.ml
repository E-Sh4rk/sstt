open Sstt_core

let tag = TagComp.Tag.mk "bool"

let add_tag t =
  (tag, t) |> Descr.mk_tag |> Ty.mk_descr

let atrue = Atoms.Atom.mk "true"
let afalse = Atoms.Atom.mk "false"

let btrue = atrue |> Descr.mk_atom |> Ty.mk_descr
let bfalse = afalse |> Descr.mk_atom |> Ty.mk_descr
let bool b = if b then btrue else bfalse

let any = Ty.cup btrue bfalse |> Transform.simplify

let extract ty =
  let (_,any) = any |> Ty.get_descr |> Descr.get_tags
  |> Tags.get tag |> TagComp.as_atom in
  if Ty.leq ty any
  then Some [[Printer.LeafParam ty]]
  else None

type t = { t : bool ; f : bool }
let any_t = { t = true ; f = true }
let empty_t = { t = false ; f = false }
let neg_t { t ; f } = { t = not t ; f = not f }
let components { t ; f } =
  [
    t, true ;
    f, false
  ] |> List.filter_map (fun (b,k) -> if b then Some k else None)

let to_t tstruct =
  match tstruct with
  | Printer.CDef (_, [[Printer.CLeaf d]]) ->
    let (pos, atoms) = d.ty |> Ty.get_descr |> Descr.get_atoms |> Atoms.destruct in
    assert pos ;
    {
      t = List.mem atrue atoms ;
      f = List.mem afalse atoms
    }
  | _ -> assert false

type printer = Format.formatter -> t -> unit
let print fmt { t ; f } =
  match t, f with
  | false, false -> assert false
  | true, true -> Format.fprintf fmt "bool"
  | true, false -> Format.fprintf fmt "true"
  | false, true -> Format.fprintf fmt "false"

let to_printer (print:printer) tstruct fmt =
  print fmt (tstruct |> to_t)  

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print
