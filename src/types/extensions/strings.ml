open Sstt_core
open Sstt_utils

let tag = TagComp.Tag.mk "str"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
  |> TagComp.as_atom |> snd

let atoms = Hashtbl.create 256
let strings = Hashtbl.create 256
let str str =
  match Hashtbl.find_opt atoms str with
  | Some atom -> atom |> Descr.mk_atom |> Ty.mk_descr |> add_tag
  | None ->
    let atom = Atoms.Atom.mk str in
    Hashtbl.add atoms str atom ;
    Hashtbl.add strings atom str ;
    atom |> Descr.mk_atom |> Ty.mk_descr |> add_tag

let any = Atoms.any () |> Descr.mk_atoms |> Ty.mk_descr |> add_tag

let extract ty =
  let open Printer in
  if Ty.leq ty (proj_tag any) && Ty.vars_toplevel ty |> VarSet.is_empty then
    let (_, atoms) = ty |> Ty.get_descr |> Descr.get_atoms |> Atoms.destruct in
    if List.for_all (Hashtbl.mem strings) atoms
    then
      let tag_case_def = [ { comp_id=0 ; comp_def=[PUnprocessed ty] } ] in
      Some [{ tag_case_id=0 ; tag_case_def } ]
    else None
  else None

type t = bool * string list

let to_t tstruct =
  let open Printer in
  match tstruct with
  | CDef (_, [{ case_id=0 ; case_def=[{ comp_id=0 ; comp_def=[PUnprocessed ty]}]}]) ->
    let (pos, atoms) = ty |> Ty.get_descr |> Descr.get_atoms |> Atoms.destruct in
    let strs = atoms |> List.map Atoms.Atom.name in
    (pos, strs)
  | _ -> assert false

type printer = Format.formatter -> t -> unit
let print fmt (pos, strs) =
  let pp_string fmt str = Format.fprintf fmt "%S" str in
  let neg = if pos then "" else "string\\" in
  match strs with
  | [] -> assert (not pos) ; Format.fprintf fmt "string"
  | [elt] -> Format.fprintf fmt "%s%a" neg pp_string elt
  | strs -> Format.fprintf fmt "%s(%a)" neg (print_seq pp_string " | ") strs

let to_printer (print:printer) tstruct fmt =
  print fmt (tstruct |> to_t)  

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print
