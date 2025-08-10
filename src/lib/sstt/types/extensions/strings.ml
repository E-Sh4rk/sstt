open Core
open Sstt_utils


let tag = Tag.mk "str"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> TagComp.as_atom |> snd

let enums = Hashtbl.create 256
let strings = Hashtbl.create 256
let str str =
  match Hashtbl.find_opt enums str with
  | Some atom -> atom |> Descr.mk_enum |> Ty.mk_descr |> add_tag
  | None ->
    let atom = Enums.Atom.mk str in
    Hashtbl.add enums str atom ;
    Hashtbl.add strings atom str ;
    atom |> Descr.mk_enum |> Ty.mk_descr |> add_tag

let any = Enums.any |> Descr.mk_enums |> Ty.mk_descr |> add_tag

type t = bool * string list

let to_t _ _ ty =
  let (pos, enums) = ty |> proj_tag |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
  let strs = enums |> List.map Enums.Atom.name in
  Some (pos, strs)

let map _ v = v

open Prec

let print prec assoc fmt (pos, strs) =
  let pp_string fmt str = Format.fprintf fmt "%S" str in
  let aux prec assoc fmt strs =
    match strs with
    | [] -> assert false
    | [elt] -> Format.fprintf fmt "%a" pp_string elt
    | strs ->
      let sym,_,_ as opinfo = varop_info Cup in
      fprintf prec assoc opinfo fmt "%a" (print_seq pp_string sym) strs
  in
  if pos then
    aux prec assoc fmt strs
  else if not pos && strs = [] then
    Format.fprintf fmt "string"
  else
    let sym,prec',_ as opinfo = binop_info Diff in
    fprintf prec assoc opinfo fmt "string%s%a" sym (aux prec' Right) strs


let printer_builder = Printer.builder ~any ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}