open Core

let tag = Tag.mk "str"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let enums = Hashtbl.create 256
let strings = Hashtbl.create 256
let is_alphanum = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false
let slugify = String.map (fun c -> if is_alphanum c then c else '_')
let str str =
  match Hashtbl.find_opt enums str with
  | Some atom -> atom |> Descr.mk_enum |> Ty.mk_descr |> add_tag
  | None ->
    let atom = Enum.mk ("_"^(slugify str)) in
    Hashtbl.add enums str atom ;
    Hashtbl.add strings atom str ;
    atom |> Descr.mk_enum |> Ty.mk_descr |> add_tag

let any_p = Enums.any |> Descr.mk_enums |> Ty.mk_descr
let any = add_tag any_p

type t = bool * string list

let to_t _ _ comp =
  try
    let (_, pty) = Op.TagComp.as_atom comp in
    if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
      let (pos, enums) = pty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
      let strs = enums |> List.map (Hashtbl.find strings) in
      Some (pos, strs)
    else
      None
    with Not_found -> None
let map _ v = v

open Prec

let print prec assoc fmt (pos, strs) =
  let pp_string _prec _assoc fmt str = Format.fprintf fmt "%S" str in
  let aux = print_cup pp_string in
  if pos then
    aux prec assoc fmt strs
  else if not pos && strs = [] then
    Format.fprintf fmt "string"
  else
    let sym,prec',_ as opinfo = binop_info Diff in
    fprintf prec assoc opinfo fmt "string%s%a" sym (aux prec' Right) strs


let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}