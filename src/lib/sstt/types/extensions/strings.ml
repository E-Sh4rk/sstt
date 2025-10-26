open Core
open Sstt_utils


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
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
    let (pos, enums) = pty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
    let strs = enums |> List.map Enum.name in
    Some (pos, strs)
  else
    None
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


let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}