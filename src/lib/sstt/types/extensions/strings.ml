open Core

let tag = Tag.mk "str"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd

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

let extract pty =
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
    let (pos, enums) = pty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
    try
      let strs = enums |> List.map (Hashtbl.find strings) in
      (pos, strs)
    with Not_found -> invalid_arg "Malformed string type"
  else
    invalid_arg "Malformed string type"

let destruct ty = Ty.cap (proj_tag ty) any_p |> extract

let to_t _ comp =
  try Some (Op.TagComp.as_atom comp |> snd |> extract)
  with Invalid_argument _ -> None
let map _ _ v = v

open Prec

let print prec assoc fmt (pos, strs) =
  let pp_string _prec _assoc fmt str = Format.fprintf fmt "%S" str in
  let aux = print_cup pp_string in
  if pos then
    aux prec assoc fmt strs
  else if not pos && strs = [] then
    Format.fprintf fmt "string"
  else
    print_binary_op' (print_atomic_str "string") aux
      prec assoc Diff fmt () strs

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}