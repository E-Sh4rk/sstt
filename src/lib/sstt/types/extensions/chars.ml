open Core
open Sstt_utils

let tag = Tag.mk "chr"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> TagComp.as_atom |> snd

type interval = char * char

let chr chr =
  Char.code chr |> Z.of_int |> Intervals.Atom.mk_singl
  |> Descr.mk_interval |> Ty.mk_descr |> add_tag

let interval (chr1, chr2) =
  let lb, ub = Char.code chr1 |> Z.of_int, Char.code chr2 |> Z.of_int in
  Intervals.Atom.mk_bounded lb ub |> Descr.mk_interval |> Ty.mk_descr |> add_tag

let any =
  let lb, ub = Z.zero, Z.of_int 255 in
  Intervals.Atom.mk_bounded lb ub
  |> Descr.mk_interval |> Ty.mk_descr |> add_tag

type t = interval list

let to_t _ _ ty =
  if not (Ty.is_empty ty) && Ty.leq ty any && Ty.vars_toplevel ty |> VarSet.is_empty
  then
    Some (ty |> proj_tag |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct
          |> List.map (fun a-> match Intervals.Atom.get a with
                Some z1, Some z2 -> Z.(to_int z1 |> Char.chr, to_int z2 |> Char.chr)
              | _ -> assert false))
  else None

let any_t = [(Char.chr 0, Char.chr 255)]

open Prec
let map _f v = v
let print prec assoc fmt ints =
  let pp_chars fmt (chr1, chr2) =
    if Char.equal chr1 chr2
    then Format.fprintf fmt "%C" chr1
    else Format.fprintf fmt "(%C-%C)" chr1 chr2
  in
  if ints = any_t then
    Format.fprintf fmt "char"
  else
    match ints with
    | [] -> assert false
    | [i] -> Format.fprintf fmt "%a" pp_chars i
    | ints ->
      let sym,_,_ as opinfo = varop_info Cup in
      fprintf prec assoc opinfo fmt "%a" (print_seq pp_chars sym) ints

let printer_builder = Printer.builder ~any ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}