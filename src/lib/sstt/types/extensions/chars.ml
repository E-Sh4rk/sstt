open Core

let tag = Tag.mk "chr"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

type interval = char * char

let chr chr =
  Char.code chr |> Z.of_int |> Intervals.Atom.mk_singl
  |> Descr.mk_interval |> Ty.mk_descr |> add_tag

let interval (chr1, chr2) =
  let lb, ub = Char.code chr1 |> Z.of_int, Char.code chr2 |> Z.of_int in
  Intervals.Atom.mk_bounded lb ub |> Descr.mk_interval |> Ty.mk_descr |> add_tag

let any_p =
  let lb, ub = Z.zero, Z.of_int 255 in
  Intervals.Atom.mk_bounded lb ub
  |> Descr.mk_interval |> Ty.mk_descr
let any = add_tag any_p

type t = interval list

let to_t _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && Ty.vars_toplevel pty |> VarSet.is_empty
  then
    Some (pty |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct
          |> List.map (fun a-> match Intervals.Atom.get a with
                Some z1, Some z2 -> Z.(to_int z1 |> Char.chr, to_int z2 |> Char.chr)
              | _ -> assert false))
  else None

let any_t = [(Char.chr 0, Char.chr 255)]

open Prec
let map _f v = v
let print prec assoc fmt ints =
  let pp_chars _prec _assoc fmt (chr1, chr2) =
    if Char.equal chr1 chr2
    then Format.fprintf fmt "%C" chr1
    else Format.fprintf fmt "(%C-%C)" chr1 chr2
  in
  if ints = any_t then Format.fprintf fmt "char"
  else print_cup pp_chars prec assoc fmt ints

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}