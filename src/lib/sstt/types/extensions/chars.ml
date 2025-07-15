open Core
open Core.Utils

let tag = TagComp.Tag.mk "chr"

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

let extract ty =
  let open Printer in
  if Ty.leq ty (proj_tag any) && Ty.vars_toplevel ty |> VarSet.is_empty
  then Some [ { pid=[] ; pdef=[PUnprocessed ty] } ] 
  else None

type t = interval list

let to_t tstruct =
  let open Printer in
  match tstruct with
  | CDef (_, [{ pid=[] ; pdef=[PUnprocessed ty] }]) ->
    let intervals = ty |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct in
    intervals |> List.map (fun int -> match Intervals.Atom.get int with
      | (Some i1, Some i2) -> Z.to_int i1 |> Char.chr, Z.to_int i2 |> Char.chr
      | _ -> assert false)
  | _ -> assert false

let any_t = [(Char.chr 0, Char.chr 255)]

open Prec

type printer = int -> assoc -> Format.formatter -> t -> unit
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

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.extensions =
    let module M = struct
      type nonrec t = t
      let tag = tag
      let extractors = [extract]
      let get = to_t
      let print = printer
    end
  in [(module M : Printer.PrinterExt)]
  }

let printer_params' = printer_params print
