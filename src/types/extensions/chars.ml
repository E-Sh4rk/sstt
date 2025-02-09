open Sstt_core
open Sstt_utils

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
  then
    let tag_params = [ { param_id=0 ; param_kind=PUnprocessed ty } ] in
    Some [{ tag_case_id=0 ; tag_params } ]
  else None

type t = interval list

let to_t tstruct =
  let open Printer in
  match tstruct with
  | CDef (_, [{ case_id=0 ; params=[{ param_id=0 ; param_kind=PUnprocessed ty}]}]) ->
    let intervals = ty |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct in
    intervals |> List.map (fun int -> match Intervals.Atom.get int with
      | (Some i1, Some i2) -> Z.to_int i1 |> Char.chr, Z.to_int i2 |> Char.chr
      | _ -> assert false)
  | _ -> assert false

let any_t = [(Char.chr 0, Char.chr 255)]

type printer = Format.formatter -> t -> unit
let print fmt ints =
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
    | ints -> Format.fprintf fmt "(%a)" (print_seq pp_chars " | ") ints

let to_printer (print:printer) tstruct fmt =
  print fmt (tstruct |> to_t)  

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print
