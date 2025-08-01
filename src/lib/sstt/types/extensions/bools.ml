open Core

let tag = Tag.mk "bool"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
  |> TagComp.as_atom |> snd

let atrue = Enums.Atom.mk "true"
let afalse = Enums.Atom.mk "false"

let btrue = atrue |> Descr.mk_enum |> Ty.mk_descr |> add_tag
let bfalse = afalse |> Descr.mk_enum |> Ty.mk_descr |> add_tag
let bool b = if b then btrue else bfalse

let any = Ty.cup btrue bfalse |> Transform.simplify

let extract ty =
  let open Printer in
  if Ty.leq ty (proj_tag any) && Ty.vars_toplevel ty |> VarSet.is_empty
  then Some [ { pid=[] ; pdef=[PUnprocessed ty] } ]
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
  let open Printer in
  match tstruct with
  | CDef (_, [{ pid=[] ; pdef=[PUnprocessed ty] }]) ->
    let (pos, enums) = ty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
    assert pos ;
    {
      t = List.mem atrue enums ;
      f = List.mem afalse enums
    }
  | _ -> assert false

open Prec

type printer = int -> assoc -> Format.formatter -> t -> unit
let print _ _ fmt { t ; f } =
  match t, f with
  | false, false -> assert false
  | true, true -> Format.fprintf fmt "bool"
  | true, false -> Format.fprintf fmt "true"
  | false, true -> Format.fprintf fmt "false"

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
