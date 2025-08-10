open Core

let tag = Tag.mk "bool"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let atrue = Enums.Atom.mk "true"
let afalse = Enums.Atom.mk "false"

let btrue = atrue |> Descr.mk_enum |> Ty.mk_descr |> add_tag
let bfalse = afalse |> Descr.mk_enum |> Ty.mk_descr |> add_tag
let bool b = if b then btrue else bfalse

let any = Ty.cup btrue bfalse |> Transform.simplify

type t = { t : bool ; f : bool }
let any_t = { t = true ; f = true }
let empty_t = { t = false ; f = false }
let neg_t { t ; f } = { t = not t ; f = not f }
let components { t ; f } =
  [
    t, true ;
    f, false
  ] |> List.filter_map (fun (b,k) -> if b then Some k else None)

let to_t _ _ ty =
  let t = Ty.leq btrue ty in
  let f = Ty.leq bfalse ty in
  Some { t; f }

let map _f v = v
let print _ _ fmt {t; f} =
  match t, f with
  | false, false -> assert false
  | true, true -> Format.fprintf fmt "bool"
  | true, false -> Format.fprintf fmt "true"
  | false, true -> Format.fprintf fmt "false"

let printer_builder = Printer.builder ~any ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}