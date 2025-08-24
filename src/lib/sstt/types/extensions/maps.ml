open Core
open Sstt_utils

type 't field = { dom: 't ; codom: 't }
type 't t = ('t field list * 't field list) list

let tag = Tag.mk "map"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> TagComp.as_atom |> snd

let mk (ps, ns) =
  let ps = ps |> List.map (fun f -> f.dom, f.codom) in
  let ns = ns |> List.map (fun f -> f.dom, f.codom) in
  let dnf = [ (Ty.any, Ty.any)::ps, ns ] in
  Arrows.of_dnf dnf |> Descr.mk_arrows |> Ty.mk_descr |> add_tag
let mk' fields = mk (fields, [])
let any = mk' []

let destruct ty =
  let pty = proj_tag ty in
  if Ty.vars_toplevel pty |> VarSet.is_empty then
    pty |> Ty.get_descr |> Descr.get_arrows |> Arrows.dnf
    |> List.map (fun (ps, ns) ->
      let ps = ps |> List.filter_map (fun (s,t) ->
          if Ty.is_any t then None
          else Some { dom=s ; codom=t })
      in
      let ns = ns |> List.map (fun (s,t) -> { dom=s ; codom=t }) in
      ps, ns
    )
  else
    invalid_arg "Malformed map type"

let map f l =
  let ff t = { dom = f t.dom; codom = f t.codom } in
  List.map (fun (ps, ns) -> List.map ff ps, List.map ff ns) l

let to_t node ctx ty =
  try
    if Ty.leq ty any |> not then raise Exit ;
    let l = destruct ty in
    Some (map (node ctx) l)
  with _ -> None
let proj ~dom t =
  let arr = proj_tag t |> Ty.get_descr |> Descr.get_arrows in
  Op.Arrows.apply arr dom

let merge t {dom ; codom} =
  let merge_line (ps,_) =
    let ps = ps |> List.concat_map (fun (fdom, fcodom) ->
        if Ty.leq codom fcodom
        then [(fdom, fcodom)]
        else
          let arr1 = (Ty.cap fdom dom, Ty.cup fcodom codom) in
          let arr2 = (Ty.diff fdom dom, fcodom) in
          [arr1;arr2]
      ) in
    (ps,[])
  in
  let dnf = proj_tag t |> Ty.get_descr |> Descr.get_arrows |> Arrows.dnf in
  let dnf = List.map merge_line dnf in
  Arrows.of_dnf dnf |> Descr.mk_arrows |> Ty.mk_descr |> add_tag

open Prec


let print prec assoc fmt t =
  let print_field fmt (pos, f) =
    let arr = if pos then "=>" else "~>" in
    Format.fprintf fmt "%a %s %a" Printer.print_descr f.dom arr Printer.print_descr f.codom
  in
  let print_line fmt (ps, ns) =
    let ps, ns = List.map (fun d -> true, d) ps, List.map (fun d -> false, d) ns in
    Format.fprintf fmt "{{ %a }}"
      (print_seq print_field " ; ") (ps@ns)
  in
  let sym,_,_ as opinfo = varop_info Cup in
  fprintf prec assoc opinfo fmt "%a" (print_seq print_line sym) t

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
