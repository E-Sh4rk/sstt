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
  let dnf = [ (Ty.any, Ty.any)::ps, ns, true ] in
  Arrows.of_dnf dnf |> Descr.mk_arrows |> Ty.mk_descr |> add_tag
let mk' fields = mk (fields, [])
let any = mk' []

let destruct ty =
  ty |> Ty.get_descr |> Descr.get_arrows |> Arrows.dnf |> Arrows.Dnf.simplify
  |> List.map (fun (ps, ns, _) ->
    let ps = ps |> List.filter_map (fun (s,t) ->
      if Ty.is_any t then None
      else Some { dom=s ; codom=t }
      )
    in
    let ns = ns |> List.map (fun (s,t) -> { dom=s ; codom=t }) in
    ps, ns
  )

let extract ty =
  let open Printer in
  if Ty.leq ty (proj_tag any) && Ty.vars_toplevel ty |> VarSet.is_empty
  then
    let fields_to_pdef fs =
      List.concat_map (fun f -> [PLeaf f.dom ; PLeaf f.codom]) fs
    in
    let ps = destruct ty |> List.concat_map (fun (ps, ns) ->
      let ps = { pid=[] ; pdef=fields_to_pdef ps } in
      let ns = { pid=[] ; pdef=fields_to_pdef ns } in
      [ps;ns]
    ) in
    Some ps
  else None

let to_t tstruct =
  let open Printer in
  let rec group_bindings lst =
    match lst with
    | [] -> []
    | [_] -> assert false
    | dom::codom::lst ->
      {dom ; codom}::(group_bindings lst)
  in
  let rec aux_lines defs =
    match defs with
    | [] -> []
    | [_] -> assert false
    | ps::ns::defs ->
      let aux {pid;pdef} =
        assert (pid=[]) ;
        let bindings = List.map (function PLeaf d -> d | _ -> assert false) pdef in
        group_bindings bindings
      in
      (aux ps, aux ns)::(aux_lines defs)
  in
  match tstruct with
  | CDef (_, defs) ->
    aux_lines defs
  | _ -> assert false

let proj ~dom t =
  let arr = proj_tag t |> Ty.get_descr |> Descr.get_arrows in
  Op.Arrows.apply arr dom

let merge t {dom ; codom} =
  let merge_line (ps,_,_) =
    let ps = ps |> List.concat_map (fun (fdom, fcodom) ->
      if Ty.leq codom fcodom
      then [(fdom, fcodom)]
      else
        let arr1 = (Ty.cap fdom dom, Ty.cup fcodom codom) in
        let arr2 = (Ty.diff fdom dom, fcodom) in
        [arr1;arr2]
      ) in
    (ps,[],true)
  in
  let dnf = proj_tag t |> Ty.get_descr |> Descr.get_arrows
   |> Arrows.dnf |> Arrows.Dnf.simplify in
  let dnf = List.map merge_line dnf in
  Arrows.of_dnf dnf |> Descr.mk_arrows |> Ty.mk_descr |> add_tag

open Prec

type printer = int -> Prec.assoc -> Format.formatter -> Printer.descr t -> unit

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

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.extensions =
    let module M = struct
      type nonrec t = Printer.descr t
      let tag = tag
      let extractors = [extract]
      let get = to_t
      let print = printer
    end
  in [(module M : Printer.PrinterExt)]
  }

let printer_params' = printer_params print
