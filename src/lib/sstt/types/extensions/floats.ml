open Core
open Sstt_utils

let tag = Tag.mk "flt"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

type k = Ninf | Neg | Nzero | Pzero | Pos | Pinf | Nan

let enums =
  let open Enum in
  [
    Ninf, mk "ninf" ;
    Neg, mk "neg" ;
    Nzero, mk "nzero" ;
    Pzero, mk "pzero" ;
    Pos, mk "pos" ;
    Pinf, mk "pinf" ;
    Nan, mk "nan"
  ]

let flt_p k =
  let enum = List.assoc k enums in
  enum |> Descr.mk_enum |> Ty.mk_descr
let flt k = flt_p k |> add_tag

let any_p =
  [Ninf;Neg;Nzero;Pzero;Pos;Pinf;Nan] |> List.map flt_p |> Ty.disj
let any = add_tag any_p


type t = { ninf : bool ; neg : bool ; nzero : bool ; pzero : bool ; pos : bool ; pinf : bool ; nan : bool }

let any_t = {
  ninf = true ; neg = true ; nzero = true ; pzero = true ;
  pos = true ; pinf = true ; nan = true
}
let empty_t = {
  ninf = false ; neg = false ; nzero = false ; pzero = false ;
  pos = false ; pinf = false ; nan = false
}
let neg_t { ninf ; neg ; nzero ; pzero ; pos ; pinf ; nan } =
  { ninf = not ninf ; neg = not neg ; nzero = not nzero ; pzero = not pzero ;
    pos = not pos ; pinf = not pinf ; nan = not nan }
let components { ninf ; neg ; nzero ; pzero ; pos ; pinf ; nan } =
  [
    ninf, Ninf ;
    neg, Neg ;
    nzero, Nzero ;
    pzero, Pzero ;
    pos, Pos ;
    pinf, Pinf ;
    nan, Nan
  ] |> List.filter_map (fun (b,k) -> if b then Some k else None)

let to_t _ _ comp =
  let (_, pty) = TagComp.as_atom comp in
  let (pos, enums') = pty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
  if pos && Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
    let has k =
      let enum = List.assoc k enums in
      List.mem enum enums'
    in
    Some {
      ninf = has Ninf ;
      neg = has Neg ;
      nzero = has Nzero ;
      pzero = has Pzero ;
      pos = has Pos ;
      pinf = has Pinf ;
      nan = has Nan
    }
  else None
open Prec

let map _ v = v
let comp_names =
  [
    Ninf, "-inf" ;
    Neg, "<0f" ;
    Nzero, "-0f" ;
    Pzero, "+0f" ;
    Pos, ">0f" ;
    Pinf, "+inf" ;
    Nan, "nan"
  ]
let print prec assoc fmt t =
  let pp_k fmt k = Format.fprintf fmt "%s" (List.assoc k comp_names) in
  let comp = components t in
  let pos, t = if List.length comp >= 4 then false, neg_t t else true, t in
  let comp = components t in
  let aux prec assoc fmt comp =
    match comp with
    | [] -> assert false
    | [elt] -> Format.fprintf fmt "%a" pp_k elt
    | comp ->
      let sym,_,_ as opinfo = varop_info Cup in
      fprintf prec assoc opinfo fmt "%a" (print_seq pp_k sym) comp
  in
  if pos then
    aux prec assoc fmt comp
  else if not pos && comp = [] then
    Format.fprintf fmt "float"
  else
    let sym,prec',_ as opinfo = binop_info Diff in
    fprintf prec assoc opinfo fmt "float%s%a" sym (aux prec' Right) comp

let printer_builder = Printer.builder ~to_t ~map ~print

let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}