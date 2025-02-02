open Sstt_core
open Sstt_utils

let tag = TagComp.Tag.mk "flt"

let add_tag t =
  (tag, t) |> Descr.mk_tag |> Ty.mk_descr

type k = Ninf | Neg | Nzero | Pzero | Pos | Pinf | Nan

let atoms =
  let open Atoms.Atom in
  [
    Ninf, mk "ninf" ;
    Neg, mk "neg" ;
    Nzero, mk "nzero" ;
    Pzero, mk "pzero" ;
    Pos, mk "pos" ;
    Pinf, mk "pinf" ;
    Nan, mk "nan"
  ]

let flt k =
  let atom = List.assoc k atoms in
  atom |> Descr.mk_atom |> Ty.mk_descr |> add_tag

let any =
  [Ninf;Neg;Nzero;Pzero;Pos;Pinf;Nan] |> List.map flt |> Ty.disj |> Transform.simplify

let extract ty =
  let open Printer in
  let (_,any) = any |> Ty.get_descr |> Descr.get_tags
  |> Tags.get tag |> TagComp.as_atom in
  if Ty.leq ty any && Ty.vars_toplevel ty |> VarSet.is_empty
  then Some [{ tag_case_id=0 ; tag_params=[PUnprocessed ty] } ]
  else None

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

let to_t tstruct =
  let open Printer in
  match tstruct with
  | CDef (_, [{ case_id=0 ; params=[PUnprocessed ty]}]) ->
    let (pos, atoms') = ty |> Ty.get_descr |> Descr.get_atoms |> Atoms.destruct in
    assert pos ;
    let has k =
      let atom = List.assoc k atoms in
      List.mem atom atoms'
    in
    {
      ninf = has Ninf ;
      neg = has Neg ;
      nzero = has Nzero ;
      pzero = has Pzero ;
      pos = has Pos ;
      pinf = has Pinf ;
      nan = has Nan
    }
  | _ -> assert false

type printer = Format.formatter -> t -> unit
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
let print fmt t =
  let pp_k fmt k = Format.fprintf fmt "%s" (List.assoc k comp_names) in
  let comp = components t in
  let pos, t = if List.length comp >= 4 then false, neg_t t else true, t in
  let comp = components t in
  let neg = if pos then "" else "float\\" in
  match comp with
  | [] -> assert (not pos) ; Format.fprintf fmt "float"
  | [k] -> Format.fprintf fmt "%s%a" neg pp_k k
  | ks -> Format.fprintf fmt "%s(%a)" neg (print_seq pp_k " | ") ks

let to_printer (print:printer) tstruct fmt =
  print fmt (tstruct |> to_t)  

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print
