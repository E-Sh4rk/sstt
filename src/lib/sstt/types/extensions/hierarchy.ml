open Core
open Sstt_utils

module Node = Id.NamedIdentifier()
module NSet = Set.Make(Node)
module NHT = Hashtbl.Make(Node)
module ESet = Set.Make(Enums.Atom)
module EHT = Hashtbl.Make(Enums.Atom)

type node_info = { sub: NSet.t; trans: NSet.t; atom: Enums.Atom.t }
type hierarchy = {
    nodes: node_info NHT.t ; mutable top_nodes: NSet.t ;
    atoms: Node.t EHT.t ; tag: Tag.t
  }

let new_hierarchy () =
  { nodes = NHT.create 10 ; top_nodes = NSet.empty ; atoms = EHT.create 10 ; tag = Tag.mk "hiy" }

let new_node h ~name ~subnodes =
  let trans_of nodes =
    let trans = NSet.to_list nodes |>
      List.map (fun n -> (NHT.find h.nodes n).trans) in
    List.fold_left NSet.union nodes trans
  in
  try
    let n = Node.mk name in
    let sub = NSet.of_list subnodes in
    let trans = trans_of sub in
    let sub = sub |> NSet.filter (fun n ->
      NSet.subset trans (NSet.remove n sub |> trans_of) |> not) in
    let trans = NSet.add n trans in
    let atom = Enums.Atom.mk name in
    NHT.add h.nodes n { sub ; trans ; atom } ;
    sub |> NSet.iter (fun n -> h.top_nodes <- NSet.remove n h.top_nodes) ;
    h.top_nodes <- NSet.add n h.top_nodes ;
    EHT.add h.atoms atom n ;
    n
  with Not_found -> raise (Invalid_argument "subnodes must be part of the hierarchy")

let mk h n =
  try
    let ns = (NHT.find h.nodes n).trans in
    let atoms = ns |> NSet.to_list |> List.map (fun n -> (NHT.find h.nodes n).atom) in
    let ty = Enums.construct (true, atoms) |> Descr.mk_enums |> Ty.mk_descr in
    (h.tag, ty) |> Descr.mk_tag |> Ty.mk_descr
  with Not_found -> raise (Invalid_argument "subnodes must be part of the hierarchy")

let any_enum = Enums.any |> Descr.mk_enums |> Ty.mk_descr
let extract h ty =
  let open Printer in
  if Ty.leq ty any_enum && Ty.vars_toplevel ty |> VarSet.is_empty then
    let (pos, enums) = ty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
    if pos && List.for_all (EHT.mem h.atoms) enums
    then Some [ { pid=[] ; pdef=[PUnprocessed ty] } ]
    else None
  else None

type t = line list
and line = L of Node.t * t

let to_t h tstruct =
    let open Printer in
    match tstruct with
    | CDef (_, [{ pid=[] ; pdef=[PUnprocessed ty] }]) ->
      let (pos, enums) = ty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
      let enums = ESet.of_list enums in
      assert pos ;
      let rec aux pos n =
        let ni = NHT.find h.nodes n in
        let sub = NSet.to_list ni.sub in
        if (ESet.mem ni.atom enums) = pos
        then [L (n, List.concat_map (aux (not pos)) sub)]
        else List.concat_map (aux pos) sub
      in
      h.top_nodes |> NSet.to_list |> List.concat_map (aux true)
    | _ -> assert false

open Prec

type printer = int -> Prec.assoc -> Format.formatter -> t -> unit

let print prec assoc fmt t =
  let rec print_line prec assoc fmt (L (n, t)) =
    let sym,prec',_ as opinfo = binop_info Diff in
    if t = []
    then
      Format.fprintf fmt "%s" (Node.name n)
    else
      fprintf prec assoc opinfo fmt "%s%s%a"
        (Node.name n) sym
        (print_t prec' NoAssoc) t
  and print_t prec assoc fmt t =
    let sym,prec',_ as opinfo = varop_info Cup in
    fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t
  in
  print_t prec assoc fmt t

let printer_params printer h =
  {
    Printer.aliases = [] ;
    Printer.extensions =
      let module M = struct
        type nonrec t = t
        let tag = h.tag
        let extractors = [extract h]
        let get = to_t h
        let print = printer
      end
    in [(module M : Printer.PrinterExt)]
  }

let printer_params' = printer_params print
