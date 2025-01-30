open Sstt_core
open Sstt_utils

let list_tag = TagComp.Tag.mk "list"

let tag t =
  (list_tag, t) |> Descr.mk_tag |> Ty.mk_descr

let cons hd tl = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr |> tag

let nil = [] |> Descr.mk_tuple |> Ty.mk_descr |> tag

let any =
  let v = Var.mk "list" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd

(* Basic printer *)

let basic_extract ty =
  let (_,any) = any |> Ty.get_descr |> Descr.get_tags
  |> Tags.get list_tag |> TagComp.as_atom in
  if Ty.leq ty any then
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps = Tuples.get 0 tuples |> Op.TupleComp.as_union in
    let cons_comps = Tuples.get 2 tuples |> Op.TupleComp.as_union in
    Some (nil_comps@cons_comps |> List.map (List.map (fun ty -> Printer.CTPLeaf ty)))
  else None

let basic_printer tstruct fmt =
  match tstruct with
  | Printer.TSNode _ -> assert false
  | Printer.TSDef (_, union) ->
    let print_line fmt l =
      match l with
      | [] -> Format.fprintf fmt "[]"
      | [Printer.TPLeaf elt; Printer.TPLeaf tl] ->
        Format.fprintf fmt "%a::%a" Printer.print_descr' elt Printer.print_descr' tl
      | _ -> assert false
    in
    Format.fprintf fmt "(%a)" (print_seq print_line " | ") union

let basic_printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(list_tag, basic_extract)] ;
  Printer.printers = [(list_tag, basic_printer)]
  }

(* Advanced printer *)

let extract ty =
  try
    let pair = TupleComp.any 2 |> Descr.mk_tuplecomp |> Ty.mk_descr in
    let nil = TupleComp.any 0 |> Descr.mk_tuplecomp |> Ty.mk_descr in
    if Ty.leq ty (Ty.cup pair nil) |> not then raise Exit ;
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps =
      Tuples.get 0 tuples |> Op.TupleComp.as_union
      |> List.map (List.map (fun ty -> Printer.CTPLeaf ty))
    in
    let cons_comps =
      Tuples.get 2 tuples |> Op.TupleComp.as_union
      |> List.map (fun pair ->
        match pair with
        | [l;r] ->
          let (_,ty) = r |> Ty.get_descr |> Descr.get_tags
          |> Tags.get list_tag |> TagComp.as_atom in
          if Ty.leq r (Descr.mk_tag (list_tag, ty) |> Ty.mk_descr) |> not
          then raise Exit ;
          [Printer.CTPLeaf l ; Printer.CTPRec ty ]
        | _ -> assert false  
      )
    in
    Some (nil_comps@cons_comps)
  with Exit -> None

type t = Node of Printer.NodeId.t * d list | Loop of Printer.NodeId.t
and d = Cons of Printer.descr * t | Nil

let rec to_t tstruct =
  match tstruct with
  | Printer.TSNode nid -> Loop nid
  | Printer.TSDef (nid, union) ->
    let to_d params =
      match params with
      | [] -> Nil
      | [Printer.TPLeaf elt ; Printer.TPRec tstruct] ->
        Cons (elt, to_t tstruct)
      | _ -> assert false
    in
    Node (nid, List.map to_d union)

module Lt = struct
  type t = Printer.descr option
  let equiv = Option.equal (fun (_,n1) (_,n2) -> Ty.equiv n1 n2)
  let compare = Option.compare (fun (_,n1) (_,n2) -> Ty.compare n1 n2)
  let symbol d = Some d
  let epsilon = None
  let is_epsilon = Option.is_none
  let pp fmt = Option.iter (Printer.print_descr' fmt)
end
module Automaton = Automaton.Make(Lt)
module NIMap = Map.Make(Printer.NodeId)

let to_automaton t =
  let auto = Automaton.create () in
  let rec aux env t =
    match t with
    | Node (nid, ds) ->
      let state = Automaton.mk_state auto in
      let env = NIMap.add nid state env in
      let treat_d d =
        match d with
        | Nil -> Automaton.set_final auto state
        | Cons (d, t) ->
          let lt = Lt.symbol d in
          let state' = aux env t in
          Automaton.add_trans auto state lt state'
      in
      List.iter treat_d ds ; state
    | Loop nid ->
      let state = Automaton.mk_state auto in
      Automaton.add_trans auto state Lt.epsilon (NIMap.find nid env) ;
      state
  in
  let state = aux NIMap.empty t in
  Automaton.set_start auto state ;
  auto

module Regexp = Automaton.R
type regexp = Regexp.t_ext
let to_regexp automaton =
  automaton |> Automaton.to_regex_my |> Regexp.simp_to_ext |> Regexp.simplify

let rec printer fmt regexp =
  match regexp with
  | Regexp.Letter d -> Format.fprintf fmt "%a" Lt.pp d
  | Regexp.Concat lst -> Format.fprintf fmt "(%a)" (print_seq printer " ; ") lst
  | Regexp.Union lst -> Format.fprintf fmt "(%a)" (print_seq printer " | ") lst
  | Regexp.Star r -> Format.fprintf fmt "(%a)*" printer r
  | Regexp.Plus r -> Format.fprintf fmt "(%a)+" printer r
  | Regexp.Option r -> Format.fprintf fmt "(%a)?" printer r

let printer tstruct fmt =
  Format.fprintf fmt "[ %a ]" printer
    (tstruct |> to_t |> to_automaton |> to_regexp)

let printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(list_tag, extract)] ;
  Printer.printers = [(list_tag, printer)]
  }