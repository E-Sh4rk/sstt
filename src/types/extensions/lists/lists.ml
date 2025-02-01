open Sstt_core
open Sstt_utils

let tag = TagComp.Tag.mk "lst"

let add_tag t =
  (tag, t) |> Descr.mk_tag |> Ty.mk_descr

let cons hd tl = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag

let nil = [] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag

let any =
  let v = Var.mk "" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd |> Transform.simplify

let any_non_empty = cons Ty.any any

(* TODO: get_tag, make destruct operate on a TagComp *)

let destruct ty =
  let union =
    ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
    |> TagComp.as_atom |> snd |> Ty.get_descr |> Descr.get_tuples
    |> Tuples.get 2 |> Op.TupleComp.as_union
  in
  union |> List.map (fun comps -> match comps with
  | [elt;tl] -> elt, tl
  | _ -> assert false)

let destruct' ty =
  try
    let comps =
      ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
      |> TagComp.as_atom |> snd |> Ty.get_descr |> Descr.get_tuples
      |> Tuples.get 2 |> Op.TupleComp.approx
    in
    match comps with
    | [elt;tl] -> elt, tl
    | _ -> assert false
  with Op.EmptyAtom -> Ty.empty, Ty.empty

(* Basic printer *)

(* TODO: disallow vars at top-level (in other extensions too).
   If vars are in the rec part, backup to a basic printer. *)

let basic_extract ty =
  let (_,any) = any |> Ty.get_descr |> Descr.get_tags
  |> Tags.get tag |> TagComp.as_atom in
  if Ty.leq ty any then
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps = Tuples.get 0 tuples |> Op.TupleComp.as_union in
    let cons_comps = Tuples.get 2 tuples |> Op.TupleComp.as_union in
    Some (nil_comps@cons_comps |> List.map (List.map (fun ty -> Printer.LeafParam ty)))
  else None

let basic_printer tstruct fmt =
  match tstruct with
  | Printer.CNode _ -> assert false
  | Printer.CDef (_, union) ->
    let print_line fmt l =
      match l with
      | [] -> Format.fprintf fmt "[]"
      | [Printer.CLeaf elt; Printer.CLeaf tl] ->
        Format.fprintf fmt "%a::%a" Printer.print_descr_atomic elt Printer.print_descr_atomic tl
      | _ -> assert false
    in
    Format.fprintf fmt "(%a)" (print_seq print_line " | ") union

let basic_printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, basic_extract)] ;
  Printer.printers = [(tag, basic_printer)]
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
      |> List.map (List.map (fun ty -> Printer.LeafParam ty))
    in
    let cons_comps =
      Tuples.get 2 tuples |> Op.TupleComp.as_union
      |> List.map (fun pair ->
        match pair with
        | [l;r] ->
          let (_,ty) = r |> Ty.get_descr |> Descr.get_tags
          |> Tags.get tag |> TagComp.as_atom in
          if Ty.leq r (Descr.mk_tag (tag, ty) |> Ty.mk_descr) |> not
          then raise Exit ;
          [Printer.LeafParam l ; Printer.RecParam ty ]
        | _ -> assert false  
      )
    in
    Some (nil_comps@cons_comps)
  with Exit -> None

type t = Node of Printer.NodeId.t * d list | Loop of Printer.NodeId.t
and d = Cons of Printer.descr * t | Nil

let rec to_t tstruct =
  match tstruct with
  | Printer.CNode nid -> Loop nid
  | Printer.CDef (nid, union) ->
    let to_d params =
      match params with
      | [] -> Nil
      | [Printer.CLeaf elt ; Printer.CRec tstruct] ->
        Cons (elt, to_t tstruct)
      | _ -> assert false
    in
    Node (nid, List.map to_d union)

module Lt = struct
  open Printer
  type t = descr option
  let equiv = Option.equal (fun d1 d2 -> Ty.equiv d1.ty d2.ty)
  let compare = Option.compare (fun d1 d2 -> Ty.compare d1.ty d2.ty)
  let symbol d = Some d
  let epsilon = None
  let is_epsilon = Option.is_none
  let to_symbol = Option.get
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

type 'a regexp =
| Epsilon
| Symbol of 'a
| Concat of 'a regexp list
| Union of 'a regexp list
| Star of 'a regexp
| Plus of 'a regexp
| Option of 'a regexp

let rec convert_regexp (r:Regexp.t_ext) =
  match r with
  | Regexp.Letter l when Lt.is_epsilon l -> Epsilon
  | Regexp.Letter l -> Symbol (Lt.to_symbol l)
  | Regexp.Concat lst -> Concat (List.map convert_regexp lst)
  | Regexp.Union lst -> Union (List.map convert_regexp lst)
  | Regexp.Star r -> Star (convert_regexp r)
  | Regexp.Plus r -> Plus (convert_regexp r)
  | Regexp.Option r -> Option (convert_regexp r)

let to_regexp automaton =
  automaton |> Automaton.to_regex_my |> Regexp.simp_to_ext |> Regexp.simplify
  |> convert_regexp

let prec_star = 2
let prec_plus = 2
let prec_option = 2
let prec_concat = 1
let prec_union = 0

let rec print prec fmt regexp =
  let need_paren = ref false in
  let paren prec' =
    if prec' <= prec
    then begin
      need_paren := true ;
      Format.fprintf fmt "("
    end
  in
  let () = match regexp with
  | Epsilon -> ()
  | Symbol d -> Format.fprintf fmt "%a" Printer.print_descr_atomic d
  | Concat lst ->
    paren prec_concat ;
    Format.fprintf fmt "%a" (print_seq (print prec_concat) " ") lst
  | Union lst ->
    paren prec_union ;
    Format.fprintf fmt "%a" (print_seq (print prec_union) " | ") lst
  | Star r ->
    paren prec_star ;
    Format.fprintf fmt "%a*" (print prec_star) r
  | Plus r ->
    paren prec_plus ;
    Format.fprintf fmt "%a+" (print prec_plus) r
  | Option r ->
    paren prec_option ;
    Format.fprintf fmt "%a?" (print prec_option) r
  in
  if !need_paren then Format.fprintf fmt ")"

type printer = Format.formatter -> Printer.descr regexp -> unit

let print fmt =
  Format.fprintf fmt "[ %a ]" (print (-1))

let to_printer (print:printer) tstruct fmt =
  print fmt (tstruct |> to_t |> to_automaton |> to_regexp)

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print

(* Builder *)

let build r =
  let rec aux r next =
    match r with
    | Epsilon -> next
    | Symbol ty -> cons ty next
    | Concat lst -> List.fold_right aux lst next
    | Union lst ->
      lst |> List.map (fun ty -> aux ty next) |> Ty.disj
    | Star r ->
      let v = Var.mk "" in
      let ty = Ty.cup (aux r (Ty.mk_var v)) next in
      Ty.of_eqs [(v,ty)] |> List.hd |> snd
    | Plus r -> aux r (aux (Star r) next)
    | Option r -> Ty.cup (aux r next) next
  in
  aux r nil
