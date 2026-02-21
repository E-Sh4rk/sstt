open Core

let tag = Tag.mk "lst"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd

let cons hd tl = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag

let nil = [] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag
let any =
  let v = Var.mk "" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd |> Transform.simplify

let any_non_empty = cons Ty.any any
let cons a b = cons a (Ty.cap any b)

let destruct ty =
  let ty = Ty.cap ty any in
  let union =
    proj_tag ty |> Ty.get_descr |> Descr.get_tuples
    |> Tuples.get 2 |> Op.TupleComp.as_union
  in
  union |> List.map (fun comps -> match comps with
      | [elt;tl] -> elt, tl
      | _ -> assert false)

let proj ty =
  let ty = Ty.cap ty any in
  try
    let comps =
      proj_tag ty |> Ty.get_descr |> Descr.get_tuples
      |> Tuples.get 2 |> Op.TupleComp.approx
    in
    match comps with
    | [elt;tl] -> elt, tl
    | _ -> assert false
  with Op.EmptyAtom -> Ty.empty, Ty.empty

let tuple_comp n t = Tuples.get n t |> Op.TupleComp.as_union
let of_tuple_comp n c =
   c |> Op.TupleComp.of_union n |> Descr.mk_tuplecomp |> Ty.mk_descr
let check_extract ty =
  let pty = ty |> proj_tag in
  let tuples = pty |> Ty.get_descr |> Descr.get_tuples
  in
  let nil_comps = tuple_comp 0 tuples in
  let cons_comps = tuple_comp 2 tuples in
  let ty_nil = of_tuple_comp 0 nil_comps in
  let ty_cons = of_tuple_comp 2 cons_comps in

  if Ty.(equiv pty (cup ty_nil ty_cons)) then
    nil_comps |> List.is_empty |> not,
    (cons_comps |> List.map (function [x;y] -> x,y | _ -> assert false))
  else invalid_arg "Invalid list type"

type node = { id : int; mutable graph : graph list}
and graph = RNil | RCons of Printer.descr * node
type basic = Nil | Cons of Printer.descr * Printer.descr
type repr = R of node | B of basic list

module VDHash = Hashtbl.Make(VDescr)

let to_repr build ty =
  let hd_tbl : Printer.descr VDHash.t = VDHash.create 8 in
  let tl_tbl = VDHash.create 8 in
  let cpt = ref 0 in
  let descr ty =
    match VDHash.find_opt hd_tbl (Ty.def ty) with
      Some d -> d
    | None -> let d = build ty in
      VDHash.add hd_tbl (Ty.def ty) d; d
  in
  let rec try_graph_node ty =
    match VDHash.find_opt tl_tbl (Ty.def ty) with
      Some n -> n
    | None ->
      let n = { id = !cpt; graph = [] } in
      VDHash.add tl_tbl (Ty.def ty) n;
      incr cpt;
      n.graph <-try_graph ty;
      n
  and try_graph ty =
    if Ty.vars_toplevel ty |> VarSet.is_empty |> not then raise Exit;
    let has_nil, cons_comps = check_extract ty in
    (if has_nil then [RNil] else []) @
    List.map (fun (hd, tl) -> RCons(descr hd, try_graph_node tl)) cons_comps
  in
  let basic ty =
    let has_nil, cons_comps = check_extract ty in
    (if has_nil then [Nil] else []) @
    (List.map (fun (hd, tl) -> Cons(descr hd, descr tl))
       cons_comps)
  in
  try
    R (try_graph_node ty)
  with Exit -> B (basic ty)

module Lt = struct
  open Printer
  type t = descr
  let equal d1 d2 = Ty.equiv d1.ty d2.ty
end
module Regexp = Regexp.Make(Lt)
module Automaton = Automaton.Make(Regexp)
module IMap = Map.Make(Int)

let to_automaton params_r =
  let auto = Automaton.create () in
  let rec aux env t =
    match IMap.find_opt t.id env with
      Some s -> s
    | None ->
      let state = Automaton.mk_state auto in
      let env = IMap.add t.id state env in
      let treat_d d =
        match d with
        | RNil -> Automaton.set_final auto state
        | RCons (d, t) ->
          let state' = aux env t in
          Automaton.add_trans auto state d state'
      in
      List.iter treat_d t.graph ; state
  in
  let state = aux IMap.empty params_r in
  assert (Automaton.is_initial auto state) ;
  auto

type 'a regexp =
  | Epsilon
  | Symbol of 'a
  | Concat of 'a regexp list
  | Union of 'a regexp list
  | Star of 'a regexp
  | Plus of 'a regexp
  | Option of 'a regexp

type t =
  | Regexp of Printer.descr regexp
  | Basic of basic list

let rec convert_regexp (r: Regexp.t_ext) =
  match r with
  | EEpsilon -> Epsilon
  | ELetter l -> Symbol l
  | EConcat rs -> Concat (List.map convert_regexp rs)
  | EUnion rs -> Union (List.map convert_regexp rs)
  | EStar r -> Star (convert_regexp r)
  | EOption r -> Option (convert_regexp r)
  | EPlus r -> Plus (convert_regexp r)


let to_regexp automaton =
  let simpl_union = function
    | Regexp.Union (Regexp.Letter d1, Regexp.Letter d2) ->
      Regexp.Letter (Printer.cup_descr d1 d2)
    | r -> r
  in
  automaton |> Automaton.to_regexp |> Regexp.simple_re simpl_union
  |> Regexp.to_ext |> convert_regexp

let to_t ctx comp =
  try
    let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
    Some (match to_repr (ctx.Printer.build) ty with
        | R r -> Regexp (r |> to_automaton |> to_regexp)
        | B bs -> Basic bs)
  with _ -> None

let rec map_re f = function
  | Epsilon -> Epsilon
  | Symbol s -> Symbol (f s)
  | Concat l -> Concat (List.map (map_re f) l)
  | Union l -> Union (List.map (map_re f) l)
  | Star r -> Star (map_re f r)
  | Plus r -> Plus (map_re f r)
  | Option r -> Option (map_re f r)

let map_basic f = function
  | Nil -> Nil
  | Cons (d1, d2) -> Cons (f d1, f d2)

let map f = function
    Regexp r -> Regexp (map_re f r)
  | Basic l -> Basic (List.map (map_basic f) l)

let prec_star = 2
let prec_plus = 2
let prec_option = 2
let prec_concat = 1
let prec_union = 0

let rec print_r prec fmt regexp =
  let need_paren = ref false in
  let paren prec' =
    if prec' <= prec
    then begin
      need_paren := true ;
      Format.fprintf fmt "!("
    end
  in
  let () = match regexp with
    | Epsilon -> ()
    | Symbol d -> Format.fprintf fmt "%a" Printer.print_descr_atomic d
    | Concat lst ->
      paren prec_concat ;
      Format.fprintf fmt "%a" (Prec.print_seq (print_r prec_concat) "@ ") lst
    | Union lst ->
      paren prec_union ;
      Format.fprintf fmt "%a" (Prec.print_seq (print_r prec_union) "@ |@ ") lst
    | Star r ->
      paren prec_star ;
      Format.fprintf fmt "%a*" (print_r prec_star) r
    | Plus r ->
      paren prec_plus ;
      Format.fprintf fmt "%a+" (print_r prec_plus) r
    | Option r ->
      paren prec_option ;
      Format.fprintf fmt "%a?" (print_r prec_option) r
  in
  if !need_paren then Format.fprintf fmt ")"

open Prec

let print_r fmt =
  Format.fprintf fmt "[@ %a@ ]" (print_r (-1))

let cons_opinfo = format_of_string "::@,", 1, Right
let print prec assoc fmt t =
  match t with
  | Regexp r -> print_r fmt r
  | Basic union ->
    let print_line prec assoc fmt l =
      match l with
      | Nil -> Format.fprintf fmt "[]"
      | Cons (elt,tl) ->
        fprintf prec assoc cons_opinfo fmt "%a::%a"
          Printer.print_descr_atomic elt Printer.print_descr_atomic tl
    in
    print_cup print_line prec assoc fmt union

let printer_builder = Printer.builder ~to_t ~map ~print

let printer_params = Printer.{ aliases = [];  extensions = [(tag, printer_builder)]}

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
