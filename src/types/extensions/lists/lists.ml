open Sstt_core
open Sstt_utils

let tag = TagComp.Tag.mk "lst"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
  |> TagComp.as_atom |> snd

let cons hd tl = [hd;tl] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag

let nil = [] |> Descr.mk_tuple |> Ty.mk_descr |> add_tag

let any =
  let v = Var.mk "" in
  let def = Ty.cup nil (cons Ty.any (Ty.mk_var v)) in
  Ty.of_eqs [(v,def)] |> List.hd |> snd |> Transform.simplify

let any_non_empty = cons Ty.any any

let destruct ty =
  let union =
    proj_tag ty |> Ty.get_descr |> Descr.get_tuples
    |> Tuples.get 2 |> Op.TupleComp.as_union
  in
  union |> List.map (fun comps -> match comps with
  | [elt;tl] -> elt, tl
  | _ -> assert false)

let destruct' ty =
  try
    let comps =
      proj_tag ty |> Ty.get_descr |> Descr.get_tuples
      |> Tuples.get 2 |> Op.TupleComp.approx
    in
    match comps with
    | [elt;tl] -> elt, tl
    | _ -> assert false
  with Op.EmptyAtom -> Ty.empty, Ty.empty

let basic_extract ty =
  let open Printer in
  if Ty.leq ty (proj_tag any) && Ty.vars_toplevel ty |> VarSet.is_empty then
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps = Tuples.get 0 tuples |> Op.TupleComp.as_union
      |> List.map (fun _ -> { tag_case_id=0 ; tag_params=[] }) in
    let cons_comps = Tuples.get 2 tuples |> Op.TupleComp.as_union
      |> List.map (fun tys -> { tag_case_id=1 ; tag_params=List.map (fun ty -> PLeaf ty) tys }) in
    Some (nil_comps@cons_comps)
  else None

let extract ty =
  let open Printer in
  try
    if Ty.vars_toplevel ty |> VarSet.is_empty |> not then raise Exit ;
    let pair = TupleComp.any 2 |> Descr.mk_tuplecomp |> Ty.mk_descr in
    let nil = TupleComp.any 0 |> Descr.mk_tuplecomp |> Ty.mk_descr in
    if Ty.leq ty (Ty.cup pair nil) |> not then raise Exit ;
    let tuples = Ty.get_descr ty |> Descr.get_tuples in
    let nil_comps =
      Tuples.get 0 tuples |> Op.TupleComp.as_union
      |> List.map (fun _ -> { tag_case_id=2 ; tag_params=[] })
    in
    let cons_comps =
      Tuples.get 2 tuples |> Op.TupleComp.as_union
      |> List.map (fun pair ->
        match pair with
        | [l;r] ->
          let ty = proj_tag r in
          if Ty.equiv r (Descr.mk_tag (tag, ty) |> Ty.mk_descr) |> not then raise Exit ;
          { tag_case_id=3 ; tag_params=[PLeaf l ; PRec ty] }
        | _ -> assert false  
      )
    in
    Some (nil_comps@cons_comps)
  with Exit -> None

type params_r = RNode of Printer.NodeId.t * params_rd list | RLoop of Printer.NodeId.t
and params_rd = RCons of Printer.descr * params_r | RNil

type basic = Nil | Cons of Printer.descr * Printer.descr
type params = R of params_r | B of basic list

let to_params tstruct =
  let open Printer in
  let rec regexp tstruct =
    match tstruct with
    | CNode nid -> RLoop nid
    | CDef (nid, union) ->
      let to_r params =
        match params with
        | { case_id=2 ; params=[]} -> RNil
        | { case_id=3 ; params=[PLeaf elt ; PRec tstruct]} -> RCons (elt, regexp tstruct)
        | _ -> raise Exit
      in
      RNode (nid, List.map to_r union)
  in
  let basic tstruct =
    match tstruct with
    | CDef (_, union) ->
      let to_b params =
        match params with
        | { case_id=0 ; params=[]} -> Nil
        | { case_id=1 ; params=[PLeaf elt ; PLeaf tl]} -> Cons (elt, tl)
        | _ -> assert false
      in
      List.map to_b union
    | _ -> assert false
  in
  try R (regexp tstruct)
  with Exit -> B (basic tstruct)

module Lt = struct
  open Printer
  type t = descr
  let equal d1 d2 = Ty.equiv d1.ty d2.ty
end
module Automaton = Automaton.Make(Lt)
module NIMap = Map.Make(Printer.NodeId)

let to_automaton params_r =
  let auto = Automaton.create () in
  let rec aux env t =
    match t with
    | RNode (nid, ds) ->
      let state = Automaton.mk_state auto in
      let env = NIMap.add nid state env in
      let treat_d d =
        match d with
        | RNil -> Automaton.set_final auto state
        | RCons (d, t) ->
          let state' = aux env t in
          Automaton.add_trans auto state d state'
      in
      List.iter treat_d ds ; state
    | RLoop nid -> NIMap.find nid env
  in
  let state = aux NIMap.empty params_r in
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

let rec convert_regexp (r: Printer.descr Regexp.t_ext) =
  match r with
  | EEpsilon -> Epsilon
  | ELetter l -> Symbol l
  | EConcat rs -> Concat (List.map convert_regexp rs)
  | EUnion rs -> Union (List.map convert_regexp rs)
  | EStar r -> Star (convert_regexp r)
  | EOption r -> Option (convert_regexp r)
  | EPlus r -> Plus (convert_regexp r)

let to_regexp automaton =
  automaton |> Automaton.to_regexp |> convert_regexp

let to_t t =
  match to_params t with
  | R r -> Regexp (r |> to_automaton |> to_regexp)
  | B bs -> Basic bs 

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
      Format.fprintf fmt "("
    end
  in
  let () = match regexp with
  | Epsilon -> ()
  | Symbol d -> Format.fprintf fmt "%a" Printer.print_descr_atomic d
  | Concat lst ->
    paren prec_concat ;
    Format.fprintf fmt "%a" (print_seq (print_r prec_concat) " ") lst
  | Union lst ->
    paren prec_union ;
    Format.fprintf fmt "%a" (print_seq (print_r prec_union) " | ") lst
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

type printer = Format.formatter -> t -> unit

let print_r fmt =
  Format.fprintf fmt "[ %a ]" (print_r (-1))

let print fmt t =
  match t with
  | Regexp r -> print_r fmt r
  | Basic union ->
    let print_line fmt l =
      match l with
      | Nil -> Format.fprintf fmt "[]"
      | Cons (elt,tl) ->
        Format.fprintf fmt "%a::%a" Printer.print_descr_atomic elt Printer.print_descr_atomic tl
    in
    Format.fprintf fmt "(%a)" (print_seq print_line " | ") union

let to_printer (print:printer) tstruct fmt =
  print fmt (to_t tstruct)

let printer_params printer = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, extract) ; (tag, basic_extract)] ;
  Printer.printers = [(tag, to_printer printer)]
  }

let printer_params' = printer_params print

let basic_printer_params = {
  Printer.aliases = [] ;
  Printer.tags = [(tag, basic_extract)] ;
  Printer.printers = [(tag, to_printer print)]
}

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
