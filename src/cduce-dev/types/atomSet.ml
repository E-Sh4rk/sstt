open Encodings
module Symbol = Utf8

module V = struct
  include Ns.Label

  let print = print_tag
  let to_string = string_of_tag
end

type elem = V.t

module SymbolSet = SortedList.FiniteCofinite (V)

let rec iter_sep sep f = function
  | [] -> ()
  | [ h ] -> f h
  | h :: t ->
      f h;
      sep ();
      iter_sep sep f t

let print_symbolset ns ppf = function
  | SymbolSet.Finite l ->
      iter_sep (fun () -> Format.fprintf ppf " |@ ") (V.print_quote ppf) l
  | SymbolSet.Cofinite t ->
      Format.fprintf ppf "@[`%a" Ns.InternalPrinter.print_any_ns ns;
      List.iter (fun x -> Format.fprintf ppf " \\@ %a" V.print_quote x) t;
      Format.fprintf ppf "@]"

include SortedList.FiniteCofiniteMap (Ns.Uri) (SymbolSet)

let test m =
  match get m with
  | `Finite [] -> Tset.Empty
  | `Cofinite [] -> Tset.Full
  | _ -> Tset.Unknown

let neg x = diff any x
let atom l = atom (fst (V.value l), l)
let contains l t = contains (fst (V.value l), l) t

let single s =
  match get s with
  | `Finite [ (_, SymbolSet.Finite [ a ]) ] -> a
  | `Finite [] -> raise Not_found
  | _ -> raise Exit

let print_tag s =
  match get s with
  | `Finite [ (_, SymbolSet.Finite [ a ]) ] -> Some (fun ppf -> V.print ppf a)
  | `Finite [ (ns, SymbolSet.Cofinite []) ] ->
      Some (fun ppf -> Ns.InternalPrinter.print_any_ns ppf ns)
  | `Cofinite [] -> Some (fun ppf -> Format.fprintf ppf "_")
  | _ -> None

let print s =
  match get s with
  | `Finite l -> List.map (fun (ns, s) ppf -> print_symbolset ns ppf s) l
  | `Cofinite [] -> [ (fun ppf -> Format.fprintf ppf "Atom") ]
  | `Cofinite l ->
      [
        (fun ppf ->
          Format.fprintf ppf "Atom";
          List.iter
            (fun (ns, s) ->
              Format.fprintf ppf " \\@ (%a)" (print_symbolset ns) s)
            l);
      ]

type 'a map = 'a Imap.t * 'a Imap.t * 'a option

let map_map f (m1, m2, o) =
  ( Imap.map f m1,
    Imap.map f m2,
    match o with
    | Some x -> Some (f x)
    | None -> None )

(* TODO: optimize this get_map *)
let get_map q (mtags, mns, def) =
  try Imap.find mtags (Upool.int q) with
  | Not_found -> (
      try Imap.find mns (Upool.int (fst (V.value q))) with
      | Not_found -> (
          match def with
          | None -> assert false
          | Some x -> x))

let mk_map l =
  let all_ns = ref [] in
  let all_tags = ref [] in
  let def = ref None in
  List.iter
    (function
      | s, x -> (
          match get s with
          | `Finite s ->
              List.iter
                (function
                  | _, SymbolSet.Finite t ->
                      List.iter
                        (fun tag -> all_tags := (Upool.int tag, x) :: !all_tags)
                        t
                  | ns, _ -> all_ns := (Upool.int ns, x) :: !all_ns)
                s
          | `Cofinite _ -> def := Some x))
    l;

  let mtags = Imap.create (Array.of_list !all_tags) in
  let mns = Imap.create (Array.of_list !all_ns) in
  (mtags, mns, !def)

type sample = (Ns.Uri.t * Ns.Label.t option) option

let contains_sample s t =
  match (s, get t) with
  | None, `Cofinite _ -> true
  | None, `Finite _ -> false
  | Some (_, Some tag), _ -> contains tag t
  | Some (ns, None), _ -> is_empty (diff (any_in_ns ns) t)

let extract s =
  let tr l =
    List.map
      (fun (ns, ss) ->
        ( ns,
          match ss with
          | SymbolSet.Finite l -> `Finite l
          | SymbolSet.Cofinite l -> `Cofinite l ))
      l
  in
  match get s with
  | `Finite l -> `Finite (tr l)
  | `Cofinite l -> `Cofinite (tr l)

let is_finite s =
  match extract s with
  | `Finite l ->
      List.for_all
        (function
          | _, `Finite _ -> true
          | _, `Cofinite _ -> false)
        l
  | `Cofinite _ -> false
