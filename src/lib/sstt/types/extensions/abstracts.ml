open Core
open Sstt_utils

type 't params = 't list
type 't t = ('t params list * 't params list) list

module THT = Hashtbl.Make(Tag)
let abs_tags = THT.create 10

let is_abstract tag = THT.mem abs_tags tag
let check_abstract tag =
  if is_abstract tag |> not then
    invalid_arg 
      (Format.asprintf "Undefined abstract type '%a'" Tag.pp tag)
let arity tag =
  check_abstract tag ; THT.find abs_tags tag

let define name n =
  let abs = Tag.mk' name Tag.NoProperty in
  THT.add abs_tags abs n ; abs

let labels = Hashtbl.create 10
let label_of_position i =
  match Hashtbl.find_opt labels i with
  | Some lbl -> lbl
  | None ->
    let lbl = Label.mk (string_of_int i) in
    Hashtbl.add labels i lbl ; lbl

let encode_params ps =
  let bindings = ps |> List.mapi (fun i p ->
      (label_of_position i, Ty.O.optional p)
    ) |> LabelMap.of_list
  in
  { Records.Atom.bindings ; Records.Atom.opened=false }
  |> Descr.mk_record |> Ty.mk_descr

let mk tag ps =
  let n = arity tag in
  if List.length ps <> n then
    invalid_arg (Format.asprintf "Wrong arity for '%a'" Tag.pp tag) ;
  (tag, encode_params ps) |> Descr.mk_tag |> Ty.mk_descr

let mk_any tag =
  check_abstract tag ;
  TagComp.any tag |> Descr.mk_tagcomp |> Ty.mk_descr

let extract_dnf tag dnf =
  let open Records.Atom in
  let n = arity tag in
  let extract_param record i =
    find (label_of_position i) record |> Ty.O.get
  in
  let extract_params record =
    List.init n Fun.id |> List.map (extract_param record)
  in
  let extract_params (_, ty) =
    Ty.get_descr ty |> Descr.get_records
    |> Op.Records.approx |> extract_params
  in
  let res = dnf |> List.map (fun (ps, ns) ->
    (List.map extract_params ps, List.map extract_params ns)
  ) in
  (* We check that the encoding of the result is equivalent to the initial type [ty]
     (otherwise it means that [ty] is not a valid encoding of an abstract type) *)
  let build_from_dnf dnf =
    TagComp.of_dnf tag dnf |> Descr.mk_tagcomp |> Ty.mk_descr
  in
  let ty = build_from_dnf dnf in
  let ty' =
    res |> List.map (fun (ps, ns) ->
        (ps |> List.map (fun ty -> tag, encode_params ty),
         ns |> List.map (fun ty -> tag, encode_params ty))
      ) |> build_from_dnf
  in
  if Ty.equiv ty ty' then res else invalid_arg "Malformed abstract type"

let destruct tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
  |> TagComp.dnf |> extract_dnf tag

let to_t node ctx comp =
  try
    let tag, dnf = TagComp.tag comp, TagComp.dnf comp in
    let params = extract_dnf tag dnf in
    let map_node l = List.map (node ctx) l in
    List.map (fun (p1, p2) ->
        List.map map_node p1, List.map map_node p2
      ) params |> Option.some
  with _ -> None

let map f l =
  l |> List.map (fun (p1, p2) ->
      (List.map (List.map f) p1,
       List.map (List.map f) p2)
    )

open Prec

let print tag prec assoc fmt t =
  let print_atom fmt params =
    let sym,prec',_ = varop_info Tuple in
    Format.fprintf fmt "%a(%a)" Tag.pp tag
      (print_seq (Printer.print_descr_ctx prec' NoAssoc) sym) params
  in
  let print_lit prec assoc fmt (pos,params) =
    if pos then
      print_atom fmt params
    else
      let sym,_,_ as opinfo = unop_info Neg in
      fprintf prec assoc opinfo fmt "%s%a" sym print_atom params
  in
  let print_line prec assoc fmt (ps, ns) =
    let ps, ns = List.map (fun d -> true, d) ps, List.map (fun d -> false, d) ns in
    let sym,prec',_ as opinfo = varop_info Cap in
    fprintf prec assoc opinfo fmt "%s%s%a"
      (if ps = [] then Tag.name tag else "")
      (if ps = [] && ns <> [] then sym else "")
      (print_seq (print_lit prec' NoAssoc) sym) (ps@ns)
  in
  let sym,prec',_ as opinfo = varop_info Cup in
  fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t

let printer_builder tag =
  Printer.builder ~to_t:to_t ~map ~print:(print tag)

let printer_params tag = Printer.{ aliases = []; extensions = [(tag, printer_builder tag)]}
