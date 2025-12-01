open Core
open Sstt_utils

type variance = Cov | Cav | Inv
type 't params = 't list
type 't t = ('t params list * 't params list) list

module THT = Hashtbl.Make(Tag)
type encoding =
  | EInv of int (* More optimized *)
  | ECov of variance list (* More general *)
let abs_tags = THT.create 10

let is_abstract tag = THT.mem abs_tags tag
let check_abstract tag =
  if is_abstract tag |> not then
    invalid_arg 
      (Format.asprintf "Undefined abstract type '%a'" Tag.pp tag)
let encoding tag =
  check_abstract tag ; THT.find abs_tags tag |> snd
let name tag =
  check_abstract tag ; THT.find abs_tags tag |> fst
let parameters tag =
  match encoding tag with
  | EInv n -> List.init n (fun _ -> Inv)
  | ECov vs -> vs
let arity tag = parameters tag |> List.length

let define name vs =
  if List.for_all ((=) Inv) vs then
    let n = List.length vs in
    let abs = Tag.mk' ("__"^name) Tag.NoProperty in
    THT.add abs_tags abs (name, EInv n) ; abs
  else
    let abs = Tag.mk' ("_"^name)
      (Tag.Monotonic { preserves_cap=false ; preserves_cup=false ; preserves_extremum=false }) in
    THT.add abs_tags abs (name, ECov vs) ; abs

let labels = Hashtbl.create 10
let label_of_position neg i =
  match Hashtbl.find_opt labels (neg,i) with
  | Some lbl -> lbl
  | None ->
    let lbl = Label.mk ("_"^(string_of_int i)^(if neg then "'" else "")) in
    Hashtbl.add labels (neg,i) lbl ; lbl

let encode_params encoding ps =
  let bindings =
    match encoding with
    | ECov vs ->
      List.combine vs ps |> List.mapi (fun i (v,p) ->
        let pos = label_of_position false i, Ty.O.optional p in
        let neg = label_of_position true i, Ty.O.optional (Ty.neg p) in
        match v with
        | Cov -> [pos] | Cav -> [neg] | Inv -> [pos;neg]
      ) |> List.concat |> Records.Atom.LabelMap.of_list
    | EInv _ ->
      ps |> List.mapi (fun i p ->
        label_of_position false i, Ty.O.optional p
      ) |> Records.Atom.LabelMap.of_list
  in
  { Records.Atom.bindings ; Records.Atom.opened=false }
  |> Descr.mk_record |> Ty.mk_descr

let mk tag ps =
  let encoding = encoding tag in
  if List.length ps <> arity tag then
    invalid_arg (Format.asprintf "Wrong arity for '%a'" Tag.pp tag) ;
  (tag, encode_params encoding ps) |> Descr.mk_tag |> Ty.mk_descr

let mk_any tag =
  check_abstract tag ;
  TagComp.any tag |> Descr.mk_tagcomp |> Ty.mk_descr

let extract_dnf tag dnf =
  let open Records.Atom in
  let vs = parameters tag in
  let extract_param record i v =
    match v with
    | Inv | Cov -> find (label_of_position false i) record |> Ty.O.get
    | Cav -> find (label_of_position true i) record |> Ty.O.get |> Ty.neg
  in
  let extract_params record =
    vs |> List.mapi (extract_param record)
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
  let encoding = encoding tag in
  let ty = build_from_dnf dnf in
  let ty' =
    res |> List.map (fun (ps, ns) ->
        (ps |> List.map (fun ty -> tag, encode_params encoding ty),
         ns |> List.map (fun ty -> tag, encode_params encoding ty))
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
    Format.fprintf fmt "%s(%a)" (name tag)
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
      (if ps = [] then name tag else "")
      (if ps = [] && ns <> [] then sym else "")
      (print_seq (print_lit prec' NoAssoc) sym) (ps@ns)
  in
  let sym,prec',_ as opinfo = varop_info Cup in
  fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t

let printer_builder tag =
  Printer.builder ~to_t:to_t ~map ~print:(print tag)

let printer_params tag = Printer.{ aliases = []; extensions = [(tag, printer_builder tag)]}
