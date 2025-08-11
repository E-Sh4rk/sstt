open Core
open Sstt_utils

type variance = Cov | Contrav | Inv
type 't params = 't list
type 't t = ('t params list * 't params list) list

let atypes = Hashtbl.create 256
let is_abstract tag = Hashtbl.mem atypes tag
let check_abstract tag =
  if not (is_abstract tag) then
    invalid_arg 
      (Format.asprintf "Unregistered abstract type '%a'" Tag.pp tag)
let labels = Hashtbl.create 10
let label_of_position i =
  match Hashtbl.find_opt labels i with
  | Some lbl -> lbl
  | None ->
    let lbl = Label.mk (string_of_int i) in
    Hashtbl.add labels i lbl ; lbl

let encode_params vs ps =
  let (ls, rs) =
    List.combine vs ps |> List.mapi (fun i (v,p) ->
        let lbl = label_of_position i in
        let constr = (lbl, Ty.O.optional p) in
        let noconstr = (lbl, Ty.O.any) in
        match v with
        | Cov -> noconstr, constr
        | Contrav -> constr, noconstr
        | Inv -> constr, constr
      ) |> List.split
  in
  let mk_record bs =
    let open Records.Atom in
    { bindings = bs |> LabelMap.of_list ; opened = true }
    |> Descr.mk_record |> Ty.mk_descr
  in
  let lhs, rhs = mk_record ls, mk_record rs in
  Descr.mk_arrow (lhs,rhs) |> Ty.mk_descr

let mk tag ps =
  check_abstract tag;
  let vs = Hashtbl.find atypes tag in
  let ty = encode_params vs ps in
  (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let mk_any tag = (tag, Ty.any) |> Descr.mk_tag |> Ty.mk_descr

let is_abstract tag = Hashtbl.mem atypes tag
let params_of tag =
  check_abstract tag;
  Hashtbl.find atypes tag
let extract_params vs ty =
  let n = List.length vs in
  let convert_to_tuple r =
    let open Records.Atom in
    List.init n (fun i -> find (label_of_position i) r |> fst)
  in
  let extract_tuples ty =
    Ty.get_descr ty |> Descr.get_records |> Op.Records.as_union |>
    List.map convert_to_tuple
  in
  let extract_tuple ty =
    Ty.get_descr ty |> Descr.get_records |> Op.Records.approx |>
    convert_to_tuple
  in
  let aux (ls, rs) =
    let aux (v,(l,r)) =
      match v with
      | Cov -> r
      | Contrav -> l
      | Inv -> l
    in
    let tys = List.combine ls rs in
    List.combine vs tys |> List.map aux
  in
  let aux (l, r) =
    let uls, rs = extract_tuples l, extract_tuple r in
    uls |> List.map (fun ls -> (ls,rs)) |> List.map aux
  in
  let res = Ty.get_descr ty |> Descr.get_arrows |> Arrows.dnf |> Arrows.Dnf.simplify
            |> List.map (fun (ps, ns, _) ->
                List.concat_map aux ps,
                List.concat_map aux ns
              ) in
  (* We check that the encoding of the result is equivalent to the initial type [ty]
     (otherwise it means that [ty] is not a valid encoding of an abstract type) *)
  let ty' =
    res |> List.map (fun (ps, ns) ->
        let ps = ps |> List.map (encode_params vs) |> Ty.conj in
        let ns = ns |> List.map (encode_params vs) |> List.map Ty.neg |> Ty.conj in
        Ty.cap ps ns
      ) |> Ty.disj
  in
  if Ty.equiv ty ty' then res else invalid_arg "Malformed abstract type"

let proj_tag tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                      |> TagComp.as_atom |> snd

let destruct tag ty =
  check_abstract tag;
  let vs = Hashtbl.find atypes tag in
  extract_params vs ty

let destruct_tagcomp comp =
  let (tag, ty) = TagComp.as_atom comp in
  (tag, destruct tag ty)

let to_t tag node ctx ty =
  try 
    let params = destruct tag (proj_tag tag ty) in
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
  Printer.builder ~to_t:(to_t tag) ~map ~print:(print tag)

let printer_params tag = Printer.{ aliases = []; extensions = [(tag, printer_builder tag)]}

let define name (vs:variance list) =
  let tag = Tag.mk name in
  Hashtbl.add atypes tag vs ;
  tag, printer_params tag
