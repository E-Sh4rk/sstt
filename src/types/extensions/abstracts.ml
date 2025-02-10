open Sstt_core
open Sstt_utils

type variance = Cov | Contrav | Inv

let atypes = Hashtbl.create 256

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
      let l, r = match v with
      | Cov -> Ty.empty, p
      | Contrav -> p, Ty.empty
      | Inv -> p, p
      in
      (lbl, (l,true)), (lbl, (r,true))
  ) |> List.split
  in
  let mk_record bs =
    let open Records.Atom in
    { bindings = bs |> LabelMap.of_list ; opened = false }
    |> Descr.mk_record |> Ty.mk_descr
  in
  let lhs, rhs = mk_record ls, mk_record rs in
  Descr.mk_arrow (lhs,rhs) |> Ty.mk_descr

let mk tag ps =
  let vs = Hashtbl.find atypes tag in
  let ty = encode_params vs ps in
  (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

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
    match extract_tuples ty with
    | [tup] -> tup
    | _ -> raise Exit
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
    List.map aux ps |> List.flatten,
    List.map aux ns |> List.flatten
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
  if Ty.equiv ty ty' |> not then raise Exit ;
  res

let extract tag ty =
  let open Printer in
  match Hashtbl.find_opt atypes tag with
  | None -> None
  | Some vs ->
    try
      let ps = extract_params vs ty in
      let cases = ps |> List.map (fun (ps, ns) ->
        let ps = ps |> List.map (fun tys ->
          { comp_id=0 ; comp_def=List.map (fun ty -> PLeaf ty) tys }
        ) in
        let ns = ns |> List.map (fun tys ->
          { comp_id=1 ; comp_def=List.map (fun ty -> PLeaf ty) tys }
        ) in
        { tag_case_id=0 ; tag_case_def=ps@ns }
      ) in
      Some cases
    with Exit -> None

type params = Printer.descr list
type t = (params list * params list) list

let to_t tstruct : t =
  let open Printer in
  let aux def =
    let (pos, def) =
      match def with
      | { comp_id = 0; comp_def } -> true, comp_def
      | { comp_id = 1; comp_def } -> false, comp_def
      | _ -> assert false
    in
    let def = def |> List.map (function PLeaf d -> d | _ -> assert false) in
    pos, def
  in
  let aux defs =
    let defs = List.map aux defs in
    let ps, ns = List.partition fst defs in
    List.map snd ps, List.map snd ns
  in
  match tstruct with
  | CDef (_, defs) ->
    defs |> List.map
      (function { case_id=0 ; case_def } -> aux case_def | _ -> assert false)
  | _ -> assert false

type printer = TagComp.Tag.t -> Format.formatter -> t -> unit

let print tag fmt t =
  let nb_lit ps = List.length ps in
  let nb_lit (ps,ns) = nb_lit ps + nb_lit ns in
  let nb_lit lst = List.map nb_lit lst |> List.fold_left (+) 0 in
  let print_lit fmt (pos,params) =
    Format.fprintf fmt "%s%a(%a)" (if pos then "" else "~") TagComp.Tag.pp tag
      (print_seq Printer.print_descr_atomic ", ") params
  in
  let print_line fmt (ps, ns) =
    let ps, ns = List.map (fun d -> true, d) ps, List.map (fun d -> false, d) ns in
    let params = ps@ns in
    Format.fprintf fmt "%a" (print_seq print_lit " & ") params
  in
  if nb_lit t > 1 then
    Format.fprintf fmt "(%a)" (print_seq print_line " | ") t
  else
    Format.fprintf fmt "%a" (print_seq print_line " | ") t

let to_printer (print:printer) tag tstruct fmt =
  print tag fmt (to_t tstruct)

let define printer name (vs:variance list) =
  let tag = TagComp.Tag.mk name in
  Hashtbl.add atypes tag vs ;
  let printer_params = {
    Printer.aliases = [] ;
    Printer.tags = [(tag, extract tag)] ;
    Printer.printers = [(tag, to_printer printer tag)]
    }
  in
  tag, printer_params

let define' = define print
