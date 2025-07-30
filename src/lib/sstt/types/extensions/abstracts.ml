open Core
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
  let vs = Hashtbl.find atypes tag in
  let ty = encode_params vs ps in
  (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let mk_any tag = (tag, Ty.any) |> Descr.mk_tag |> Ty.mk_descr

let is_abstract tag = Hashtbl.mem atypes tag
let params_of tag = Hashtbl.find atypes tag

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
  if Ty.equiv ty ty' then Some res else None

let destruct tag ty =
  match Hashtbl.find_opt atypes tag with
  | None -> None
  | Some vs ->
    begin match extract_params vs ty with
    | None -> None
    | Some ts -> Some ts
    end

let extract tag ty =
  let open Printer in
  match destruct tag ty with
  | None -> None
  | Some ps ->
    let ps = ps |> List.mapi (fun i (ps, ns) ->
      let ps = ps |> List.map (fun tys ->
        { pid=[i;0] ; pdef=List.map (fun ty -> PLeaf ty) tys }
      ) in
      let ns = ns |> List.map (fun tys ->
        { pid=[i;1] ; pdef=List.map (fun ty -> PLeaf ty) tys }
      ) in
      { pid=[i] ; pdef=[] }::ps@ns
    ) |> List.flatten in
    Some ps

let destruct comp =
  let (tag, ty) = TagComp.as_atom comp in
  Option.map (fun x -> (tag, x)) (destruct tag ty)

type params = Printer.descr list
type t = (params list * params list) list

let to_t tstruct : t =
  let open Printer in
  let aux_i defs i =
    let ps, ns = defs |> List.filter_map (fun {pid;pdef} ->
      match pid with
      | [j;neg] ->
        if j = i then
          Some (neg=0, List.map (function PLeaf d -> d | _ -> assert false) pdef)
        else None
      | [_] -> None
      | _ -> assert false
    ) |> List.partition fst in
    List.map snd ps, List.map snd ns
  in
  match tstruct with
  | CDef (_, defs) ->
    let ids = defs |> List.filter_map
      (fun {pid;_} -> match pid with [i] -> Some i | _ -> None) in
    List.map (aux_i defs) ids
  | _ -> assert false

open Prec

type printer = Tag.t -> int -> assoc -> Format.formatter -> t -> unit

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

let define printer name (vs:variance list) =
  let tag = Tag.mk name in
  Hashtbl.add atypes tag vs ;
  let printer_params = {
    Printer.aliases = [] ;
    Printer.extensions =
      let module M = struct
        type nonrec t = t
        let tag = tag
        let extractors = [extract tag]
        let get = to_t
        let print = printer tag
      end
    in [(module M : Printer.PrinterExt)]
  } in
  tag, printer_params

let define' = define print
