open Sstt_core
open Sstt_utils

module NodeId = struct
  type t = { id : int ; mutable name : string option }
  let next_id =
    let c = ref 0 in
    fun () -> c := !c + 1 ; !c
  let mk () = { id = next_id() ; name = None }
  let has_name t = t.name <> None
  let name t = Option.get t.name
  let rename t name = t.name <- Some name
  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = compare t1.id t2.id
  let equal t1 t2 = (t1.id = t2.id)
  let pp fmt t =
    match t.name with
    | None -> Format.fprintf fmt "(%i)" t.id
    | Some str -> Format.fprintf fmt "%s" str
end

type unop =
| PNeg
type binop =
| PCup | PCap | PDiff | PArrow
type varop =
| PTuple
type builtin =
| PEmpty | PAny | PAnyTuple | PAnyAtom | PAnyTag | PAnyInt
| PAnyArrow | PAnyRecord | PAnyTupleComp of int | PAnyTagComp of TagComp.Tag.t
type t = descr * defs list
and defs = NodeId.t * descr
and descr = op * Ty.t
and op =
| PNamed of string
| PNode of NodeId.t
| PBuiltin of builtin
| PVar of Var.t
| PAtom of Atoms.Atom.t
| PTag of TagComp.Tag.t * descr
| PCustomTag of TagComp.Tag.t * descr list
| PInterval of Z.t option * Z.t option
| PRecord of (Label.t * descr * bool) list * bool
| PVarop of varop * descr list
| PBinop of binop * descr * descr
| PUnop of unop * descr

type aliases = (Ty.t * string) list
type custom_tags = (TagComp.Tag.t * (TagComp.t -> Ty.t list option)) list
type params = { aliases : aliases ; tags : custom_tags }

module VD = VDescr
module D = Descr
module VDMap = Map.Make(VD)

module NIMap = Map.Make(NodeId)
module NISet = Set.Make(NodeId)
module TagMap = Map.Make(TagComp.Tag)

let map_descr f d = (* Assumes f preserves semantic equivalence *)
  let rec aux (d,n) =
    let d = match d with
    | PNamed str -> PNamed str
    | PNode n -> PNode n
    | PBuiltin b -> PBuiltin b
    | PVar v -> PVar v
    | PAtom atom -> PAtom atom
    | PTag (tag, d) -> PTag (tag, aux d)
    | PCustomTag (tag, ds) -> PCustomTag (tag, List.map aux ds)
    | PInterval (lb, ub) -> PInterval (lb, ub)
    | PRecord (bindings, b) ->
      PRecord (List.map (fun (l,d,b) -> l, aux d, b) bindings, b)
    | PVarop (v, ds) -> PVarop (v, List.map aux ds)
    | PBinop (b, d1, d2) -> PBinop (b, aux d1, aux d2)
    | PUnop (u, d) -> PUnop (u, aux d)
    in (f (d,n), n)
  in
  aux d

let nodes_in_descr d =
  let res = ref NISet.empty in
  let f (d,_) = match d with
  | PNode n -> res := NISet.add n !res ; PNode n
  | d -> d
  in
  map_descr f d |> ignore ;
  !res

let size_of_descr d =
  let res = ref 0 in
  let f (d,_) = res := !res + 1 ; d in
  map_descr f d |> ignore ;
  !res

let map_t f (d,defs) =
  let defs = defs |> List.map (fun (id,d) -> (id,map_descr f d)) in
  (map_descr f d, defs)

let size_t (d,defs) =
  List.fold_left (fun n (_,d) ->
    n + 3 + size_of_descr d
  ) (size_of_descr d) defs

let subst n (d,_) t =
  let subst (d',_) =
    match d' with
    | PNode n' when NodeId.equal n n' -> d
    | d' -> d'
  in
  map_t subst t

let neg (d,n) =
  match d with
  | PUnop (PNeg, (d,n)) -> d, n
  | PBuiltin PEmpty -> PBuiltin PAny, Ty.neg n
  | PBuiltin PAny -> PBuiltin PEmpty, Ty.neg n
  | d -> PUnop (PNeg, (d,n)), Ty.neg n

let cap (d1,n1) (d2,n2) =
  if Ty.leq n1 n2 then d1,n1
  else if Ty.leq n2 n1 then d2,n2
  else PBinop (PCap, (d1,n1), (d2,n2)), Ty.cap n1 n2

let cup (d1,n1) (d2,n2) =
  if Ty.leq n2 n1 then d1,n1
  else if Ty.leq n1 n2 then d2,n2
  else PBinop (PCup, (d1,n1), (d2,n2)), Ty.cup n1 n2

let any = PBuiltin PAny, Ty.any
let empty = PBuiltin PEmpty, Ty.empty

let union union =
  match union with
  | [] -> empty
  | d::union -> List.fold_left cup d union

let inter inter =
  match inter with
  | [] -> any
  | d::inter -> List.fold_left cap d inter

let arrow (d1,n1) (d2,n2) =
  PBinop (PArrow, (d1,n1), (d2,n2)), D.mk_arrow (n1,n2) |> Ty.mk_descr

let tuple lst =
  let (_,ns) = List.split lst in
  PVarop (PTuple, lst), D.mk_tuple ns |> Ty.mk_descr

let record bindings opened =
  let nbindings = bindings |> List.map (fun (l, (_,n), b) ->
    (l,(n,b))
  ) |> LabelMap.of_list in
  PRecord (bindings, opened), D.mk_record { bindings=nbindings ; opened } |> Ty.mk_descr

let tag tag (o,n) =
  PTag (tag,(o,n)), D.mk_tag (tag,n) |> Ty.mk_descr

let atom a =
  PAtom a, D.mk_atom a |> Ty.mk_descr

let interval (o1, o2) =
  PInterval (o1, o2), Intervals.Atom.mk o1 o2 |> D.mk_interval |> Ty.mk_descr

(* Step 1 : Build the initial ctx and AST *)

type ctx = {
  mutable nodes : NodeId.t VDMap.t ;
  aliases : (Ty.t * op) list ;
  tags : (TagComp.t -> Ty.t list option) TagMap.t
}

let node ctx n =
  let def = Ty.def n in
  match VDMap.find_opt def ctx.nodes with
  | Some nid -> PNode nid, n
  | None ->
    let nid = NodeId.mk () in
    ctx.nodes <- VDMap.add def nid ctx.nodes ;
    PNode nid, n

let build_t (params:params) n =
  let aliases = params.aliases |> List.map (fun (n, s) -> (n, PNamed s)) in
  let tags = params.tags |> TagMap.of_list in
  let ctx = { nodes=VDMap.empty ; aliases ; tags } in
  ctx, (node ctx n, [])

(* Step 2 : Resolve missing definitions (and recognize custom type aliases) *)

let resolve_alias ctx n =
  begin match List.find_opt (fun (n',_) -> Ty.equiv n n') ctx.aliases with
  | None ->
    begin match List.find_opt (fun (n',_) -> Ty.equiv n (Ty.neg n')) ctx.aliases with
    | None -> None
    | Some (n, d) -> Some (neg (d, n))
    end
  | Some (n, d) -> Some (d, n)
  end

let resolve_arrows ctx a =
  let dnf = Arrows.dnf a |> Arrows.Dnf.simplify in
  let resolve_arr (n1, n2) =
    let d1, d2 = node ctx n1, node ctx n2 in
    arrow d1 d2
  in
  let resolve_dnf (ps, ns, _) =
    let ps = ps |> List.map resolve_arr in
    let ns = ns |> List.map resolve_arr |> List.map neg in
    ps@ns |> inter
  in
  dnf |> List.map resolve_dnf |> union

let resolve_atoms _ a =
  let (pos, atoms) = Atoms.destruct a in
  let atoms = atoms |> List.map atom |> union in
  if pos then atoms else neg atoms

let resolve_intervals _ a =
  let pos =
    Intervals.destruct a |> List.map Intervals.Atom.get
    |> List.map interval |> union
  in
  let neg =
    Intervals.destruct_neg a |> List.map Intervals.Atom.get
    |> List.map interval |> union |> neg
  in
  if size_of_descr neg < size_of_descr pos then neg else pos

let resolve_tuplecomp ctx a =
  let n = Tuples.mk_comp a |> D.mk_tuples |> Ty.mk_descr in
  match resolve_alias ctx n with
  | Some d -> d
  | None ->
    let dnf = TupleComp.dnf a |> TupleComp.Dnf.simplify in
    let resolve_tup lst =
      tuple (lst |> List.map (node ctx))
    in
    let resolve_dnf (ps, ns, _) =
      let ps = ps |> List.map resolve_tup in
      let ns = ns |> List.map resolve_tup |> List.map neg in
      ps@ns |> inter
    in
    dnf |> List.map resolve_dnf |> union

let resolve_tuples ctx a =
  let (pos, components) = Tuples.destruct a in
  let d = components |> List.map (fun p ->
    let len = TupleComp.len p in
    let elt = resolve_tuplecomp ctx p in
    cap (PBuiltin (PAnyTupleComp len),
        TupleComp.any len |> D.mk_tuplecomp |> Ty.mk_descr) elt
  ) |> union in
  if pos then d else neg d

let resolve_records ctx a =
  let open Records.Atom in
  let dnf = Records.dnf a |> Records.Dnf.simplify in
  let resolve_rec r =
    let bindings = r.bindings |> LabelMap.bindings |> List.map (fun (l,(n,b)) ->
      (l, node ctx n, b)
    ) in
    record bindings r.opened
  in
  let resolve_dnf (ps, ns, _) =
    let ps = ps |> List.map resolve_rec in
    let ns = ns |> List.map resolve_rec |> List.map neg in
    ps@ns |> inter
  in
  dnf |> List.map resolve_dnf |> union

let resolve_tagcomp ctx a =
  let n = Tags.mk_comp a |> D.mk_tags |> Ty.mk_descr in
  match resolve_alias ctx n with
  | Some d -> d
  | None ->
    let t = TagComp.tag a in
    let custom_nodes = TagMap.find_opt t ctx.tags
      |> Option.fold ~none:None ~some:(fun f -> f a) in
    begin match custom_nodes with
    | Some lst -> (PCustomTag (t, List.map (node ctx) lst), n)
    | None ->
      let dnf = TagComp.dnf a |> TagComp.Dnf.simplify in
      let resolve_tag (t, n) = tag t (node ctx n) in
      let resolve_dnf (ps, ns, _) =
        let ps = ps |> List.map resolve_tag in
        let ns = ns |> List.map resolve_tag |> List.map neg in
        ps@ns |> inter
      in
      dnf |> List.map resolve_dnf |> union
    end

let resolve_tags ctx a =
  let (pos, components) = Tags.destruct a in
  let d = components |> List.map (fun t ->
    let tag = TagComp.tag t in
    let elt = resolve_tagcomp ctx t in
    cap (PBuiltin (PAnyTagComp tag),
        TagComp.any tag |> D.mk_tagcomp |> Ty.mk_descr) elt
  ) |> union in
  if pos then d else neg d

let resolve_comp ctx c =
  let n = D.of_component c |> Ty.mk_descr in
  let alias = resolve_alias ctx n in
  let alias_or f c =
    match alias with
    | None -> f ctx c
    | Some d -> d
  in
  match c with
  | D.Atoms c ->
    alias_or resolve_atoms c,
    (PBuiltin PAnyAtom, Atoms.any () |> D.mk_atoms |> Ty.mk_descr)
  | D.Arrows c ->
    alias_or resolve_arrows c,
    (PBuiltin PAnyArrow, Arrows.any () |> D.mk_arrows |> Ty.mk_descr)
  | D.Intervals c ->
    alias_or resolve_intervals c,
    (PBuiltin PAnyInt, Intervals.any () |> D.mk_intervals |> Ty.mk_descr)
  | D.Tags c ->
    alias_or resolve_tags c,
    (PBuiltin PAnyTag, Tags.any () |> D.mk_tags |> Ty.mk_descr)
  | D.Tuples c ->
    alias_or resolve_tuples c,
    (PBuiltin PAnyTuple, Tuples.any () |> D.mk_tuples |> Ty.mk_descr)
  | D.Records c ->
    alias_or resolve_records c,
    (PBuiltin PAnyRecord, Records.any () |> D.mk_records |> Ty.mk_descr)

let resolve_descr ctx d =
  let n = VD.mk_descr d |> Ty.of_def in
  match resolve_alias ctx n with
  | None ->
    let ds = D.components d |> List.map (resolve_comp ctx) in
    let combine ds =
      ds |> List.map (fun (d,any_d) -> cap any_d d) |> union
    in
    let pd = ds |> combine in
    let nd = ds |> List.map (fun (d, any_d) -> (neg d, any_d)) |> combine |> neg in
    if size_of_descr nd < size_of_descr pd then nd else pd
  | Some d -> d

let resolve_def ctx def =
  let n = Ty.of_def def in
  match resolve_alias ctx n with
  | None ->
    let dnf = def |> VD.dnf |> VD.Dnf.simplify in
    let resolve_var v = PVar v, Ty.mk_var v in
    let resolve_dnf (ps, ns, d) =
      let ps = ps |> List.map resolve_var in
      let ns = ns |> List.map resolve_var |> List.map neg in
      let d = resolve_descr ctx d in
      ps@ns@[d] |> inter
    in
    dnf |> List.map resolve_dnf |> union
  | Some d -> d

let rec resolve_missing_defs ctx t =
  let used_defs = ctx.nodes |> VDMap.bindings in
  let (main_descr,defs) = t in
  let to_define = used_defs |> List.find_opt (fun (_,nid) ->
    defs |> List.exists (fun (nid',_) -> NodeId.equal nid nid') |> not
  ) in
  match to_define with
  | None -> t
  | Some (def,nid) ->
    let descr = resolve_def ctx def in
    resolve_missing_defs ctx (main_descr, (nid,descr)::defs)

(* Step 3 : Inline nodes when relevant, remove unused nodes *)

let used_nodes (descr, defs) =
  let res = nodes_in_descr descr in
  let rec aux nodes =
    let add_nodes nodes (n, d) =
      if NISet.mem n nodes
      then NISet.union nodes (nodes_in_descr d)
      else nodes
    in
    let nodes' = List.fold_left add_nodes nodes defs in
    if NISet.equal nodes nodes' then nodes' else aux nodes'
  in
  aux res

let remove_unused_nodes (descr, defs) =
  let used = used_nodes (descr, defs) in
  let defs = defs |> List.filter (fun (n,_) -> NISet.mem n used) in
  (descr, defs)

let inline t =
  let t = remove_unused_nodes t in
  let rec aux t =
    let size = size_t t in
    let rec try_inline defs =
      match defs with
      | [] -> None
      | (n,d)::defs ->
        let t' = subst n d t |> remove_unused_nodes in
        if size_t t' < size
        then Some t'
        else try_inline defs
    in
    match try_inline (snd t) with
    | None -> t
    | Some t' -> aux t'
  in
  aux t

(* Step 4 : Syntactic simplifications *)

let simplify t =
  let f (d,_) = match d with
  | PBinop (PCap, d1, (PUnop (PNeg, d2), _)) -> PBinop (PDiff, d1, d2)
  | d -> d
  in
  map_t f t

(* Step 5 : Rename nodes *)

let rename_nodes (_, defs) =
  let c = ref 0 in
  let next_name () =
    c := !c + 1 ;
    "x"^(string_of_int !c)
  in
  defs |> List.iter (fun (n,_) ->
    NodeId.rename n (next_name ())
    )

(* Step 6 : Print *)

type tags_printer =
  (TagComp.Tag.t * descr list * (NodeId.t * descr) list ->
    Format.formatter -> unit) list

let print_builtin fmt b =
  let str =
    match b with
    | PEmpty -> "empty"
    | PAny -> "any"
    | PAnyTuple -> "tuple"
    | PAnyAtom -> "atom"
    | PAnyTag -> "tag"
    | PAnyInt -> "int"
    | PAnyArrow -> "arrow"
    | PAnyRecord -> "record"
    | PAnyTupleComp i -> "tuple"^(string_of_int i)
    | PAnyTagComp t -> (TagComp.Tag.name t)^"(any)"
  in
  Format.fprintf fmt "%s" str

let pp_z = Z.pp_print
let print_interval fmt (lb,ub) =
  match lb, ub with
  | None, None -> print_builtin fmt PAnyInt
  | Some lb, Some ub when Z.equal lb ub ->
    Format.fprintf fmt "%a" pp_z lb
  | Some lb, Some ub ->
    Format.fprintf fmt "(%a..%a)" pp_z lb pp_z ub
  | None, Some ub ->
    Format.fprintf fmt "(..%a)" pp_z ub
  | Some lb, None ->
    Format.fprintf fmt "(%a..)" pp_z lb

type assoc = Left | Right | NoAssoc

let varop_info v = match v with
| PTuple -> ", ", 0, NoAssoc

let binop_info b = match b with
| PArrow -> "->", 1, Right
| PCup -> "|", 2, Left
| PCap -> "&", 3, Left
| PDiff -> "\\", 4, Left

let unop_info u = match u with
| PNeg -> "~", 5, NoAssoc

let rec print_descr prec assoc fmt (d,_) =
  let need_paren = ref false in
  let paren prec' assoc' =
    if prec' < prec || prec' = prec && (assoc' <> assoc || assoc' = NoAssoc)
    then begin
      need_paren := true ;
      Format.fprintf fmt "("
    end
  in
  let () = match d with
  | PNamed str -> Format.fprintf fmt "%s" str
  | PNode n -> Format.fprintf fmt "%a" NodeId.pp n
  | PBuiltin b -> print_builtin fmt b
  | PVar v -> Format.fprintf fmt "%a" Var.pp v
  | PAtom a -> Format.fprintf fmt "%a" Atoms.Atom.pp a
  | PTag (t,d) ->
    Format.fprintf fmt "%a(%a)"
      TagComp.Tag.pp t (print_descr (-1) NoAssoc) d
  | PCustomTag _ -> failwith "TODO"
  | PInterval (lb,ub) -> Format.fprintf fmt "%a" print_interval (lb,ub)
  | PRecord (bindings,opened) ->
    let print_binding fmt (l,d,b) =
      Format.fprintf fmt "%a %s %a"
        Label.pp l
        (if b then ":?" else ":")
        (print_descr (-1) NoAssoc) d
    in
    Format.fprintf fmt "{ %a %s}"
      (print_seq print_binding " ; ")
      bindings
      (if opened then ".." else "")
  | PVarop (v,ds) ->
    let sym,prec',assoc' = varop_info v in
    paren prec' assoc' ;
    Format.fprintf fmt "%a"
      (print_seq (print_descr prec' NoAssoc) sym)
      ds
  | PBinop (b,d1,d2) ->
    let sym,prec',assoc' = binop_info b in
    paren prec' assoc' ;
    Format.fprintf fmt "%a %s %a"
      (print_descr prec' Left) d1 sym
      (print_descr prec' Right) d2
  | PUnop (u,d) ->
    let sym,prec',assoc' = unop_info u in
    paren prec' assoc' ;
    Format.fprintf fmt "%s%a" sym (print_descr prec' NoAssoc) d
  in
  if !need_paren then Format.fprintf fmt ")"

let print_descr = print_descr (-1) NoAssoc

let print_def fmt (n,d) =
  Format.fprintf fmt "%a = %a" NodeId.pp n print_descr d

let print_t fmt (d,defs) =
  Format.fprintf fmt "%a" print_descr d ;
  match defs with
  | [] -> ()
  | def::defs ->
    Format.fprintf fmt " where %a%a" print_def def
      (print_seq print_def " and ") defs

(* MAIN *)

let get customs ty =
  let (ctx, t) = build_t customs ty in
  let t = resolve_missing_defs ctx t in
  let t = inline t in
  let t = simplify t in
  rename_nodes t ;
  t

let print = print_t

let print_ty customs fmt ty =
  let ast = get customs ty in
  Format.fprintf fmt "%a" print ast

let print_subst customs fmt s =
  let print_ty = print_ty customs in
  let pp_binding fmt (v,ty) =
    Format.fprintf fmt "@,%a: %a" Var.pp v print_ty ty
  in
  Format.fprintf fmt "@[<v 0>[@[<v 1>%a@]@,]@]"
    (print_seq pp_binding " ;") (Subst.bindings s)

let empty_params : params = { aliases = [] ; tags = [] }
let print_ty' = print_ty empty_params
let print_subst' = print_subst empty_params
