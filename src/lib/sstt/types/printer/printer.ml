open Core
open Prec


module NodeId = struct
  type t = { id : int ; mutable name : string option }
  let next_id =
    let c = ref 0 in
    fun () -> c := !c + 1 ; !c
  let mk () = { id = next_id() ; name = None }
  let has_name t = t.name <> None
  let name t = Option.get t.name
  let rename t name = t.name <- Some name
  let hash t = Hash.int t.id
  let compare t1 t2 = compare t1.id t2.id
  let equal t1 t2 = (t1.id = t2.id)
  let pp fmt t =
    match t.name with
    | None -> Format.fprintf fmt "(%i)" t.id
    | Some str -> Format.fprintf fmt "%s" str
end

module VD = VDescr
module D = Descr
module NISet = Set.Make(NodeId)
module VDHash = Hashtbl.Make(VD)
module TagMap = Map.Make(Tag)

type builtin =
  | Empty | Any | AnyTuple | AnyEnum | AnyTag | AnyInt
  | AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of Tag.t

type descr = { op : op ; ty : Ty.t }
and op =
  | Extension of extension_node
  | Alias of string
  | Node of NodeId.t
  | Builtin of builtin
  | Var of Var.t
  | Enum of Enum.t
  | Tag of Tag.t * descr
  | Interval of Z.t option * Z.t option
  | Record of (Label.t * fdescr) list * fdescr
  | Varop of varop * descr list
  | Binop of binop * descr * descr
  | Unop of unop * descr
and fdescr = { fop : fop ; fty : Ty.F.t }
and fop =
  | FVarop of fvarop * fdescr list
  | FBinop of fbinop * fdescr * fdescr
  | FUnop of funop * fdescr
  | FTy of descr * bool
  | FRowVar of RowVar.t
and extension_node = E : {
    value : 'a;
    map : (descr -> descr) -> (fdescr -> fdescr) -> 'a -> 'a;
    print : int -> assoc -> Format.formatter -> 'a -> unit
  } -> extension_node
type def = NodeId.t * descr
type 'm t = { main : 'm ; defs : def list }

(* Extensions *)
type aliases = (Ty.t * string) list
type extension_builder = ctx -> TagComp.t -> extension_node option

and extensions = (Tag.t * extension_builder) list
and params = { aliases : aliases ; extensions : extensions }
and ctx = {
  nodes : NodeId.t VDHash.t ;
  aliases : (Ty.t * op) list ;
  extensions : extension_builder TagMap.t
}


let rec map_fdescr f ff fd = (* Assumes f and ff preserve semantic equivalence *)
  let rec aux fd =
    let fop = match fd.fop with
      | FVarop (v, fops) -> FVarop (v, List.map aux fops)
      | FBinop (b, fop1, fop2) -> FBinop (b, aux fop1, aux fop2)
      | FUnop (u, fop) -> FUnop (u, aux fop)
      | FTy (d, b) -> FTy (map_descr f ff d, b)
      | FRowVar v -> FRowVar v
    in { fd with fop = ff { fd with fop } }
  in
  aux fd

and map_descr f ff d = (* Assumes f and ff preserve semantic equivalence *)
  let rec aux d =
    let op = match d.op with
      | Extension (E e) ->
        Extension (E{ e with value = e.map aux (map_fdescr f ff) e.value } )
      | Alias str -> Alias str
      | Node n -> Node n
      | Builtin b -> Builtin b
      | Var v -> Var v
      | Enum enum -> Enum enum
      | Tag (tag, d) -> Tag (tag, aux d)
      | Interval (lb, ub) -> Interval (lb, ub)
      | Record (bindings, fop) ->
        Record (
          List.map (fun (l,fop) -> l, map_fdescr f ff fop) bindings,
          map_fdescr f ff fop)
      | Varop (v, ds) -> Varop (v, List.map aux ds)
      | Binop (b, d1, d2) -> Binop (b, aux d1, aux d2)
      | Unop (u, d) -> Unop (u, aux d)
    in { d with op = f { d with op } }
  in
  aux d

let map_t map_m f ff t =
  let main = map_m (map_descr f ff) (map_fdescr f ff) t.main in
  let defs = t.defs |> List.map (fun (id,d) -> id,map_descr f ff d) in
  { main ; defs }

let nodes_in_descr d =
  let res = ref NISet.empty in
  let f d = match d.op with
    | Node n -> res := NISet.add n !res ; Node n
    | op -> op
  in
  map_descr f (fun fd -> fd.fop) d |> ignore ;
  !res
let nodes_in_fdescr fd =
  let res = ref NISet.empty in
  let f d = match d.op with
    | Node n -> res := NISet.add n !res ; Node n
    | op -> op
  in
  map_fdescr f (fun fd -> fd.fop) fd |> ignore ;
  !res

let nodes_in_t map_m t =
  let res = ref NISet.empty in
  let _ = map_m
    (fun d -> res := NISet.union (!res) (nodes_in_descr d) ; d)
    (fun fd -> res := NISet.union (!res) (nodes_in_fdescr fd) ; fd)
    t.main in
  t.defs |> List.map (fun (_,d) -> nodes_in_descr d) |> List.fold_left NISet.union !res

let size_of_descr d =
  let res = ref 0 in
  let f d = res := !res + 1 ; d.op in
  let ff fd = res := !res + 1 ; fd.fop in
  map_descr f ff d |> ignore ;
  !res
let size_of_fdescr d =
  let res = ref 0 in
  let f d = res := !res + 1 ; d.op in
  let ff fd = res := !res + 1 ; fd.fop in
  map_fdescr f ff d |> ignore ;
  !res

let size_t map_m t =
  let res = ref 0 in
  let _ = map_m
    (fun d -> res := !res + (size_of_descr d) ; d)
    (fun fd -> res := !res + (size_of_fdescr fd) ; fd)
    t.main in
  t.defs |> List.map (fun (_,d) -> size_of_descr d + 3) |> List.fold_left (+) !res

let subst map_m n d t =
  let aux d' =
    match d'.op with
    | Node n' when NodeId.equal n n' -> d.op
    | _ -> d'.op
  in
  map_t map_m aux (fun fd -> fd.fop) t

let neg d =
  match d.op with
  | Unop (Neg, d) -> d
  | Builtin Empty -> { op = Builtin Any ; ty = Ty.neg d.ty }
  | Builtin Any -> { op = Builtin Empty ; ty = Ty.neg d.ty }
  | _ -> { op = Unop (Neg, d) ; ty = Ty.neg d.ty }
let fneg fd =
  match fd.fop with
  | FUnop (FNeg, fd) -> fd
  | _ -> { fop = FUnop (FNeg, fd) ; fty = Ty.F.neg fd.fty }

let cap d1 d2 =
  let ty = Ty.cap d1.ty d2.ty in
  match d1.op, d2.op with
  | Varop (Cap, c1), Varop (Cap, c2) ->
    { op = Varop (Cap, c1@c2) ; ty }
  | Varop (Cap, c1), _ -> { op = Varop (Cap, c1@[d2]) ; ty }
  | _, Varop (Cap, c2) -> { op = Varop (Cap, d1::c2) ; ty }
  | _, _ -> { op = Varop (Cap, [d1;d2]) ; ty }
let fcap fd1 fd2 =
  let fty = Ty.F.cap fd1.fty fd2.fty in
  match fd1.fop, fd2.fop with
  | FVarop (FCap, c1), FVarop (FCap, c2) ->
    { fop = FVarop (FCap, c1@c2) ; fty }
  | FVarop (FCap, c1), _ -> { fop = FVarop (FCap, c1@[fd2]) ; fty }
  | _, FVarop (FCap, c2) -> { fop = FVarop (FCap, fd1::c2) ; fty }
  | _, _ -> { fop = FVarop (FCap, [fd1;fd2]) ; fty }

let cup d1 d2 =
  let ty = Ty.cup d1.ty d2.ty in
  match d1.op, d2.op with
  | Varop (Cup, c1), Varop (Cup, c2) ->
    { op = Varop (Cup, c1@c2) ; ty }
  | Varop (Cup, c1), _ -> { op = Varop (Cup, c1@[d2]) ; ty }
  | _, Varop (Cup, c2) -> { op = Varop (Cup, d1::c2) ; ty }
  | _, _ -> { op = Varop (Cup, [d1;d2]) ; ty }
let fcup fd1 fd2 =
  let fty = Ty.F.cup fd1.fty fd2.fty in
  match fd1.fop, fd2.fop with
  | FVarop (FCup, c1), FVarop (FCup, c2) ->
    { fop = FVarop (FCup, c1@c2) ; fty }
  | FVarop (FCup, c1), _ -> { fop = FVarop (FCup, c1@[fd2]) ; fty }
  | _, FVarop (FCup, c2) -> { fop = FVarop (FCup, fd1::c2) ; fty }
  | _, _ -> { fop = FVarop (FCup, [fd1;fd2]) ; fty }

let cap' d1 d2 =
  if Ty.leq d1.ty d2.ty then d1
  else if Ty.leq d2.ty d1.ty then d2
  else cap d1 d2

let cup' d1 d2 =
  if Ty.leq d1.ty d2.ty then d2
  else if Ty.leq d2.ty d1.ty then d1
  else cup d1 d2

let fcap' fd1 fd2 =
  if Ty.F.leq fd1.fty fd2.fty then fd1
  else if Ty.F.leq fd2.fty fd1.fty then fd2
  else fcap fd1 fd2

let fcup' fd1 fd2 =
  if Ty.F.leq fd1.fty fd2.fty then fd2
  else if Ty.F.leq fd2.fty fd1.fty then fd1
  else fcup fd1 fd2

let any = { op = Builtin Any ; ty = Ty.any }
let empty = { op = Builtin Empty ; ty = Ty.empty }
let fany = { fop = FTy (any, true) ; fty = Ty.F.any }
let fempty = { fop = FTy (empty, false) ; fty = Ty.F.empty }

let union union =
  let union = union |> List.filter (fun d -> Ty.is_empty d.ty |> not) in
  match union with
  | [] -> empty
  | d::union -> List.fold_left cup d union

let inter inter =
  let inter = inter |> List.filter (fun d -> Ty.is_any d.ty |> not) in
  match inter with
  | [] -> any
  | d::inter -> List.fold_left cap d inter

let funion union =
  let union = union |> List.filter (fun fd -> Ty.F.is_empty fd.fty |> not) in
  match union with
  | [] -> fempty
  | fd::union -> List.fold_left fcup fd union

let finter inter =
  let inter = inter |> List.filter (fun fd -> Ty.F.is_any fd.fty |> not) in
  match inter with
  | [] -> fany
  | fd::inter -> List.fold_left fcap fd inter

let arrow d1 d2 =
  { op = Binop (Arrow, d1, d2) ; ty = D.mk_arrow (d1.ty, d2.ty) |> Ty.mk_descr }

let tuple lst =
  let tys = List.map (fun d -> d.ty) lst in
  { op = Varop (Tuple, lst) ; ty = D.mk_tuple tys |> Ty.mk_descr }

let record bindings tail =
  let nbindings = bindings |>
                  List.map (fun (l, fd) -> (l, fd.fty)) |> LabelMap.of_list in
  { op = Record (bindings, tail) ;
    ty = { bindings=nbindings ; tail=tail.fty } |> D.mk_record |> Ty.mk_descr }

let tag tag d =
  { op = Tag (tag,d) ; ty = D.mk_tag (tag, d.ty) |> Ty.mk_descr }

let enum a =
  { op = Enum a ; ty = D.mk_enum a |> Ty.mk_descr }

let var v =
  { op = Var v ; ty = Ty.mk_var v }

let interval (o1, o2) =
  { op = Interval (o1, o2) ;
    ty = Intervals.Atom.mk o1 o2 |> D.mk_interval |> Ty.mk_descr }

let fvar rv =
  { fop = FRowVar rv ; fty = Ty.F.mk_var rv }

let fty (d,b) =
  let fty = Ty.F.mk_descr (Ty.O.mk (d.ty,b)) in
  { fop = FTy (d,b) ; fty }

(* Step 1 : Build the initial ctx and AST *)

let empty_params = { aliases = []; extensions = [] }
let merge_params l = List.fold_left (fun acc p ->
    {  aliases = acc.aliases @ p.aliases;
       extensions = acc.extensions @ p.extensions }) empty_params l

let node ctx ty =
  let def = Ty.def ty in
  match VDHash.find_opt ctx.nodes def  with
  | Some nid -> { op = Node nid ; ty }
  | None ->
    let nid = NodeId.mk () in
    VDHash.add ctx.nodes def nid  ;
    { op = Node nid ; ty }

let build_ctx params =
  let aliases = params.aliases |> List.map (fun (ty, s) -> (ty, Alias s)) in
  let extensions = params.extensions |> TagMap.of_list in
  { nodes=VDHash.create 16 ; aliases ; extensions }

(* Step 2 : Resolve missing definitions (and recognize Ext type aliases) *)

let resolve_alias (ctx:ctx) any_ty ty =
  begin match List.find_opt (fun (ty',_) -> Ty.equiv (Ty.cap any_ty ty) ty') ctx.aliases with
    | None ->
      begin match List.find_opt (fun (ty',_) -> Ty.equiv (Ty.diff any_ty ty) ty') ctx.aliases with
        | None -> None
        | Some (ty, op) -> Some (neg { op ; ty })
      end
    | Some (ty, op) -> Some { op ; ty }
  end

let resolve_line fa (ps, ns) =
  let ps = ps |> List.map fa in
  let ns = ns |> List.map fa |> List.map neg in
  ps@ns |> inter
let resolve_dnf fa dnf =
  dnf |> List.map (resolve_line fa) |> union

let resolve_arrows ctx a =
  let resolve_arr (n1, n2) =
    let d1, d2 = node ctx n1, node ctx n2 in
    arrow d1 d2
  in
  Arrows.dnf a |> resolve_dnf resolve_arr

let resolve_enums _ a =
  let (pos, enums) = Enums.destruct a in
  let enums = enums |> List.map enum |> union in
  if pos then enums else neg enums

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
  let ty = Tuples.mk_comp a |> D.mk_tuples |> Ty.mk_descr in
  let len = TupleComp.len a in
  let any = TupleComp.any len |> D.mk_tuplecomp |> Ty.mk_descr in
  let any_d = { op = Builtin (AnyTupleComp len) ; ty = any } in
  match resolve_alias ctx any ty with
  | Some d -> d, any_d
  | None ->
    let resolve_tup lst = tuple (lst |> List.map (node ctx)) in
    TupleComp.dnf a |> resolve_dnf resolve_tup, any_d

let resolve_tuples ctx a =
  let (pos, components) = Tuples.destruct a in
  let d = components |> List.map (fun p ->
      let elt, any = resolve_tuplecomp ctx p in
      cap' any elt
    ) |> union in
  if pos then d else neg d

let resolve_field nf f =
  let aux_oty oty =
    let ty, b = Ty.O.get oty in
    fty (nf ty, b)
  in
  let aux_ps ps = ps |> List.map fvar in
  let aux_ns ns = ns |> List.map fvar |> List.map fneg in
  let aux_line (ps, ns, oty) =
    let fops, fops' = aux_ps ps, aux_ns ns in
    let fd = match aux_oty oty with
      | { fty ; _ } when Ty.F.is_any fty -> []
      | fd -> [fd]
    in
    fops@fops'@fd |> finter
  in
  Ty.F.dnf f |> List.map aux_line |> funion

let resolve_records ctx a =
  let open Records.Atom in
  let resolve_rec r =
    let bindings = r.bindings |> LabelMap.bindings
                   |> List.map (fun (l,f) -> (l, resolve_field (node ctx) f)) in
    record bindings (resolve_field (node ctx) r.tail)
  in
  Records.dnf a |> resolve_dnf resolve_rec

let resolve_tagcomp_default ctx a =
  let resolve_tag (t, ty) = tag t (node ctx ty) in
  TagComp.dnf a |> resolve_dnf resolve_tag

let resolve_tagcomp ctx a =
  let ty = Tags.mk_comp a |> D.mk_tags |> Ty.mk_descr in
  let tag = TagComp.tag a in
  let any = TagComp.any tag |> D.mk_tagcomp |> Ty.mk_descr in
  let any_d = { op = Builtin (AnyTagComp tag) ; ty = any } in
  match resolve_alias ctx any ty with
  | Some d -> d, any_d
  | None ->
    match TagMap.find_opt (TagComp.tag a) ctx.extensions with
      None -> resolve_tagcomp_default ctx a, any_d
    | Some f ->
      match f ctx a with
        None -> resolve_tagcomp_default ctx a, any_d
      | Some e ->
        { op = Extension e; ty }, { op = Builtin Any ; ty = Ty.any }

let resolve_tags ctx a =
  let (pos, components) = Tags.destruct a in
  let d = components |> List.map (fun p ->
      let elt, any = resolve_tagcomp ctx p in
      cap' any elt
    ) |> union in
  if pos then d else neg d

let resolve_comp ctx c =
  let ty = D.of_component c |> Ty.mk_descr in
  let any, any_op = match c with
    | D.Enums _ ->
      Enums.any |> D.mk_enums |> Ty.mk_descr, Builtin AnyEnum
    | D.Arrows _ ->
      Arrows.any |> D.mk_arrows |> Ty.mk_descr, Builtin AnyArrow
    | D.Intervals _ ->
      Intervals.any |> D.mk_intervals |> Ty.mk_descr, Builtin AnyInt
    | D.Tags _ ->
      Tags.any |> D.mk_tags |> Ty.mk_descr, Builtin AnyTag
    | D.Tuples _ ->
      Tuples.any |> D.mk_tuples |> Ty.mk_descr, Builtin AnyTuple
    | D.Records _ ->
      Records.any |> D.mk_records |> Ty.mk_descr, Builtin AnyRecord
  in
  let any_d = { op = any_op ; ty = any } in
  let alias = resolve_alias ctx any ty in
  let alias_or f c =
    match alias with
    | None -> f ctx c
    | Some d -> d
  in
  let d = match c with
    | D.Enums c -> alias_or resolve_enums c
    | D.Arrows c -> alias_or resolve_arrows c
    | D.Intervals c -> alias_or resolve_intervals c
    | D.Tags c -> alias_or resolve_tags c
    | D.Tuples c -> alias_or resolve_tuples c
    | D.Records c -> alias_or resolve_records c
  in
  d, any_d

let resolve_descr ctx d =
  let ty = VD.mk_descr d |> Ty.of_def in
  match resolve_alias ctx Ty.any ty with
  | None ->
    let (pos, components) = Descr.destruct d in
    let d = components |> List.map (fun p ->
        let elt, any = resolve_comp ctx p in
        cap' any elt
      ) |> union in
    if pos then d else neg d
  | Some d -> d

let resolve_def ctx def =
  let ty = Ty.of_def def in
  match resolve_alias ctx Ty.any ty with
  | None ->
    let resolve_dnf (ps, ns, d) =
      let ps = ps |> List.map var in
      let ns = ns |> List.map var |> List.map neg in
      let d = resolve_descr ctx d in
      ps@ns@[d] |> inter
    in
    def |> VD.dnf |> List.map resolve_dnf |> union
  | Some d -> d

let rec resolve_missing_defs ctx defs =
  let used_defs = ctx.nodes |> VDHash.to_seq |> List.of_seq in
  let to_define = used_defs |> List.find_opt (fun (_,nid) ->
      defs |> List.exists (fun (nid',_) -> NodeId.equal nid nid') |> not
    ) in
  match to_define with
  | None -> defs
  | Some (def,nid) ->
    let descr = resolve_def ctx def in
    resolve_missing_defs ctx ((nid,descr)::defs)

(* Step 3 : Inline nodes when relevant, remove unused nodes *)

let used_nodes map_m t =
  let res = nodes_in_t map_m { main=t.main ; defs=[] } in
  let rec aux nodes =
    let add_nodes nodes (n, d) =
      if NISet.mem n nodes
      then NISet.union nodes (nodes_in_descr d)
      else nodes
    in
    let nodes' = List.fold_left add_nodes nodes t.defs in
    if NISet.equal nodes nodes' then nodes' else aux nodes'
  in
  aux res

let remove_unused_nodes map_m t =
  let used = used_nodes map_m t in
  let defs = t.defs |> List.filter (fun (n,_) -> NISet.mem n used) in
  { t with defs }

let inline' map_m metric t =
  let t = remove_unused_nodes map_m t in
  let rec aux t =
    let size = metric t in
    let rec try_inline defs =
      match defs with
      | [] -> None
      | (n,d)::defs ->
        let t' = subst map_m n d t |> remove_unused_nodes map_m in
        if metric t' < size
        then Some t'
        else try_inline defs
    in
    match try_inline t.defs with
    | None -> t
    | Some t' -> aux t'
  in
  aux t

let inline_mid map_m t = inline' map_m (size_t map_m) t
let inline_max map_m t =
  let nb_defs t = List.length t.defs in
  inline' map_m nb_defs t

(* Step 4 : Syntactic simplifications *)

let simplify map_m t =
  let f d =
    match d.op with
    | Varop (Cap, [ d ; { op = Unop (Neg, dn) ; _ } ]) -> Binop (Diff, d, dn)
    | op -> op
  in
  let ff fd =
    match fd.fop with
    | FVarop (FCap, [ fd ; { fop = FUnop (FNeg, dn) ; _ } ]) -> FBinop (FDiff, fd, dn)
    | fop -> fop
  in
  map_t map_m f ff t

(* Step 5 : Rename nodes *)

module StrSet = Set.Make(String)
let names map_m t =
  let res = ref StrSet.empty in
  let f d = match d.op with
    | Alias str -> res := StrSet.add str !res ; Alias str
    | Node nid when NodeId.has_name nid ->
      res := StrSet.add (NodeId.name nid) !res ; Node nid
    | Var v -> res := StrSet.add (Var.name v) !res ; Var v
    | Enum e -> res := StrSet.add (Enum.name e) !res ; Enum e
    | op -> op
  in
  let ff df = match df.fop with
    | FRowVar rv -> res := StrSet.add (RowVar.name rv) !res ; FRowVar rv
    | fop -> fop
  in
  let _ = map_t map_m f ff t in !res
let rename_nodes map_m t =
  let names = names map_m t in
  let c = ref 0 in
  let rec next_name () =
    c := !c + 1 ;
    let res = "x"^(string_of_int !c) in
    if StrSet.mem res names then next_name () else res
  in
  t.defs |> List.iter (fun (n,_) ->
      NodeId.rename n (next_name ())
    )

(* Step 6 : Print *)

let print_builtin fmt b =
  let str =
    match b with
    | Empty -> "empty"
    | Any -> "any"
    | AnyTuple -> "tuple"
    | AnyEnum -> "enum"
    | AnyTag -> "tag"
    | AnyInt -> "int"
    | AnyArrow -> "arrow"
    | AnyRecord -> "record"
    | AnyTupleComp i -> "tuple"^(string_of_int i)
    | AnyTagComp t -> (Tag.name t)^("()")
  in
  Format.fprintf fmt "%s" str

let pp_z = Z.pp_print
let print_interval fmt (lb,ub) =
  match lb, ub with
  | None, None -> print_builtin fmt AnyInt
  | Some lb, Some ub when Z.equal lb ub ->
    Format.fprintf fmt "%a" pp_z lb
  | Some lb, Some ub ->
    Format.fprintf fmt "(%a..%a)" pp_z lb pp_z ub
  | None, Some ub ->
    Format.fprintf fmt "(..%a)" pp_z ub
  | Some lb, None ->
    Format.fprintf fmt "(%a..)" pp_z lb

let print_extension_node_ctx prec assoc fmt (E e) =
  e.print prec assoc fmt e.value

let rec print_descr prec assoc fmt d =
  let rec aux prec assoc fmt d =
    let open Format in
    match d.op with
    | Extension e -> print_extension_node_ctx prec assoc fmt e
    | Alias str -> fprintf fmt "%s" str
    | Node n -> fprintf fmt "%a" NodeId.pp n
    | Builtin b -> print_builtin fmt b
    | Var v -> fprintf fmt "%a" Var.pp v
    | Enum a -> fprintf fmt "%a" Enum.pp a
    | Tag (t,d) ->
      fprintf fmt "%a(%a)"
        Tag.pp t print_descr' d
    | Interval (lb,ub) -> fprintf fmt "%a" print_interval (lb,ub)
    | Record (bindings,tail) ->
      let print_binding fmt (l,f) =
        Format.fprintf fmt "%a :@ %a"
          Label.pp l
          print_fdescr' f
      in
      Format.fprintf fmt "{@ %a@ %a}"
        (Prec.print_seq print_binding " ;@ ") bindings
        print_tail tail
    | Varop (v,ds) -> Prec.print_nary_op aux prec assoc v fmt ds
    | Binop (b,d1,d2) -> Prec.print_binary_op aux prec assoc b fmt d1 d2
    | Unop (u,d) -> Prec.print_unary_op aux prec assoc u fmt d
  in
  aux prec assoc fmt d

and print_fdescr prec assoc fmt fd =
  let rec aux prec assoc fmt fd =
    match fd.fop with
    | FRowVar v -> Format.fprintf fmt "%a" RowVar.pp v
    | FTy (d, opt) ->
      if opt then
        Format.fprintf fmt "%a?" (print_descr max_prec NoAssoc) d
      else
        print_descr prec assoc fmt d
    | FVarop (v,fops) -> Prec.print_nary_fop aux prec assoc v fmt fops
    | FBinop (b,fop1,fop2) -> Prec.print_binary_fop aux prec assoc b fmt fop1 fop2
    | FUnop (u,fop) -> Prec.print_unary_fop aux prec assoc u fmt fop
  in
  aux prec assoc fmt fd

and print_tail fmt tail =
  match tail with
  | {fty ; _} when Ty.F.equiv fty Ty.F.any -> Format.fprintf fmt ".."
  | {fty ; _} when Ty.F.equiv fty (Ty.F.mk_descr Ty.O.absent) -> Format.fprintf fmt ""
  | _ -> Format.fprintf fmt ";;@ %a@ " print_fdescr' tail

and print_descr' fmt d = print_descr min_prec NoAssoc fmt d
and print_fdescr' fmt fop = print_fdescr min_prec NoAssoc fmt fop

and print_def fmt (n,d) =
  Format.fprintf fmt "%a =@ %a" NodeId.pp n print_descr' d

and print_t fmt t =
  Format.fprintf fmt "%a" print_descr' t.main ;
  match t.defs with
  | [] -> ()
  | defs ->
    Format.fprintf fmt "@ where@ %a" (print_seq print_def "@ and@ ") defs

(* MAIN *)
type build_ctx = { build : Ty.t -> descr ; build_field : Ty.F.t -> fdescr }
type 'a map = (descr -> descr) -> (fdescr -> fdescr) -> 'a -> 'a
let builder ~to_t ~map ~print =
  (fun ctx ty ->
     let build_ctx = { build=node ctx ; build_field=resolve_field (node ctx) } in
     match to_t build_ctx ty with
       None -> None
     | Some value -> Some (E {value; map; print})
  )

let get' ?(factorize=false) customs tys =
  let map_m f _ main = List.map f main in
  let ctx = build_ctx customs in
  let main = List.map (node ctx) tys in
  let defs = resolve_missing_defs ctx [] in
  let t = { main ; defs } in
  let t = if factorize then inline_mid map_m t else inline_max map_m t in
  let t = simplify map_m t in
  rename_nodes map_m t; t

let get ?(factorize=false) customs ty =
  match get' ~factorize customs [ty] with
  | { main=[d] ; defs } -> { main=d ; defs }
  | _ -> assert false

let get_field' ?(factorize=false) customs ftys =
  let map_m _ ff main = List.map ff main in
  let ctx = build_ctx customs in
  let main = List.map (resolve_field (node ctx)) ftys in
  let defs = resolve_missing_defs ctx [] in
  let t = { main ; defs } in
  let t = if factorize then inline_mid map_m t else inline_max map_m t in
  let t = simplify map_m t in
  rename_nodes map_m t; t

let get_field ?(factorize=false) customs fty =
  match get_field' ~factorize customs [fty] with
  | { main=[d] ; defs } -> { main=d ; defs }
  | _ -> assert false

let print = print_t
let print_descr_atomic = print_descr max_prec NoAssoc
let print_descr, print_descr_ctx, print_field_ctx = print_descr', print_descr, print_fdescr

let print_ty customs fmt ty =
  let ast = get customs ty in
  Format.fprintf fmt "%a" print ast

let print_row customs fmt r =
  let bindings, tail = Row.bindings r, Row.tail r in
  let tail, fields, defs =
    match get_field' customs (tail::List.map snd bindings) with
    | { main=tl::bindings ; defs } -> tl, bindings, defs
    | _ -> assert false
  in
  let bindings = List.combine (List.map fst bindings) fields in
  let ast = { main={ ty=Ty.any ; op=Record (bindings, tail) } ; defs } in
  print fmt ast

let print_subst customs fmt s =
  let print_ty = print_ty customs in
  let print_row = print_row customs in
  let pp_binding1 fmt (v,ty) =
    Format.fprintf fmt "@,@[<hov>%a: %a@]" Var.pp v print_ty ty
  in
  let pp_binding2 fmt (v,r) =
    Format.fprintf fmt "@,@[<hov>%a: %a@]" RowVar.pp v print_row r
  in
  let pp_binding' fmt b =
    match b with
    | `T (v,ty) -> pp_binding1 fmt (v,ty)
    | `R (v,r) -> pp_binding2 fmt (v,r)
  in
  let b1 = Subst.bindings1 s |> List.map (fun b -> `T b) in
  let b2 = Subst.bindings2 s |> List.map (fun b -> `R b) in
  Format.fprintf fmt "@[<v 0>[@[<v 1>%a@]@,]@]"
    (print_seq pp_binding' " ;") (b1@b2)

let print_ty' = print_ty empty_params
let print_subst' = print_subst empty_params
let print_row' = print_row empty_params

let cap_descr = cap'
let cup_descr = cup'
let neg_descr = neg
let cap_fdescr = fcap'
let cup_fdescr = fcup'
let neg_fdescr = fneg

let map_descr = map_descr
let map_fdescr = map_fdescr
let map = map_t (fun d _ -> d)
let map_f = map_t (fun _ fd -> fd)
