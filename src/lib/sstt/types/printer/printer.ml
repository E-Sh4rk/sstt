open Core
open Sstt_utils
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
  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = compare t1.id t2.id
  let equal t1 t2 = (t1.id = t2.id)
  let pp fmt t =
    match t.name with
    | None -> Format.fprintf fmt "(%i)" t.id
    | Some str -> Format.fprintf fmt "%s" str
end

module type CustomNode = sig
  type t
  val v : t
  val print : int -> assoc -> Format.formatter -> t -> unit
end

type builtin =
  | Empty | Any | AnyTuple | AnyEnum | AnyTag | AnyInt
  | AnyArrow | AnyRecord | AnyTupleComp of int | AnyTagComp of Tag.t
type 'c t' = { main : 'c descr' ; defs : 'c def' list }
and 'c def' = NodeId.t * 'c descr'
and 'c descr' = { op : 'c op' ; ty : Ty.t }
and 'c op' =
  | Custom of 'c
  | Alias of string
  | Node of NodeId.t
  | Builtin of builtin
  | Var of Var.t
  | Enum of Enums.Atom.t
  | Tag of Tag.t * 'c descr'
  | Interval of Z.t option * Z.t option
  | Record of (Label.t * 'c descr' * bool) list * bool
  | Varop of varop * 'c descr' list
  | Binop of binop * 'c descr' * 'c descr'
  | Unop of unop * 'c descr'

type t = (module CustomNode) t'
type descr = (module CustomNode) descr'
type def = (module CustomNode) def'
type op = (module CustomNode) op'

(* Printer extensions *)
type ('u, 'l, 'r) cparam = PUnprocessed of 'u | PLeaf of 'l | PRec of 'r
type ('u, 'l, 'r) cparams = { pid : int list ; pdef : ('u, 'l, 'r) cparam list }
type custom = CDef of NodeId.t * (Ty.t, descr, custom) cparams list | CNode of NodeId.t
type extracted_params = (Ty.t, Ty.t, Ty.t) cparams list
module type PrinterExt = sig
  type t
  val tag : Tag.t
  val extractors : (Ty.t -> extracted_params option) list
  val get : custom -> t
  val print : int -> assoc -> Format.formatter -> t -> unit
end
type custom' = CDef of NodeId.t * (Ty.t, (Tag.t * custom') descr', custom') cparams list | CNode of NodeId.t

type aliases = (Ty.t * string) list
type extensions = (module PrinterExt) list
type params = { aliases : aliases ; extensions : extensions }

module VD = VDescr
module D = Descr
module VDMap = Map.Make(VD)

module NIMap = Map.Make(NodeId)
module NISet = Set.Make(NodeId)
module TagMap = Map.Make(Tag)

let map_descr' fc f d = (* Assumes f preserves semantic equivalence *)
  let rec aux d =
    let op = match d.op with
      | Custom c -> Custom (fc aux c)
      | Alias str -> Alias str
      | Node n -> Node n
      | Builtin b -> Builtin b
      | Var v -> Var v
      | Enum enum -> Enum enum
      | Tag (tag, d) -> Tag (tag, aux d)
      | Interval (lb, ub) -> Interval (lb, ub)
      | Record (bindings, b) ->
        Record (List.map (fun (l,d,b) -> l, aux d, b) bindings, b)
      | Varop (v, ds) -> Varop (v, List.map aux ds)
      | Binop (b, d1, d2) -> Binop (b, aux d1, aux d2)
      | Unop (u, d) -> Unop (u, aux d)
    in { d with op = f { d with op } }
  in
  aux d
let map_ic map (tag, ts) =
  let rec map_ts ts =
    match ts with
    | CDef (nid,params) ->
      CDef (nid, List.map map_params params)
    | CNode nid -> CNode nid
  and map_params { pid ; pdef } =
    { pid ; pdef=List.map map_param pdef }
  and map_param p =
    match p with
    | PUnprocessed ty -> PUnprocessed ty
    | PLeaf d -> PLeaf (map d)
    | PRec ts -> PRec (map_ts ts)
  in
  (tag, map_ts ts)
let map_descr = map_descr' map_ic
let map_t' fc f t =
  let main = map_descr' fc f t.main in
  let defs = t.defs |> List.map (fun (id,d) -> (id,map_descr' fc f d)) in
  { main ; defs }
let map_t = map_t' map_ic

let nodes_in_descr d =
  let res = ref NISet.empty in
  let f d = match d.op with
    | Node n -> res := NISet.add n !res ; Node n
    | op -> op
  in
  map_descr f d |> ignore ;
  !res

let size_of_descr d =
  let res = ref 0 in
  let f d = res := !res + 1 ; d.op in
  map_descr f d |> ignore ;
  !res

let size_t t =
  List.fold_left (fun n (_,d) ->
      n + 3 + size_of_descr d
    ) (size_of_descr t.main) t.defs

let subst n d t =
  let aux d' =
    match d'.op with
    | Node n' when NodeId.equal n n' -> d.op
    | _ -> d'.op
  in
  map_t aux t

let neg d =
  match d.op with
  | Unop (Neg, d) -> d
  | Builtin Empty -> { op = Builtin Any ; ty = Ty.neg d.ty }
  | Builtin Any -> { op = Builtin Empty ; ty = Ty.neg d.ty }
  | _ -> { op = Unop (Neg, d) ; ty = Ty.neg d.ty }

let cap d1 d2 =
  let ty = Ty.cap d1.ty d2.ty in
  match d1.op, d2.op with
  | Varop (Cap, c1), Varop (Cap, c2) ->
    { op = Varop (Cap, c1@c2) ; ty }
  | Varop (Cap, c1), _ -> { op = Varop (Cap, c1@[d2]) ; ty }
  | _, Varop (Cap, c2) -> { op = Varop (Cap, d1::c2) ; ty }
  | _, _ -> { op = Varop (Cap, [d1;d2]) ; ty }

let cup d1 d2 =
  let ty = Ty.cup d1.ty d2.ty in
  match d1.op, d2.op with
  | Varop (Cup, c1), Varop (Cup, c2) ->
    { op = Varop (Cup, c1@c2) ; ty }
  | Varop (Cup, c1), _ -> { op = Varop (Cup, c1@[d2]) ; ty }
  | _, Varop (Cup, c2) -> { op = Varop (Cup, d1::c2) ; ty }
  | _, _ -> { op = Varop (Cup, [d1;d2]) ; ty }

let cap' d1 d2 =
  if Ty.leq d1.ty d2.ty then d1
  else if Ty.leq d2.ty d1.ty then d2
  else cap d1 d2

let cup' d1 d2 =
  if Ty.leq d1.ty d2.ty then d2
  else if Ty.leq d2.ty d1.ty then d1
  else cup d1 d2

let any = { op = Builtin Any ; ty = Ty.any }
let empty = { op = Builtin Empty ; ty = Ty.empty }

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

let arrow d1 d2 =
  { op = Binop (Arrow, d1, d2) ; ty = D.mk_arrow (d1.ty, d2.ty) |> Ty.mk_descr }

let tuple lst =
  let tys = List.map (fun d -> d.ty) lst in
  { op = Varop (Tuple, lst) ; ty = D.mk_tuple tys |> Ty.mk_descr }

let record bindings opened =
  let nbindings = bindings |>
                  List.map (fun (l, d, b) -> (l, (d.ty, b))) |> LabelMap.of_list in
  { op = Record (bindings, opened) ;
    ty = D.mk_record { bindings=nbindings ; opened } |> Ty.mk_descr }

let tag tag d =
  { op = Tag (tag,d) ; ty = D.mk_tag (tag, d.ty) |> Ty.mk_descr }

let enum a =
  { op = Enum a ; ty = D.mk_enum a |> Ty.mk_descr }

let var v =
  { op = Var v ; ty = Ty.mk_var v }

let interval (o1, o2) =
  { op = Interval (o1, o2) ;
    ty = Intervals.Atom.mk o1 o2 |> D.mk_interval |> Ty.mk_descr }

(* Step 1 : Build the initial ctx and AST *)

let empty_params : params = { aliases = [] ; extensions = [] }

let merge_params p1 p2 =
  { aliases = p1.aliases@p2.aliases ;
    extensions = p1.extensions@p2.extensions }

let merge_params = List.fold_left merge_params empty_params

type ctx = {
  mutable nodes : NodeId.t VDMap.t ;
  aliases : (Ty.t * (Tag.t * custom') op') list ;
  extensions : (module PrinterExt) TagMap.t ;
}

let node ctx ty =
  let def = Ty.def ty in
  match VDMap.find_opt def ctx.nodes with
  | Some nid -> { op = Node nid ; ty }
  | None ->
    let nid = NodeId.mk () in
    ctx.nodes <- VDMap.add def nid ctx.nodes ;
    { op = Node nid ; ty }

let tag_handlers ctx tag =
  match TagMap.find_opt tag ctx.extensions with
  | None -> []
  | Some m ->
    let module M = (val m : PrinterExt) in
    M.extractors

let build_t (params:params) ty =
  let aliases = params.aliases |> List.map (fun (ty, s) -> (ty, Alias s)) in
  let extensions = params.extensions |> List.map (fun m ->
      let module M = (val m : PrinterExt) in
      M.tag, m
    ) |> TagMap.of_list in
  let ctx = { nodes=VDMap.empty ; aliases ; extensions } in
  ctx, { main = node ctx ty ; defs = [] }

(* Step 2 : Resolve missing definitions (and recognize custom type aliases) *)

let resolve_alias ctx ty =
  begin match List.find_opt (fun (ty',_) -> Ty.equiv ty ty') ctx.aliases with
    | None ->
      begin match List.find_opt (fun (ty',_) -> Ty.equiv ty (Ty.neg ty')) ctx.aliases with
        | None -> None
        | Some (ty, op) -> Some (neg { op ; ty })
      end
    | Some (ty, op) -> Some { op ; ty }
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
  match resolve_alias ctx ty with
  | Some d -> d
  | None ->
    let dnf = TupleComp.dnf a |> TupleComp.Dnf.simplify in
    let resolve_tup lst = tuple (lst |> List.map (node ctx)) in
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
      let any = { op = Builtin (AnyTupleComp len) ;
                  ty = TupleComp.any len |> D.mk_tuplecomp |> Ty.mk_descr }
      in
      cap' any elt
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

let rec resolve_custom_tagcomp f ctx env ty =
  let vd = Ty.def ty in
  match VDMap.find_opt vd env with
  | Some nid -> CNode nid
  | None ->
    begin match f ty with
      | None -> raise Exit
      | Some extracted ->
        let nid = NodeId.mk () in
        let env = VDMap.add vd nid env in
        let treat_param param =
          match param with
          | PUnprocessed ty -> PUnprocessed ty
          | PLeaf ty -> PLeaf (node ctx ty)
          | PRec ty -> PRec (resolve_custom_tagcomp f ctx env ty)
        in
        let treat_params { pid ; pdef } =
          { pid ; pdef=List.map treat_param pdef }
        in
        let union = List.map treat_params extracted in
        CDef (nid, union)
    end

let resolve_tagcomp ctx a =
  let ty = Tags.mk_comp a |> D.mk_tags |> Ty.mk_descr in
  match resolve_alias ctx ty with
  | Some d -> d
  | None ->
    let (t, ty') = TagComp.as_atom a in
    let handlers = tag_handlers ctx t in
    let rec aux handlers =
      match handlers with
      | [] -> tag t (node ctx ty')
      | f::handlers ->
        try { op = Custom (t, resolve_custom_tagcomp f ctx VDMap.empty ty') ; ty }
        with Exit -> aux handlers
    in
    aux handlers

let resolve_tags ctx a =
  let (pos, components) = Tags.destruct a in
  let d = components |> List.map (resolve_tagcomp ctx) |> union in
  if pos then d else neg d

let resolve_comp ctx c =
  let ty = D.of_component c |> Ty.mk_descr in
  let alias = resolve_alias ctx ty in
  let alias_or f c =
    match alias with
    | None -> f ctx c
    | Some d -> d
  in
  match c with
  | D.Enums c ->
    alias_or resolve_enums c,
    { op = Builtin AnyEnum ; ty = Enums.any |> D.mk_enums |> Ty.mk_descr }
  | D.Arrows c ->
    alias_or resolve_arrows c,
    { op = Builtin AnyArrow ; ty = Arrows.any |> D.mk_arrows |> Ty.mk_descr }
  | D.Intervals c ->
    alias_or resolve_intervals c,
    { op = Builtin AnyInt ; ty = Intervals.any |> D.mk_intervals |> Ty.mk_descr }
  | D.Tags c ->
    alias_or resolve_tags c,
    { op = Builtin AnyTag ; ty = Tags.any |> D.mk_tags |> Ty.mk_descr }
  | D.Tuples c ->
    alias_or resolve_tuples c,
    { op = Builtin AnyTuple; ty = Tuples.any |> D.mk_tuples |> Ty.mk_descr }
  | D.Records c ->
    alias_or resolve_records c,
    { op = Builtin AnyRecord ; ty = Records.any |> D.mk_records |> Ty.mk_descr }

let resolve_descr ctx d =
  let ty = VD.mk_descr d |> Ty.of_def in
  match resolve_alias ctx ty with
  | None ->
    let ds = D.components d |> List.map (resolve_comp ctx) in
    let combine ds =
      ds |> List.map (fun (d,any_d) -> cap' any_d d) |> union
    in
    let pd = ds |> combine in
    let nd = ds |> List.map (fun (d, any_d) -> (neg d, any_d)) |> combine |> neg in
    if size_of_descr nd < size_of_descr pd then nd else pd
  | Some d -> d

let resolve_def ctx def =
  let ty = Ty.of_def def in
  match resolve_alias ctx ty with
  | None ->
    let dnf = def |> VD.dnf |> VD.Dnf.simplify in
    let resolve_dnf (ps, ns, d) =
      let ps = ps |> List.map var in
      let ns = ns |> List.map var |> List.map neg in
      let d = resolve_descr ctx d in
      ps@ns@[d] |> inter
    in
    dnf |> List.map resolve_dnf |> union
  | Some d -> d

let rec resolve_missing_defs ctx t =
  let used_defs = ctx.nodes |> VDMap.bindings in
  let to_define = used_defs |> List.find_opt (fun (_,nid) ->
      t.defs |> List.exists (fun (nid',_) -> NodeId.equal nid nid') |> not
    ) in
  match to_define with
  | None -> t
  | Some (def,nid) ->
    let descr = resolve_def ctx def in
    resolve_missing_defs ctx { t with defs = (nid,descr)::t.defs }

(* Step 3 : Inline nodes when relevant, remove unused nodes *)

let used_nodes t =
  let res = nodes_in_descr t.main in
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

let remove_unused_nodes t =
  let used = used_nodes t in
  let defs = t.defs |> List.filter (fun (n,_) -> NISet.mem n used) in
  { t with defs }

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
    match try_inline t.defs with
    | None -> t
    | Some t' -> aux t'
  in
  aux t

(* Step 4 : Syntactic simplifications *)

let simplify t =
  let f d =
    match d.op with
    | Varop (Cap, [ d ; { op = Unop (Neg, dn) ; _ } ]) -> Binop (Diff, d, dn)
    | op -> op
  in
  map_t f t

(* Step 5 : Transform Custom *)

let transform_custom_tags ctx t =
  let aux map (tag, ts) =
    let m = TagMap.find tag ctx.extensions in
    let module M = (val m : PrinterExt) in
    let rec map_ts ts : custom =
      match ts with
      | CDef (nid,params) ->
        CDef (nid, List.map map_params params)
      | CNode nid -> CNode nid
    and map_params { pid; pdef } =
      { pid ; pdef=List.map map_param pdef }
    and map_param p =
      match p with
      | PUnprocessed ty -> PUnprocessed ty
      | PLeaf d -> PLeaf (map d)
      | PRec ts -> PRec (map_ts ts)
    in
    let module M' = struct
      type t = M.t
      let v = M.get (map_ts ts)
      let print = M.print
    end in
    (module M' : CustomNode)
  in
  let op d = d.op in
  map_t' aux op t

(* Step 6 : Rename nodes *)

let rename_nodes t =
  let c = ref 0 in
  let next_name () =
    c := !c + 1 ;
    "x"^(string_of_int !c)
  in
  t.defs |> List.iter (fun (n,_) ->
      NodeId.rename n (next_name ())
    )

(* Step 7 : Print *)

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
    | AnyTagComp t -> (Tag.name t)^"(any)"
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

let rec print_descr prec assoc fmt d =
  let rec aux prec assoc fmt d =
    match d.op with
    | Custom m ->
      let module M = (val m : CustomNode) in
      M.print prec assoc fmt M.v
    | Alias str -> Format.fprintf fmt "%s" str
    | Node n -> Format.fprintf fmt "%a" NodeId.pp n
    | Builtin b -> print_builtin fmt b
    | Var v -> Format.fprintf fmt "%a" Var.pp v
    | Enum a -> Format.fprintf fmt "%a" Enums.Atom.pp a
    | Tag (t,d) ->
      Format.fprintf fmt "%a(%a)"
        Tag.pp t print_descr' d
    | Interval (lb,ub) -> Format.fprintf fmt "%a" print_interval (lb,ub)
    | Record (bindings,opened) ->
      let print_binding fmt (l,d,b) =
        Format.fprintf fmt "%a %s %a"
          Label.pp l
          (if b then ":?" else ":")
          print_descr' d
      in
      Format.fprintf fmt "{ %a %s}"
        (print_seq print_binding " ; ")
        bindings
        (if opened then ".." else "")
    | Varop (v,ds) ->
      let sym,prec',_ as opinfo = varop_info v in
      Prec.fprintf prec assoc opinfo fmt "%a"
        (print_seq (aux prec' NoAssoc) sym)
        ds
    | Binop (b,d1,d2) ->
      let sym,prec',_ as opinfo = binop_info b in
      Prec.fprintf prec assoc opinfo fmt "%a%s%a"
        (aux prec' Left) d1 sym
        (aux prec' Right) d2
    | Unop (u,d) ->
      let sym,prec',_ as opinfo = unop_info u in
      Prec.fprintf prec assoc opinfo fmt "%s%a" sym (aux prec' NoAssoc) d
  in
  aux prec assoc fmt d

and print_descr' fmt d = print_descr min_prec NoAssoc fmt d

and print_def fmt (n,d) =
  Format.fprintf fmt "%a = %a" NodeId.pp n print_descr' d

and print_t fmt t =
  Format.fprintf fmt "%a" print_descr' t.main ;
  match t.defs with
  | [] -> ()
  | defs ->
    Format.fprintf fmt " where %a" (print_seq print_def " and ") defs

(* MAIN *)

let get customs ty =
  let (ctx, t) = build_t customs ty in
  let t = resolve_missing_defs ctx t in
  let t = inline t in
  let t = simplify t in
  let t = transform_custom_tags ctx t in
  rename_nodes t ;
  t

let print = print_t
let print_descr_atomic = print_descr max_prec NoAssoc
let print_descr, print_descr_ctx = print_descr', print_descr

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

let print_ty' = print_ty empty_params
let print_subst' = print_subst empty_params

let cap_descr = cap'
let cup_descr = cup'
let neg_descr = neg
