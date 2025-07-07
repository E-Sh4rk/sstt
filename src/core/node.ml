open Sstt_utils
open Base
open Sigs
open Effect.Deep
open Effect

module rec Node : Node with type vdescr = VDescr.t and type descr = VDescr.Descr.t = struct

  module NSet = Set.Make(Node)
  module NMap = Map.Make(Node)
  module VDMap = Map.Make(VDescr)
  type _ Effect.t += GetCache: unit -> (bool VDMap.t) t
  type _ Effect.t += SetCache: bool VDMap.t -> unit t

  type vdescr = VDescr.t
  type descr = VDescr.Descr.t
  type t = {
    id : int ;
    mutable def : VDescr.t option ;
    mutable simplified : bool ;
    mutable dependencies : NSet.t option;
    mutable neg : t option

  }

  let has_def t = Option.is_some t.def
  let def t = t.def |> Option.get

  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = Int.compare t1.id t2.id
  let equal t1 t2 = (t1.id = t2.id)

  let next_id =
    let c = ref 0 in
    fun () -> c := !c + 1 ; !c

  let mk () =
    {
      id = next_id () ;
      def = None ;
      simplified = false ;
      dependencies = None;
      neg = None;

    }

  let define ?(simplified=false) t d =
    t.def <- Some d ;
    t.simplified <- simplified
  let cons d =
    let t = mk () in
    define t d ; t

  let of_def d = d |> cons

  let any, empty =
    let empty = VDescr.empty |> cons in
    let any = VDescr.any |> cons in
    empty.neg <- Some any;
    any.neg <- Some empty;
    (fun () -> any), (fun () -> empty)

  let is_any_ =
    let any = any () in
    fun t -> t == any
  let is_empty_ =
    let empty = empty () in
    fun t -> t == empty
  let cap t1 t2 =
    if is_empty_ t1 || is_empty_ t2 then empty ()
    else if is_any_ t1 then t2
    else if is_any_ t2 then t1
    else
      VDescr.cap (def t1) (def t2) |> cons
  let cup t1 t2 =
    if is_any_ t1 || is_any_ t2 then any ()
    else if is_empty_ t1 then t2
    else if is_empty_ t2 then t1
    else
      VDescr.cup (def t1) (def t2) |> cons
  let neg t =
    match t.neg with
      Some s -> s
    | None ->
      let s = t |> def |> VDescr.neg |> cons in
      t.neg <- Some s;
      s.neg <- Some t;
      s

  let diff t1 t2 =
    if is_empty_ t1 || is_any_ t2 then empty ()
    else if is_any_ t1 then neg t2
    else if is_empty_ t2 then t1
    else
      VDescr.diff (def t1) (def t2) |> cons
  let conj ts = List.fold_left cap (any ()) ts
  let disj ts = List.fold_left cup (empty ()) ts

  let is_empty t =
    let def = def t in
    if t.simplified then
      VDescr.equal def VDescr.empty
    else
      let cache = perform (GetCache ()) in
      begin match VDMap.find_opt def cache with
        | Some b -> b
        | None ->
          let cache' = ref (VDMap.add def true cache) in
          let b =
            match VDescr.is_empty def with
            | b -> b
            | effect GetCache (), k -> continue k !cache'
            | effect SetCache c, k -> cache' := c ; continue k ()
          in
          let cache = if b then !cache' else VDMap.add def false cache in
          perform (SetCache cache); b
      end
  let leq t1 t2 = diff t1 t2 |> is_empty
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let is_any t = neg t |> is_empty
  let disjoint t1 t2 = cap t1 t2 |> is_empty
  let with_own_cache f t =
    let cache = ref VDMap.empty in
    match f t with
    | x -> x
    | effect GetCache (), k -> continue k !cache
    | effect SetCache c, k -> cache := c ; continue k ()

  let rec simplify t =
    if not t.simplified then begin
      let s_def = def t |> VDescr.simplify in
      define ~simplified:true t s_def ;
      t.dependencies <- None;
      s_def |> VDescr.direct_nodes |> List.iter simplify;
      let nt = match t.neg with
          None -> mk ()
        | Some nt -> nt
      in
      define ~simplified:true nt (VDescr.neg s_def);
      nt.dependencies <- None;
      t.neg <- Some nt
    end

  let dependencies t =
    let direct_nodes t = def t |> VDescr.direct_nodes |> NSet.of_list in
    let rec aux ts =
      let ts' = ts
                |> NSet.to_list
                |> List.map direct_nodes
                |> List.fold_left NSet.union ts
      in
      if NSet.equal ts ts' then ts' else aux ts'
    in
    aux (NSet.singleton t)

  let dependencies t =
    match t.dependencies with
    | Some d -> d
    | None -> let d = dependencies t in t.dependencies <- Some d; d

  let vars_toplevel t = def t |> VDescr.direct_vars |> VarSet.of_list
  let vars t =
    NSet.fold (fun n -> VarSet.union (vars_toplevel n)) (dependencies t) VarSet.empty

  let of_eqs eqs =
    let deps = eqs
               |> List.fold_left (fun acc (_, t) -> NSet.union (dependencies t) acc) NSet.empty in
    let copies = NSet.fold (fun n acc -> NMap.add n (mk ()) acc) deps NMap.empty in
    let new_node n =
      match eqs |> List.find_opt (fun (v,_) ->
          VDescr.equal (VDescr.mk_var v) (def n)) with
      | None -> NMap.find n copies
      | Some (_,n) -> NMap.find n copies (* Optimisation to avoid introducing a useless node *)
    in
    let rec define_all deps =
      if NSet.is_empty deps |> not then
        let deps_ok n =
          let vs = vars_toplevel n in
          eqs |> List.for_all (fun (v,n) ->
              VarSet.mem v vs |> not || new_node n |> has_def
            )
        in
        match deps |> find_opt_of_iter deps_ok NSet.iter with
        | None -> raise (Invalid_argument "Set of equations is not contractive.")
        | Some n ->
          let nn = new_node n in
          if has_def nn |> not then begin
            let s = eqs |> List.filter_map (fun (v,n) ->
                let nn = new_node n in
                if has_def nn then Some (v, def nn) else None
              ) |> VarMap.of_list in
            let d = def n |> VDescr.map_nodes new_node |> VDescr.substitute s in
            define nn d
          end ;
          define_all (NSet.remove n deps)
    in
    define_all deps ;
    eqs |> List.map (fun (v,n) -> v,new_node n)

  let substitute s t =
    let dom = VarMap.fold (fun n _ -> VarSet.add n) s VarSet.empty in
    let s = s |> VarMap.map (fun n -> def n) in
    (* Optimisation: reuse nodes if possible *)
    let unchanged n = VarSet.disjoint (vars n) dom in
    let deps = dependencies t
               |> NSet.filter  (fun n -> unchanged n |> not) in
    let copies = NSet.fold (fun n acc -> NMap.add n (mk ()) acc) deps NMap.empty in
    let new_node n =
      match NMap.find_opt n copies with
      | Some n -> n
      | None -> n
    in
    deps |> NSet.iter (fun n ->
        let d = def n |> VDescr.map_nodes new_node |> VDescr.substitute s in
        define (new_node n) d
      ) ;
    new_node t

  let factorize t =
    let cache = ref NMap.empty in
    let rec aux t =
      match NMap.find_opt t !cache with
      | Some n -> n
      | None ->
        begin match
            !cache
            |> find_opt_of_iter2 (fun t' _ -> equiv t t') NMap.iter
          with
          | Some (_, n) -> n
          | None ->
            let n = mk () in
            cache := NMap.add t n (!cache) ;
            let vd = def t |> VDescr.map_nodes aux in
            define n vd ;
            n
        end
    in
    aux t

  let mk_var v = VDescr.mk_var v |> cons
  let mk_descr d = VDescr.mk_descr d |> cons
  let get_descr t = def t |> VDescr.get_descr
  let nodes t = dependencies t |> NSet.to_list
end
and VDescr : VDescr' with type node = Node.t = Vdescr.Make(Node)
