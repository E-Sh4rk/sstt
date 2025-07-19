open Effect.Deep
open Effect

open Sstt_utils
open Base
open Sigs

(* The Node module is where we close the knot:
   - a Node.t contains a VDescr.t
   - a VDescr.t contains a Descr.t
   - a Descr.t contains several components, e.g. Arrows, Records, …
   - the Atoms of Arrows, Record contain Node.t

   Since these modules form a cycle, one of them must be safe.
   The problem is that they all contains top-level values that are not
   closures, namely `any` and `empty`. We therefore split the definition
   so that any and empty exist in a module outside of the cycle.
   An delayed initialization function is called to populate their content.
   Lastly, the whole mess is hidden via a top-level include with
   a contstrained signature (at the end of the file) to only expose the
   Node and VDescr modules, with abstract types.

*)


include (struct
  module rec TyRef : sig 
    (* The type of type reference, this module only contains the type definition to avoid
       repeating it everywhere. *)

    type t = {
      id : int ;
      mutable def : VDescr.t option ;
      mutable simplified : bool ;
      mutable dependencies : NSet.t option;
      mutable neg : t option
    }
  end = TyRef (* Trick: a recursive module with only types can be its own definition *)

  and AnyEmpty : sig
    (* The definition of any and empty, TyRef.t creation and the delayed init function *)
    type t = TyRef.t
    val mk : unit -> t
    val any : t
    val empty : t
    val init : VDescr.t -> VDescr.t -> unit
  end = struct
    type t = TyRef.t
    let next_id =
      let c = ref ~-1 in
      fun () -> incr c; !c
    open TyRef
    let mk () =
      {
        id = next_id () ;
        def = None ;
        simplified = false ;
        dependencies = None;
        neg = None;
      }

    let empty = mk ()
    let any = mk ()
    let init empty_def any_def =
      assert (empty.def = None && any.def = None);
      empty.def <- Some empty_def;
      empty.neg <- Some any;
      empty.simplified <- true;
      empty.dependencies <- Some (NSet.singleton empty);

      any.def <- Some any_def;
      any.neg <- Some empty;
      any.simplified <- true;
      any.dependencies <- Some (NSet.singleton any)
  end
  and Node : Node with type t = AnyEmpty.t and type vdescr = VDescr.t and type descr = VDescr.Descr.t = struct
    (* The module which contains any and empty that is passed to Vdescr.Make *)
    include PreNode
    let any = AnyEmpty.any
    let empty = AnyEmpty.empty  
  end
  and NSet : Set.S with type elt = AnyEmpty.t = Set.Make(PreNode) (* Sets of Node.t, but use PreNode to have a well defined cycle *)
  and VDescr : VDescr' with type node = Node.t = Vdescr.Make(Node) (* Instanciate VDescr *)
  and PreNode : PreNode with type t = AnyEmpty.t and type vdescr = VDescr.t and type descr = VDescr.Descr.t = struct
    (* The PreNode module that contain the entry points of all functions on types. *)
    module NMap = Map.Make(PreNode)
    module VDMap = Map.Make(VDescr)
    type _ Effect.t += GetCache: (bool VDMap.t) t
    type _ Effect.t += SetCache: bool VDMap.t -> unit t

    type vdescr = VDescr.t
    type descr = VDescr.Descr.t

    type t = TyRef.t
    open TyRef
    open AnyEmpty


    let () = init VDescr.empty VDescr.any (* Delayed initialization *)

    let has_def t = Option.is_some t.def
    let def t = t.def |> Option.get

    let hash t = Hashtbl.hash t.id
    let compare t1 t2 = Int.compare t1.id t2.id
    let equal t1 t2 = (t1.id = t2.id)


    let define ?(simplified=false) t d =
      t.def <- Some d ;
      t.dependencies <- None ;
      t.simplified <- simplified
    let cons ?(simplified=false) d =
      let t = mk () in
      define ~simplified t d ; t

    let of_def d = d |> cons

    let cap t1 t2 =
      if t1 == empty || t2 == empty then empty
      else if t1 == any then t2
      else if t2 == any then t1
      else
        VDescr.cap (def t1) (def t2) |> cons

    let cup t1 t2 =
      if t1 == any || t2 == any then any
      else if t1 == empty then t2
      else if t2 == empty then t1
      else
        VDescr.cup (def t1) (def t2) |> cons
    let neg t =
      match t.neg with
      | Some s -> s
      | None ->
        let s = t |> def |> VDescr.neg
                |> cons ~simplified:t.simplified in
        t.neg <- Some s;
        s.neg <- Some t;
        s

    let diff t1 t2 =
      if t1 == empty || t2 == any then empty
      else if t1 == any then neg t2
      else if t2 == empty then t1
      else
        VDescr.diff (def t1) (def t2) |> cons

    let conj ts = List.fold_left cap any ts
    let disj ts = List.fold_left cup empty ts

    let is_empty t =
      let def = def t in
      if t.simplified then
        VDescr.equal def VDescr.empty
      else
        let cache = perform GetCache in
        begin match VDMap.find_opt def cache with
          | Some b -> b
          | None ->
            let cache' = ref (VDMap.add def true cache) in
            let b =
              match VDescr.is_empty def with
              | b -> b
              | effect GetCache , k -> continue k !cache'
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
      | effect GetCache, k -> continue k !cache
      | effect SetCache c, k -> cache := c ; continue k ()

    let rec simplify t =
      if not t.simplified then begin
        let s_def = def t |> VDescr.simplify in
        define ~simplified:true t s_def ;
        s_def |> VDescr.direct_nodes |> List.iter simplify ;
        match t.neg with
        | None -> ()
        | Some nt -> define ~simplified:true nt (VDescr.neg s_def)
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


end : sig
           module rec Node : (Node with type vdescr = VDescr.t and type descr = VDescr.Descr.t)
           and VDescr : VDescr with type node = Node.t
         end)