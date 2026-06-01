open Effect.Deep
open Effect

open Sstt_utils
open Base
open Sigs

(* There is intrinsicly a cycle in the definitions of set-theoretic types, since
   they are co-inductive:
   - a type is a reference pointing to a variable-descriptor (VDescr)
   - a VDescr is a BDD where atoms are variables and leaves are descriptors
     (Descr)
   - a Descr is a disjoint union of components
   - a component is either basic (like Intervals) or a constructor (like Arrows
     or Tuples)
   - constructors contain type references.

   This can be naturally encoded as mutually recursive modules. The caveat is
   that all these modules must contain any/empty in their signatures, which are
   just constants. Therefore none of the recursive modules is "safe" (according
   to the OCaml manual), since none of them contain only functional values.

   We work around this issue here, all other module are naturally expressed as
   functors taking a Node as argument.


   1. PreNode is our safe module. It is initialized with stubs.
   2. AnyEmpty is initialized properly, the references to any/empty are created,
     but not initialized
   3. Node is initialized, it includes PreNode (stubs) and AnyEmpty
   4. VDescr = Vdescr.Make(Node) is initialized as well as its content. In
     particular, VDescr.Descr.Records.Atom.any and similar reference the
     properly initialized top-level value AnyEmpty.any
   5. Node is patched and its top-level expressions are evaluated

   After all this, we finally initialize AnyEmpty.{any/empty} by calling the init
   function (see at the bottom of the file). Client-code which lives in the cycle
   (so VDescr, Descr or a component) must never dereference Node.any or
   Node.empty in a toplevel definition, otherwise they will get an exception
   since the type references are still not initialized.

   Lastly, to prevent external code to access the internal definition of TyRef.t
   directly (as well as accessing internal functions), we use the trick to
   include all the modules and constrain the signature, exposing only Node and
   VDescr.
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
      mutable all_vars : MixVarSet.t option;
      mutable neg : t option
    }
  end = TyRef (* Trick: a recursive module with only types can be its own definition *)

  and AnyEmpty : sig
    (* The definition of any and empty, TyRef.t creation and the delayed init function *)
    type t = TyRef.t
    val mk : unit -> t
    val hash : t -> int
    val compare : t -> t -> int
    val equal : t -> t -> bool
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
        all_vars = None;
        neg = None;
      }
    let hash t = t.id
    let compare t1 t2 = Int.compare t1.id t2.id
    let equal t1 t2 = t1 == t2
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
  and Node : Node with type t = AnyEmpty.t and type vdescr = VDescr.t and type descr = VDescr.Descr.t
                                                                      and type row = VDescr.Descr.Records.Atom.t = struct
    (* The module which contains any and empty that is passed to Vdescr.Make *)
    include PreNode
    (* We need to duplicate these here, has the one in PreNode are uninitialized  *)
    let hash = AnyEmpty.hash
    let compare = AnyEmpty.compare
    let equal = AnyEmpty.equal

    let any = AnyEmpty.any
    let empty = AnyEmpty.empty
  end
  and NSet : Set.S with type elt = AnyEmpty.t = Set.Make(PreNode) (* Sets of Node.t, but use PreNode to have a well defined cycle *)
  and VDescr : VDescr' with type node = Node.t = Vdescr.Make(Node) (* Instanciate VDescr *)
  and PreNode : PreNode with type t = AnyEmpty.t and type vdescr = VDescr.t and type descr = VDescr.Descr.t
                                                                            and type row = VDescr.Descr.Records.Atom.t = struct
    (* The PreNode module that contain the entry points of all functions on types. *)

    (* Subtyping cache *)
    module Table = Bttable.MakeOpt(PreNode)(Bool)


    (** The memoization cache. Two caches are kept, one for simplified the other
          for unsimplified nodes. Given a node [t], the following invariant
          should be maintained when memoization is active (i.e. in the context
          of a [with_shared_cache]).

        For every a node [n] that occurs in [t], if [def n] → [n'] is in the
        cache, then [n == n']. [n'] is calthenled the canonical node for the
        descriptor [def n].

        A descriptor has at most two canonical nodes, one in each version of the
        cache.

        This invariant must be enforced by all functions that call [define]
        below to update the content of a node.
    *)

    module ConsCache = Hashtbl.Make(VDescr)

    (** Cache for set-theoretic operations *)

    module BinOpCache = Hashtbl.Make(struct
        type t = PreNode.t * PreNode.t
        let equal (t1, t2) (s1, s2) = PreNode.equal t1 s1 && PreNode.equal t2 s2
        let hash (t1, t2) = Hash.mix (PreNode.hash t1) (PreNode.hash t2) end
      )

    type cache = { is_empty_cache : Table.t;
                   cons_cache : PreNode.t ConsCache.t * PreNode.t ConsCache.t;
                   inter_cache : PreNode.t BinOpCache.t;
                   union_cache : PreNode.t BinOpCache.t;
                   diff_cache : PreNode.t BinOpCache.t }

    type _ Effect.t += GetCache: cache t

    let create_cache () =
      let sim_cons_cache = ConsCache.create 4 in
      ConsCache.add sim_cons_cache VDescr.empty AnyEmpty.empty;
      ConsCache.add sim_cons_cache VDescr.any AnyEmpty.any;

      { is_empty_cache = Table.create ();
        cons_cache = sim_cons_cache, ConsCache.create 4;
        inter_cache = BinOpCache.create 4;
        union_cache = BinOpCache.create 4;
        diff_cache = BinOpCache.create 4 }

    let[@inline always] get_cache () = perform GetCache
    let[@inline always] get_is_empty_cache () = (get_cache ()).is_empty_cache
    let[@inline always] get_cons_cache b =
      let c1, c2 = (get_cache()).cons_cache in
      if b then c1 else c2
    let[@inline always] get_inter_cache () = (get_cache()).inter_cache
    let[@inline always] get_union_cache () = (get_cache()).union_cache
    let[@inline always] get_diff_cache () = (get_cache()).diff_cache


    (* with_shared_cache is made re-entrant by looking for the top-most cache
       on the call stack, if any.
    *)
    let with_shared_cache f t =
      let cache : cache option ref = ref None in
      try f t with
      | effect GetCache, k ->
        match !cache with
          Some c -> continue k c (* the topmost cache has already been found *)
        | None ->
          (* Otherwise, we perform the effect again, to reach the topmost call to with_shared_cache *)
          let c = try get_cache () with Unhandled GetCache ->
            (* no handler above us, create the cache *)
            create_cache ()
          in
          cache := Some c; (* store the returned cache *)
          continue k c

    module NH = Hashtbl.Make(PreNode)

    type vdescr = VDescr.t
    type descr = VDescr.Descr.t

    type t = TyRef.t
    open TyRef
    open AnyEmpty

    type row = VDescr.Descr.Records.Atom.t
    type subst = (t, row) MixVarMap.t


    let has_def t = Option.is_some t.def
    let def t = t.def |> Option.get

    let hash = AnyEmpty.hash
    let compare = AnyEmpty.compare
    let equal = AnyEmpty.equal

    let define ?(simplified=false) t d =
      (* Clear the cached fields *)
      let () = match t.neg with
          None -> ()
        | Some n -> n.neg <- None; t.neg <- None
      in
      t.dependencies <- None ;
      t.all_vars <- None;

      t.def <- Some d ;
      t.simplified <- simplified

    (* If a handler for the GetCache effect is in place,
       memoize the node. Otherwise just create a fresh node.
    *)
    let cons ?(simplified=false) d =
      if VDescr.(equal d empty) then empty
      else if VDescr.(equal d any) then any
      else
        try
          let cache = get_cons_cache simplified in
          match ConsCache.find_opt cache d with
            Some t -> t (* returns the canonical node *)
          | None ->
            let t = mk () in
            ConsCache.add cache d t; (* sets t as the canonical node *)
            define ~simplified t d ; t
        with
          Unhandled GetCache ->
          let t = mk () in define ~simplified t d; t

    let of_def d = d |> cons

    let binop op get t1 t2 =
      try
        let cache = get () in
        let key = t1, t2 in
        match BinOpCache.find_opt cache key with
          Some t -> t
        | None ->
          let t = op t1 t2 in
          BinOpCache.add cache key t;
          t
      with Unhandled GetCache -> op t1 t2


    let dcap t1 t2 = VDescr.cap (def t1) (def t2) |> cons
    let dcap = binop dcap get_inter_cache
    let cap = fcap ~empty ~any ~cap:dcap

    let dcup t1 t2 = VDescr.cup (def t1) (def t2) |> cons
    let dcup = binop dcup get_union_cache
    let cup = fcup ~empty ~any ~cup:dcup

    let neg t =
      match t.neg with
      | Some s -> s
      | None ->
        let s = t |> def |> VDescr.neg
          |> cons ~simplified:t.simplified in
        t.neg <- Some s;
        s.neg <- Some t;
        s
    let neg = fneg ~empty ~any ~neg

    let fdiff t1 t2 = VDescr.diff (def t1) (def t2) |> cons
    let fdiff = binop fdiff get_diff_cache
    let diff = fdiff_neg ~empty ~any ~neg ~diff:fdiff

    let conj ts = List.fold_left cap any ts
    let disj ts = List.fold_left cup empty ts

    (** Entry point of the subtyping algorithm. The fast path is done outside of
        the scope of with_shared_cache so that a toplevel call to `is_empty`
        returns quickly if the result is trivial.

        Any function in this module which calls directly or indirectly the
        subtyping algorithm can be guarded by [with_shared_cache] to share the
        subtyping cache among all its calls.
    *)


    let is_empty_rec t =
      let cache = get_is_empty_cache () in
      begin match Table.find ~default:true cache t with
        | Some b -> b
        | None ->
          let b = VDescr.is_empty (def t) in
          Table.update cache t b;
          b
      end

    let is_empty_rec = with_shared_cache is_empty_rec

    let is_empty t =
      if t == empty then true
      else if t == any then false
      else if t.simplified then VDescr.equal (def t) VDescr.empty
      else is_empty_rec t

    (* Derived subtyping function *)
    let leq t1 t2 = diff t1 t2 |> is_empty

    let equiv t1 t2 = leq t1 t2 && leq t2 t1
    let equiv = with_shared_cache equiv

    let is_any t = neg t |> is_empty
    let disjoint t1 t2 = cap t1 t2 |> is_empty


    (* Node simplification we recursively traverse the node and simplify it. For
       each vdescr encountered we ensure that its surrounding node is the
       canonical one, if it exists in the cache, otherwise we can leave it. *)

    let rec simplify_rec t =
      if t.simplified then t
      else
        let t_def = def t in
        let cache = get_cons_cache true in
        match ConsCache.find_opt cache t_def with
          Some t' -> t' (* a simplified t exists in the cache, return it *)
        | None ->
          t.simplified <- true; (* to stop the recursion when we encounter t again *)
          let s_def = t_def |> VDescr.simplify |> VDescr.map_nodes simplify_rec in
          define ~simplified:true t s_def;
          match ConsCache.find_opt cache s_def with
            Some t' -> t' (* the simplified descriptor alread has a canonical node *)
          | None -> t (* No canonical node t was updated in place *)


    let simplify_rec = with_shared_cache simplify_rec
    let simplify t = if t.simplified then t else simplify_rec t

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

    let vars_toplevel t = def t |> VDescr.direct_vars
    let row_vars_toplevel t = def t |> VDescr.direct_row_vars
    let all_vars_toplevel t = MixVarSet.of_set (vars_toplevel t) (row_vars_toplevel t)
    let vars t =
      NSet.fold (fun n -> VarSet.union (vars_toplevel n)) (dependencies t) VarSet.empty
    let row_vars t =
      NSet.fold (fun n -> RowVarSet.union (row_vars_toplevel n)) (dependencies t) RowVarSet.empty
    let all_vars t =
      match t.all_vars with
        Some s -> s
      | None ->
        let s = MixVarSet.of_set (vars t) (row_vars t) in
        t.all_vars <- Some s; s


    (** Systems of contractive equations. [simplify] above must be called on the result (done in Core) *)
    let of_eqs eqs =
      let deps = eqs
        |> List.fold_left (fun acc (_, t) -> NSet.union (dependencies t) acc) NSet.empty in
      let copies = NH.create 10 in
      let () = NSet.iter (fun n -> NH.add copies n (mk ())) deps in
      let new_node n =
        match eqs |> List.find_opt (fun (v,_) ->
            VDescr.equal (VDescr.mk_var v) (def n)) with
        | None -> NH.find copies n
        | Some (_,n) -> NH.find copies n (* Optimisation to avoid introducing a useless node *)
      in
      let rec define_all deps =
        if NSet.is_empty deps |> not then
          let deps_ok n =
            let vs = vars_toplevel n in
            if eqs |> List.for_all (fun (v,n) ->
                VarSet.mem v vs |> not || new_node n |> has_def
              ) then Some n else None
          in
          match deps |> NSet.to_seq |> Seq.find_map deps_ok with
          | None -> invalid_arg "Set of equations is not contractive."
          | Some n ->
            let nn = new_node n in
            if has_def nn |> not then begin
              let s = eqs |> List.filter_map (fun (v,n) ->
                  let nn = new_node n in
                  if has_def nn then Some (v, def nn) else None
                ) in
              let d = def n |> VDescr.map_nodes new_node
                |> VDescr.substitute (MixVarMap.of_list1 s) in
              define nn d
            end ;
            define_all (NSet.remove n deps)
      in
      define_all deps ;
      eqs |> List.map (fun (v,n) -> v,new_node n)

    (** Variable substitution. [simplify] above must be called on the result (done in Core) *)
    let substitute s t =
      if MixVarMap.is_empty s then t else
        let dom = MixVarMap.fold
            (fun n _ -> MixVarSet.add1 n)
            (fun r _ -> MixVarSet.add2 r)
            s MixVarSet.empty in
        let s = s |> MixVarMap.map1 (fun n -> def n) in
        (* Optimisation: reuse nodes if possible *)
        let unchanged n = MixVarSet.disjoint (all_vars n) dom in
        let deps = dependencies t |> NSet.filter (fun n -> unchanged n |> not) in
        let copies = NH.create 10 in
        let () = NSet.iter (fun n -> NH.add copies n (mk ())) deps in
        let new_node n =
          match NH.find_opt copies n with
          | Some n -> n
          | None -> n
        in
        deps |> NSet.iter (fun n ->
            let d = def n |> VDescr.map_nodes new_node |> VDescr.substitute s in
            define (new_node n) d
          ) ;
        new_node t

    (** Common node factorization. [simplify] above must be called on the result (done in Core) *)
    let factorize t =
      let cache = NH.create 10 in
      let nodes = ref [] in
      let rec aux t =
        match NH.find_opt cache t with
        | Some n -> n
        | None ->
          begin match
              List.find_opt (fun (t', _) -> equiv t t') !nodes
            with
            | Some (_, n) -> n
            | None ->
              let n = mk () in
              NH.add cache t n;
              nodes := (t, n) :: !nodes;
              let vd = def t |> VDescr.map_nodes aux in
              define n vd ;
              n
          end
      in
      aux t

    let factorize = with_shared_cache factorize

    let mk_var v = VDescr.mk_var v |> cons
    let mk_descr d = VDescr.mk_descr d |> cons
    let get_descr t = def t |> VDescr.get_descr
    let nodes t = dependencies t |> NSet.to_list
  end

  let () = AnyEmpty.init VDescr.empty VDescr.any (* Delayed initialization. *)

end : sig (* Hide everything, we could also add that in a .mli file. *)
           module rec Node : (Node with type vdescr = VDescr.t and type descr = VDescr.Descr.t
                                                               and type row=VDescr.Descr.Records.Atom.t)
           and VDescr : VDescr with type node = Node.t
         end)