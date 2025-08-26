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
        neg = None;
      }
    let hash t = Hash.int t.id
    let compare t1 t2 = Int.compare t1.id t2.id
    let equal t1 t2 = t1.id = t2.id
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
    (* We need to duplicate these here, has the one in PreNode are uninitialized  *)
    let hash = AnyEmpty.hash
    let compare = AnyEmpty.compare
    let equal = AnyEmpty.equal

    let any = AnyEmpty.any
    let empty = AnyEmpty.empty  
  end
  and NSet : Set.S with type elt = AnyEmpty.t = Set.Make(PreNode) (* Sets of Node.t, but use PreNode to have a well defined cycle *)
  and VDescr : VDescr' with type node = Node.t = Vdescr.Make(Node) (* Instanciate VDescr *)
  and PreNode : PreNode with type t = AnyEmpty.t and type vdescr = VDescr.t and type descr = VDescr.Descr.t = struct
    (* The PreNode module that contain the entry points of all functions on types. *)
    module NH = Hashtbl.Make(PreNode)
    module Table = Bttable.Make(VDescr)(Bool)
    type _ Effect.t += GetCache: (Table.t) t

    type vdescr = VDescr.t
    type descr = VDescr.Descr.t

    type t = TyRef.t
    open TyRef
    open AnyEmpty

    let has_def t = Option.is_some t.def
    let def t = t.def |> Option.get

    let hash = AnyEmpty.hash
    let compare = AnyEmpty.compare
    let equal = AnyEmpty.equal

    let define ?(simplified=false) t d =
      t.def <- Some d ;
      t.dependencies <- None ;
      t.simplified <- simplified
    let cons ?(simplified=false) d =
      let t = mk () in
      define ~simplified t d ; t

    let of_def d = d |> cons

    let dcap t1 t2 = VDescr.cap (def t1) (def t2) |> cons
    let cap = fcap ~empty ~any ~cap:dcap

    let dcup t1 t2 = VDescr.cup (def t1) (def t2) |> cons
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
    let diff = fdiff_neg ~empty ~any ~neg ~diff:fdiff

    let conj ts = List.fold_left cap any ts
    let disj ts = List.fold_left cup empty ts

    let get_cache () = perform GetCache
    let with_own_cache f t =
      let cache = Table.create () in
      match f t with
        x -> x
      | effect GetCache, k -> continue k cache

    let is_empty t =
      let def = def t in
      if t.simplified then
        VDescr.equal def VDescr.empty
      else
        let cache = get_cache () in
        begin match Table.find ~default:true cache def with
          | Some b -> b
          | None ->
            let b = VDescr.is_empty def in
            Table.update cache def b;
            b
        end

    let leq t1 t2 = diff t1 t2 |> is_empty
    let equiv t1 t2 = leq t1 t2 && leq t2 t1
    let is_any t = neg t |> is_empty
    let disjoint t1 t2 = cap t1 t2 |> is_empty

    let rec simplify t =
      if not t.simplified then begin
        let s_def = def t |> VDescr.simplify in
        define ~simplified:true t s_def;
        s_def |> VDescr.direct_nodes |> List.iter simplify;
        match t.neg with
          None -> ()
        | Some nt -> define ~simplified:true nt (VDescr.neg s_def);
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

    let mk_var v = VDescr.mk_var v |> cons
    let mk_descr d = VDescr.mk_descr d |> cons
    let get_descr t = def t |> VDescr.get_descr
    let nodes t = dependencies t |> NSet.to_list
  end

  let () = AnyEmpty.init VDescr.empty VDescr.any (* Delayed initialization. *)

end : sig (* Hide everything, we could also add that in a .mli file. *)
           module rec Node : (Node with type vdescr = VDescr.t and type descr = VDescr.Descr.t)
           and VDescr : VDescr with type node = Node.t
         end)