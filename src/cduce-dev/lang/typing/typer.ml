open Cduce_loc
open Ast
open Ident

let ( = ) (x : int) y = x = y
let ( <= ) (x : int) y = x <= y
let ( < ) (x : int) y = x < y
let ( >= ) (x : int) y = x >= y
let ( > ) (x : int) y = x > y

let _warning loc msg =
  let ppf = Format.err_formatter in
  Cduce_loc.print_loc ppf (loc, `Full);
  Format.fprintf ppf ": Warning: %s@." msg

type schema = {
  sch_uri : string;
  sch_ns : Ns.Uri.t;
  sch_comps : (Types.t * Schema_validator.t) Ident.Env.t;
}

type item =
  (* These are really exported by CDuce units: *)
  | Type of (Types.t * Var.t list)
  | Val of Types.t
  | ECDuce of Compunit.t
  | ESchema of schema
  | ENamespace of Ns.Uri.t
  (* These are only used internally: *)
  | EVal of Compunit.t * id * Types.t
  | EOCaml of string
  | EOCamlComponent of string
  | ESchemaComponent of (Types.t * Schema_validator.t)

type t = {
  ids : item Env.t;
  ids_loc : loc Env.t;
  ns : Ns.table;
  keep_ns : bool;
  poly_vars : (U.t * Var.t) list;
  mono_vars : Var.Set.t;
  mutable weak_vars : Types.t option Var.Map.map;
}

(* Namespaces *)

let set_ns_table_for_printer env = Ns.InternalPrinter.set_table env.ns
let get_ns_table tenv = tenv.ns
let type_keep_ns env k = { env with keep_ns = k }

let protect_error_ns loc f x =
  try f x with
  | Ns.UnknownPrefix ns ->
    Cduce_error.(raise_err_loc ~loc Generic ("Undefined namespace prefix " ^ U.to_string ns))

let qname env loc t = protect_error_ns loc (Ns.map_tag env.ns) t
let ident env loc t = protect_error_ns loc (Ns.map_attr env.ns) t
let parse_atom env loc t = AtomSet.V.mk (qname env loc t)
let parse_ns env loc ns = protect_error_ns loc (Ns.map_prefix env.ns) ns

let parse_label env loc t =
  Label.mk (protect_error_ns loc (Ns.map_attr env.ns) t)

let parse_record env loc f r =
  let r = List.map (fun (l, x) -> (parse_label env loc l, f x)) r in
  LabelMap.from_list
    (fun _ _ -> Cduce_error.raise_err_loc ~loc Generic "Duplicated record field")
    r

(*fun _ _ -> assert false*)
let from_comp_unit = ref (fun _ -> assert false)
let load_comp_unit = ref (fun _ -> assert false)
let has_ocaml_unit = ref (fun _ -> false)
let has_static_external = ref (fun _ -> assert false)

let type_schema env loc name uri =
  let x = ident env loc name in
  let ns, sch = Schema_converter.load_schema (U.to_string name) uri in
  let sch = { sch_uri = uri; sch_comps = sch; sch_ns = ns } in
  { env with ids = Env.add x (ESchema sch) env.ids }

let empty_env =
  {
    ids = Env.empty;
    ids_loc = Env.empty;
    ns = Ns.def_table;
    keep_ns = false;
    poly_vars = [];
    mono_vars = Var.Set.empty;
    weak_vars = Var.Map.empty;
  }

let enter_id x i env = { env with ids = Env.add x i env.ids }

let type_using env loc x cu =
  try
    let cu = !load_comp_unit cu in
    enter_id (ident env loc x) (ECDuce cu) env
  with
  | Not_found -> Cduce_error.raise_err_loc ~loc Typer_Error ("Cannot find external unit " ^ U.to_string cu)

let enter_type id t env = enter_id id (Type t) env

let enter_types l env =
  {
    env with
    ids =
      List.fold_left
        (fun accu (id, t, al) -> Env.add id (Type (t, al)) accu)
        env.ids l;
  }

let find_id env0 env loc head x =
  let id = ident env0 loc x in
  try Env.find id env.ids with
  | Not_found when head -> (
      try ECDuce (!load_comp_unit x) with
      | Not_found -> Cduce_error.(raise_err_loc ~loc Typer_Error ("Cannot resolve this identifier: " ^ U.get_str x)))
let find_id_comp env0 env loc x =
  if
    (match (U.get_str x).[0] with
     | 'A' .. 'Z' -> true
     | _ -> false)
    && !has_ocaml_unit x
  then EOCaml (U.get_str x)
  else find_id env0 env loc true x

let enter_value id t env = { env with ids = Env.add id (Val t) env.ids }

let enter_values l env =
  {
    env with
    ids = List.fold_left (fun accu (id, t) -> Env.add id (Val t) accu) env.ids l;
  }

let enter_values_dummy l env =
  {
    env with
    ids =
      List.fold_left
        (fun accu id -> Env.add id (Val Types.empty) accu)
        env.ids l;
  }

let value_name_ok id env =
  try
    match Env.find id env.ids with
    | Val _
    | EVal _ ->
      true
    | _ -> false
  with
  | Not_found -> true

let iter_values env f =
  Env.iter
    (fun x -> function
       | Val t -> f x t
       | _ -> ())
    env.ids

let register_types cu env =
  Env.iter
    (fun x t ->
       match t with
       | Type (t, vparams) ->
         let params = List.map Types.var vparams in
         Types.Print.register_global cu x ~params t
       | _ -> ())
    env.ids

let rec const env loc = function
  | LocatedExpr (loc, e) -> const env loc e
  | Pair (x, y) -> Types.Pair (const env loc x, const env loc y)
  | Xml (x, y) -> Types.Xml (const env loc x, const env loc y)
  | RecordLitt x -> Types.Record (parse_record env loc (const env loc) x)
  | String (i, j, s, c) -> Types.String (i, j, s, const env loc c)
  | Atom t -> Types.Atom (parse_atom env loc t)
  | Integer i -> Types.Integer i
  | Char c -> Types.Char c
  | Const c -> c
  | _ -> Cduce_error.raise_err_loc ~loc Typer_InvalidConstant ()

(* I. Transform the abstract syntax of types and patterns into
      the internal form *)

let find_schema_component sch name =
  try ESchemaComponent (Env.find name sch.sch_comps) with
  | Not_found ->
    Cduce_error.(raise_err Typer_Error
                   (Printf.sprintf "No component named '%s' found in schema '%s'"
                      (Ns.QName.to_string name) sch.sch_uri))

let navig loc env0 (env, comp) id =
  match comp with
  | ECDuce cu ->
    let env = !from_comp_unit cu in
    let c =
      try find_id env0 env loc false id with
      | Not_found -> Cduce_error.raise_err_loc ~loc Typer_UnboundId ((Ns.empty, id), false)
    in
    let c =
      match c with
      | Val t -> EVal (cu, ident env0 loc id, t)
      | c -> c
    in
    (env, c)
  | EOCaml cu -> (
      let s = cu ^ "." ^ U.get_str id in
      match (U.get_str id).[0] with
      | 'A' .. 'Z' -> (env, EOCaml s)
      | _ -> (env, EOCamlComponent s))
  | ESchema sch -> (env, find_schema_component sch (ident env0 loc id))
  | Type _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "Types don't have components") 
  | Val _
  | EVal _ ->
    Cduce_error.(raise_err_loc ~loc Typer_Error "Values don't have components")
  | ENamespace _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "Namespaces don't have components")
  | EOCamlComponent _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "Caml values don't have components")
  | ESchemaComponent _ -> Cduce_error.(raise_err_loc  ~loc Typer_Error "Schema components don't have components")

(*
    | _ -> error loc "Invalid dot access"
*)

let rec find_global env loc ids =
  match ids with
  | id :: rest ->
    let comp = find_id env env loc true id in
    snd (List.fold_left (navig loc env) (env, comp) rest)
  | _ -> assert false

let eval_ns env loc = function
  | `Uri ns -> ns
  | `Path ids -> (
      match find_global env loc ids with
      | ENamespace ns -> ns
      | ESchema sch -> sch.sch_ns
      | _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "This path does not refer to a namespace or schema"))

let type_ns env loc p ns =
  (* TODO: check that p has no prefix *)
  let ns = eval_ns env loc ns in
  {
    env with
    ns = Ns.add_prefix p ns env.ns;
    ids = Env.add (Ns.empty, p) (ENamespace ns) env.ids;
  }

let find_global_type env loc ids =
  match find_global env loc ids with
  | Type (t, pargs) -> (t, pargs)
  | ESchemaComponent (t, _) -> (t, []) (*TODO CHECK*)
  | _ -> Cduce_error.raise_err_loc ~loc Typer_Error "This path does not refer to a type"

let find_global_schema_component env loc ids =
  match find_global env loc ids with
  | ESchemaComponent c -> c
  | _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "This path does not refer to a schema component")

let find_local_type env loc id =
  match Env.find id env.ids with
  | Type t -> t
  | _ -> raise Not_found

let find_value id env =
  match Env.find id env.ids with
  | Val t
  | EVal (_, _, t) ->
    t
  | _ -> raise Not_found

let do_open env cu =
  let env_cu = !from_comp_unit cu in
  let ids =
    Env.fold
      (fun n d ids ->
         let d =
           match d with
           | Val t -> EVal (cu, n, t)
           | d -> d
         in
         Env.add n d ids)
      env_cu.ids env.ids
  in
  { env with ids; ns = Ns.merge_tables env.ns env_cu.ns }

let type_open env loc ids =
  match find_global env loc ids with
  | ECDuce cu -> do_open env cu
  | _ -> Cduce_error.(raise_err_loc ~loc Typer_Error "This path does not refer to a CDuce unit")

module IType = struct
  open Typepat

  (* From AST to the intermediate representation *)
  (* We need to be careful about the order of type definitions in case
      of polymorphic types.  Mutually recursive polymorphic types
      cannot be recursively called with different parameters within
      their recursive groups. We build a graph from the type
      definitions and use Tarjan's algorithm to find all strongly
      connected components in topological order.  Then we translate the
      AST into intermediate representation in that order. *)

  (* [scc defs] takes a list of definitions and returns [ldefs, map] where
     - [ldefs] is a list of list of definitions in topological order. Each
          internal list is a group of mutually recursive definitions.
     - [map] is mapping from type names to their rank and list of parameters.
  *)
  let scc defs =
    let module Info = struct
      type t = {
        mutable index : int;
        mutable lowlink : int;
        mutable is_removed : bool;
        def : loc * U.t * U.t list * ppat;
      }

      let empty =
        {
          index = -1;
          lowlink = -1;
          is_removed = false;
          def = (noloc, U.empty, [], mknoloc (Internal Types.empty));
        }
    end in
    let open Info in
    let index = ref 0
    and stack = ref [] in
    let res = ref []
    and map = Hashtbl.create 17
    and rank = ref ~-1 in
    let g = Hashtbl.create 17 in
    List.iter
      (fun ((_, v, _, _) as def) -> Hashtbl.add g v { empty with def })
      defs;
    let rec strong_connect v vinfo =
      vinfo.index <- !index;
      vinfo.lowlink <- !index;
      incr index;
      stack := v :: !stack;
      vinfo.is_removed <- false;
      let _, _, _, vdef = vinfo.def in
      pat_iter
        (fun p ->
           match p.descr with
           | PatVar ([ w ], _) -> (
               let winfo =
                 try Some (Hashtbl.find g w) with
                 | Not_found -> None
               in
               match winfo with
               | Some winfo ->
                 if winfo.index == -1 then begin
                   strong_connect w winfo;
                   vinfo.lowlink <- min vinfo.lowlink winfo.lowlink
                 end
                 else if not winfo.is_removed then
                   vinfo.lowlink <- min vinfo.lowlink winfo.index
               | _ -> ())
           | _ -> ())
        vdef;
      if vinfo.lowlink == vinfo.index then begin
        let cc = ref [] in
        incr rank;
        while
          let w = List.hd !stack in
          stack := List.tl !stack;
          let winfo = Hashtbl.find g w in
          let _, _, params, _ = winfo.def in
          (*TODO remove U.get_str*)
          Hashtbl.add map w
            (!rank, List.map (fun v -> (v, Var.mk (U.get_str v))) params);
          cc := winfo.def :: !cc;
          winfo.is_removed <- true;
          not (U.equal w v)
        do
          ()
        done;
        res := (!cc, Hashtbl.copy map) :: !res;
        Hashtbl.clear map
      end
    in
    let () =
      List.iter
        (fun (_, v, _, _) ->
           let vinfo = Hashtbl.find g v in
           if vinfo.index == -1 then strong_connect v vinfo)
        defs
    in
    List.rev !res

  type penv = {
    penv_tenv : t;
    penv_derec : (node * U.t list) Env.t;
    mutable penv_var : (U.t * Var.t) list;
  }

  let penv tenv = { penv_tenv = tenv; penv_derec = Env.empty; penv_var = [] }
  let all_delayed = ref []
  let dummy_params = (-1, [], Hashtbl.create 0)
  let current_params = ref dummy_params
  let to_register = ref []

  let clean_params () =
    current_params := dummy_params;
    to_register := []

  let clean_on_err () =
    all_delayed := [];
    clean_params ()

  let delayed loc =
    let s = mk_delayed () in
    all_delayed := (loc, s) :: !all_delayed;
    s

  let check_one_delayed (loc, p) =
    if not (check_wf p) then Cduce_error.(raise_err_loc ~loc Typer_Error "Ill-formed recursion")

  let check_delayed () =
    let l = !all_delayed in
    all_delayed := [];
    List.iter check_one_delayed l

  let rec comp_var_pat vl pl =
    match (vl, pl) with
    | [], [] -> true
    | (v, _) :: vll, { descr = Poly p; _ } :: pll when U.equal v p ->
      comp_var_pat vll pll
    | _ -> false

  let rec untuple p =
    match p.descr with
    | Prod (p1, p2) -> p1 :: untuple p2
    | _ -> [ p ]

  let rec tuple = function
    | [] -> assert false
    | [ p ] -> p
    | p1 :: rest -> Cduce_loc.mknoloc (Prod (p1, tuple rest))

  let match_type_params l args =
    let aux l args =
      if List.length l == List.length args then Some args else None
    in
    match (l, args) with
    | [ _ ], _ :: _ :: _ -> aux l [ tuple args ]
    | _ :: _ :: _, [ p ] -> aux l (untuple p)
    | _ -> aux l args

  let invalid_instance_error loc s =
    Cduce_error.raise_err_loc ~loc Typer_InvalidRecInst (U.to_string s)

  let rec clean_regexp r =
    match r with
    | Epsilon
    | Elem _
    | Guard _ ->
      r
    | Alt (r1, r2) -> Alt (clean_regexp r1, clean_regexp r2)
    | Star r -> Star (clean_regexp r)
    | WeakStar r -> WeakStar (clean_regexp r)
    | SeqCapture (loc, id, r) -> SeqCapture (loc, id, clean_regexp r)
    | Seq (r1, r2) -> (
        match clean_regexp r1 with
        | Seq (e, rr1) -> Seq (e, clean_regexp (Seq (rr1, r2)))
        | e -> Seq (e, clean_regexp r2))

  let rec derecurs env p =
    let err s = Cduce_error.(mk_loc p.loc (Typer_Pattern, s)) in
    match p.descr with
    | Poly v ->
      let vv =
        try List.assoc v env.penv_var with
        | Not_found ->
          let vv = Var.mk (U.get_str v) in
          env.penv_var <- (v, vv) :: env.penv_var;
          vv
      in
      mk_type (Types.var vv)
    | PatVar ids -> derecurs_var env p.loc ids
    | Recurs (p, b) ->
      let b = List.map (fun (l, n, p) -> (l, n, [], p)) b in
      derecurs (fst (derecurs_def env b)) p
    | Internal t -> mk_type t
    | NsT ns ->
      mk_type
        (Types.atom (AtomSet.any_in_ns (parse_ns env.penv_tenv p.loc ns)))
    | Or (p1, p2) -> mk_or ~err (derecurs env p1) (derecurs env p2)
    | And (p1, p2) -> mk_and ~err (derecurs env p1) (derecurs env p2)
    | Diff (p1, p2) -> mk_diff ~err (derecurs env p1) (derecurs env p2)
    | Prod (p1, p2) -> mk_prod (derecurs env p1) (derecurs env p2)
    | XmlT (p1, p2) -> mk_xml (derecurs env p1) (derecurs env p2)
    | Arrow (p1, p2) -> mk_arrow (derecurs env p1) (derecurs env p2)
    | Optional p -> mk_optional ~err (derecurs env p)
    | Record (o, r) ->
      let aux = function
        | p, Some e -> (derecurs env p, Some (derecurs env e))
        | p, None -> (derecurs env p, None)
      in
      mk_record ~err o (parse_record env.penv_tenv p.loc aux r)
    | Constant (x, c) ->
      mk_constant (ident env.penv_tenv p.loc x) (const env.penv_tenv p.loc c)
    | Cst c -> mk_type (Types.constant (const env.penv_tenv p.loc c))
    | Regexp r -> rexp (derecurs_regexp env (clean_regexp r))
    | Concat (p1, p2) -> mk_concat ~err (derecurs env p1) (derecurs env p2)
    | Merge (p1, p2) -> mk_merge ~err (derecurs env p1) (derecurs env p2)

  and derecurs_regexp env = function
    | Epsilon -> mk_epsilon
    | Elem p -> mk_elem (derecurs env p)
    | Guard p -> mk_guard (derecurs env p)
    | Seq
        ( (Elem { descr = PatVar ((id :: rest as ids), []); loc } as p1),
          ((Elem _ | Seq (Elem _, _)) as p2) ) ->
      let arg, make =
        match p2 with
        | Elem arg -> (arg, fun x -> x)
        | Seq (Elem arg, pp2) ->
          (arg, fun x -> mk_seq x (derecurs_regexp env pp2))
        | _ -> assert false
      in
      let v = ident env.penv_tenv loc id in
      let patch_arg =
        try
          try snd (Env.find v env.penv_derec) != [] with
          | Not_found ->
            let _, pargs =
              if rest == [] then find_local_type env.penv_tenv loc v
              else find_global_type env.penv_tenv loc ids
            in
            pargs != []
        with
        | Not_found -> false
      in
      if patch_arg then
        make (mk_elem (derecurs env { descr = PatVar (ids, [ arg ]); loc }))
      else mk_seq (derecurs_regexp env p1) (derecurs_regexp env p2)
    | Seq (p1, p2) -> mk_seq (derecurs_regexp env p1) (derecurs_regexp env p2)
    | Alt (p1, p2) -> mk_alt (derecurs_regexp env p1) (derecurs_regexp env p2)
    | Star p -> mk_star (derecurs_regexp env p)
    | WeakStar p -> mk_weakstar (derecurs_regexp env p)
    | SeqCapture (loc, x, p) ->
      mk_seqcapt (ident env.penv_tenv loc x) (derecurs_regexp env p)

  and derecurs_var env loc ids =
    match ids with
    | (id :: rest as ids), args -> (
        let cidx, cparams, cmap = !current_params in
        let v = ident env.penv_tenv loc id in
        try
          let node, _ = Env.find v env.penv_derec in
          if args == [] || comp_var_pat cparams args then node
          else invalid_instance_error loc id
        with
        | Not_found -> (
            try
              let (cu, name), (t, pargs), tidx =
                if rest == [] then
                  ( ("", v),
                    find_local_type env.penv_tenv loc v,
                    try fst (Hashtbl.find cmap id) with
                    | Not_found -> ~-1 )
                else
                  let t, pargs = find_global_type env.penv_tenv loc ids in
                  match find_id env.penv_tenv env.penv_tenv loc true id with
                  | ECDuce _
                  | EOCaml _ ->
                    ( ( U.get_str id,
                        ident env.penv_tenv loc
                          (U.mk @@ String.concat "."
                           @@ List.map U.get_str rest) ),
                      (t, pargs),
                      ~-1 )
                  | _ -> assert false
              in
              if cidx >= 0 && tidx == cidx && not (comp_var_pat cparams args)
              then invalid_instance_error loc id;
              let _err s = Error s in
              let l =
                match match_type_params pargs args with
                | Some args ->
                  List.map2 (fun v p -> (v, typ (derecurs env p))) pargs args
                | None ->
                  Cduce_error.raise_err_loc
                    ~loc Typer_InvalidInstArity
                    (U.to_string id, List.length pargs, List.length args)
              in
              let sub = Types.Subst.from_list l in
              let ti = mk_type (Types.Subst.apply_full sub t) in
              to_register := (cu, name, List.map snd l, ti, loc) :: !to_register;
              ti
            with
            | Not_found ->
              assert (rest == []);
              if args != [] then
                Cduce_error.raise_err_loc ~loc Typer_UnboundId (v, true)
              else mk_capture v))
    | _ -> assert false

  and derecurs_def env b =
    let seen = ref IdMap.empty in
    let b =
      List.map
        (fun (loc, v, args, p) ->
           let v = ident env.penv_tenv loc v in
           try
             let old_loc = IdMap.assoc v !seen in
             Cduce_error.raise_err_loc ~loc Typer_MultipleTypeDef (v, old_loc)
           with Not_found ->
             seen := IdMap.add v loc !seen ;
             (v, loc, args, p, delayed loc))
        b
    in
    let env =
      List.fold_left
        (fun env (v, _, a, p, s) ->
           {
             env with
             penv_derec = Env.add v (s, a) env.penv_derec;
             penv_var =
               List.fold_left
                 (fun acc v -> (v, Var.mk (U.get_str v)) :: acc)
                 env.penv_var a;
           })
        env b
    in
    List.iter
      (fun (v, _, a, p, s) ->
         (* Copy. The unknown polymorphic variables that are not already in
               penv_var are introduced for the scope of the current type and
               discarded afterwards.
         *)
         let env = { env with penv_var = env.penv_var } in
         link s (derecurs env p))
      b;
    (env, b)

  let derec penv p =
    let d = derecurs penv p in
    elim_concats ();
    check_delayed ();
    internalize d;
    d

  (* API *)

  let check_no_fv loc n =
    match peek_fv n with
    | None -> ()
    | Some x ->
      Cduce_error.raise_err_loc ~loc Typer_CaptureNotAllowed x
  let type_defs env b =
    let penv, b = derecurs_def (penv env) b in
    elim_concats ();
    check_delayed ();
    let aux loc d =
      internalize d;
      check_no_fv loc d;
      try typ d with
      | Cduce_error.Error ((Located loc' | PreciselyLocated (loc', _)) , (Typer_Pattern, s)) ->
        Cduce_error.raise_err_loc ~loc:loc' Typer_Error s
      | Cduce_error.Error (Unlocated, (Typer_Pattern, s)) -> Cduce_error.raise_err_loc ~loc Generic s
    in
    let b =
      List.map
        (fun (v, loc, args, _, d) ->
           let t_rhs = aux loc d in
           if loc <> noloc && Types.is_empty t_rhs then
             Cduce_error.(warning ~loc
                            ("This definition yields an empty type for " ^ Ident.to_string v) ());

           let vars_rhs = Types.Subst.vars t_rhs in
           let vars_lhs = Var.Set.from_list (List.map snd penv.penv_var) in
           let undecl = Var.Set.diff vars_rhs vars_lhs in
           if not (Var.Set.is_empty undecl) then
             Cduce_error.raise_err_loc ~loc
               Typer_UnboundTypeVariable (v, Var.Set.choose undecl);
           (* recreate the mapping in the correct order *)
           let vars_args = List.map (fun v -> List.assoc v penv.penv_var) args in
           let final_vars =
             (* create a sequence 'a -> 'a_0 for all variables *)
             List.map (fun v -> (v, Var.(mk (name v)))) vars_args
           in
           let subst =
             Types.Subst.from_list
               (List.map (fun (v, vv) -> (v, Types.var vv)) final_vars)
           in
           let t_rhs = Types.Subst.apply_full subst t_rhs in
           (v, t_rhs, List.map snd final_vars))
        (List.rev b)
    in
    List.iter
      (fun (v, t, al) ->
         let params = List.map Types.var al in
         Types.Print.register_global "" v ~params t)
      b;
    let env = enter_types b env in
    List.iter
      (fun (cu, name, params, ti, loc) ->
         let tti = aux loc ti in
         Types.Print.register_global cu name ~params tti)
      !to_register;
    env

  let equal_params l1 l2 =
    try List.for_all2 U.equal l1 l2 with
    | _ -> false

  let check_params l =
    match l with
    | [] -> assert false
    | [ (_, u, _, _) ] -> u
    | (loc1, u, p, _) :: r -> (
        try
          let loc2, v, _, _ =
            List.find (fun (_, _, q, _) -> not (equal_params p q)) r
          in
          let loc = merge_loc loc1 loc2 in
          Cduce_error.raise_err_loc ~loc
            Typer_Error (Printf.sprintf (* TODO create an error *)
                           "mutually recursive types %s and %s have different arities"
                           (U.to_string u) (U.to_string v))
        with
        | Not_found -> u)

  let type_defs env b =
    try
      let b = scc b in
      let r =
        List.fold_left
          (fun env (b, map) ->
             let u = check_params b in
             let idx, params = Hashtbl.find map u in
             current_params := (idx, params, map);
             type_defs env b)
          env b
      in
      clean_params ();
      r
    with
    | exn ->
      clean_on_err ();
      raise exn

  let typ vars env t =
    let aux loc d =
      internalize d;
      check_no_fv loc d;
      try typ_node d with
      | Cduce_error.Error (_, (Typer_Pattern, s)) -> Cduce_error.raise_err_loc ~loc Typer_Error s
    in
    try
      let penv = { (penv env) with penv_var = vars } in
      let d = derec penv t in
      let res = aux t.loc d in
      List.iter
        (fun (cu, name, params, ti, loc) ->
           let tti = aux loc ti in
           Types.Print.register_global cu name ~params (Types.descr tti))
        !to_register;
      clean_params ();
      res
    with
    | exn ->
      clean_on_err ();
      raise exn

  let pat env t =
    try
      let d = derec (penv env) t in
      try pat_node d with
      | Cduce_error.Error (_, (Typer_Pattern, s)) ->
        Cduce_error.raise_err_loc ~loc:t.loc Typer_Error s
    with
    | exn ->
      clean_on_err ();
      raise exn
end

let typ = IType.typ []
let var_typ = IType.typ
let pat = IType.pat
let type_defs = IType.type_defs

let dump_types ppf env =
  Env.iter
    (fun v -> function
       | Type _ -> Format.fprintf ppf " %a" Ident.print v
       | _ -> ())
    env.ids

let dump_ns ppf env = Ns.dump_table ppf env.ns

(* II. Build skeleton *)

type type_fun = Cduce_loc.loc -> Types.t -> bool -> Types.t

module Fv = IdSet

type branch = Branch of Typed.branch * branch list

let cur_branch : branch list ref = ref []

let exp' loc e =
  { Typed.exp_loc = loc; Typed.exp_typ = Types.empty; Typed.exp_descr = e }

let exp loc fv e = (fv, exp' loc e)
let exp_nil = exp' noloc (Typed.Cst Types.Sequence.nil_cst)

let pat_true =
  let n = Patterns.make Fv.empty in
  Patterns.define n (Patterns.constr Builtin_defs.true_type);
  n

let pat_false =
  let n = Patterns.make Fv.empty in
  Patterns.define n (Patterns.constr Builtin_defs.false_type);
  n

let ops = Hashtbl.create 13
let register_op op arity f = Hashtbl.add ops op (arity, f)
let typ_op op = snd (Hashtbl.find ops op)

let fun_name env a =
  match a.fun_name with
  | None -> None
  | Some (loc, s) -> Some (ident env loc s)

let is_op env s =
  if Env.mem s env.ids then None
  else
    let ns, s = s in
    if Ns.Uri.equal ns Ns.empty then
      let s = U.get_str s in
      try
        let o = Hashtbl.find ops s in
        Some (s, fst o)
      with
      | Not_found -> None
    else None

module USet = Set.Make (U)

let collect_vars acc p =
  let vset = ref acc in
  pat_iter
    (function
      | { descr = Poly v; _ } -> vset := USet.add v !vset
      | _ -> ())
    p;
  !vset

let rec get_dot_for_annot loc expr =
  match expr with
  | Dot _ -> expr
  | LocatedExpr (_, e) -> get_dot_for_annot loc e
  | e -> Cduce_error.raise_err_loc ~loc Typer_Error "Only OCaml external can have type arguments"

let rec expr env loc = function
  | LocatedExpr (loc, e) -> expr env loc e
  | Forget (e, t) ->
    let fv, e = expr env loc e
    and t = typ env t in
    exp loc fv (Typed.Forget (e, t))
  | Check (e, t) ->
    let fv, e = expr env loc e
    and t = typ env t in
    exp loc fv (Typed.Check (ref Types.empty, e, t))
  | Var s -> var env loc s
  | Apply (e1, e2) -> (
      let fv1, e1 = expr env loc e1
      and fv2, e2 = expr env loc e2 in
      let fv = Fv.cup fv1 fv2 in
      match e1.Typed.exp_descr with
      | Typed.Op (op, arity, args) when arity > 0 ->
        exp loc fv (Typed.Op (op, arity - 1, args @ [ e2 ]))
      | _ -> exp loc fv (Typed.Apply (e1, e2)))
  | Abstraction a -> abstraction env loc a
  | (Integer _ | Char _ | Atom _ | Const _) as c ->
    exp loc Fv.empty (Typed.Cst (const env loc c))
  | Abstract v -> exp loc Fv.empty (Typed.Abstract v)
  | Pair (e1, e2) ->
    let fv1, e1 = expr env loc e1
    and fv2, e2 = expr env loc e2 in
    exp loc (Fv.cup fv1 fv2) (Typed.Pair (e1, e2))
  | Xml (e1, e2) ->
    let fv1, e1 = expr env loc e1
    and fv2, e2 = expr env loc e2 in
    let n = if env.keep_ns then Some env.ns else None in
    exp loc (Fv.cup fv1 fv2) (Typed.Xml (e1, e2, n))
  | Dot _ as e -> dot loc env e []
  | TyArgs (e, args) ->
    (*let e = get_dot_for_annot loc e in*)
    dot loc env e args
  | RemoveField (e, l) ->
    let fv, e = expr env loc e in
    exp loc fv (Typed.RemoveField (e, parse_label env loc l))
  | RecordLitt r ->
    let fv = ref Fv.empty in
    let r =
      parse_record env loc
        (fun e ->
           let fv2, e = expr env loc e in
           fv := Fv.cup !fv fv2;
           e)
        r
    in
    exp loc !fv (Typed.RecordLitt r)
  | String (i, j, s, e) ->
    let fv, e = expr env loc e in
    exp loc fv (Typed.String (i, j, s, e))
  | Match (e, b) ->
    let fv1, e = expr env loc e
    and fv2, b = branches env b in
    exp loc (Fv.cup fv1 fv2) (Typed.Match (e, b))
  | Map (e, b) ->
    let fv1, e = expr env loc e
    and fv2, b = branches env b in
    exp loc (Fv.cup fv1 fv2) (Typed.Map (e, b))
  | Transform (e, b) ->
    let fv1, e = expr env loc e
    and fv2, b = branches env b in
    exp loc (Fv.cup fv1 fv2) (Typed.Transform (e, b))
  | Xtrans (e, b) ->
    let fv1, e = expr env loc e
    and fv2, b = branches env b in
    exp loc (Fv.cup fv1 fv2) (Typed.Xtrans (e, b))
  | Validate (e, ids) ->
    let fv, e = expr env loc e in
    let t, v = find_global_schema_component env loc ids in
    exp loc fv (Typed.Validate (e, t, v))
  | SelectFW (e, from, where) -> select_from_where env loc e from where
  | Try (e, b) ->
    let fv1, e = expr env loc e
    and fv2, b = branches env b in
    exp loc (Fv.cup fv1 fv2) (Typed.Try (e, b))
  | NamespaceIn (pr, ns, e) ->
    let env = type_ns env loc pr ns in
    expr env loc e
  | KeepNsIn (k, e) -> expr (type_keep_ns env k) loc e
  | Ref (e, t) ->
    let fv, e = expr env loc e
    and t = var_typ env.poly_vars env t in
    exp loc fv (Typed.Ref (e, t))

and if_then_else loc cond yes no =
  let b =
    {
      Typed.br_typ = Types.empty;
      Typed.br_branches =
        [
          {
            Typed.br_loc = yes.Typed.exp_loc;
            Typed.br_used = false;
            Typed.br_ghost = false;
            Typed.br_vars_empty = Fv.empty;
            Typed.br_pat = pat_true;
            Typed.br_body = yes;
          };
          {
            Typed.br_loc = no.Typed.exp_loc;
            Typed.br_used = false;
            Typed.br_ghost = false;
            Typed.br_vars_empty = Fv.empty;
            Typed.br_pat = pat_false;
            Typed.br_body = no;
          };
        ];
      Typed.br_accept = Builtin_defs.bool;
    }
  in
  exp' loc (Typed.Match (cond, b))

and dot loc env0 e args =
  let dot_access loc (fv, e) l =
    exp loc fv (Typed.Dot (e, parse_label env0 loc l))
  in

  let no_args () =
    if args <> [] then Cduce_error.raise_err_loc ~loc Typer_Error "Only OCaml externals can have type arguments"
  in
  let rec aux loc = function
    | LocatedExpr (loc, e) -> aux loc e
    | Dot (e, id) -> (
        match aux loc e with
        | `Val e -> `Val (dot_access loc e id)
        | `Comp c -> `Comp (navig loc env0 c id))
    | Var id -> (
        match find_id_comp env0 env0 loc id with
        | Val _ -> `Val (var env0 loc id)
        | c -> `Comp (env0, c))
    | e -> `Val (expr env0 loc e)
  in
  match aux loc e with
  | `Val e ->
    no_args ();
    e
  | `Comp (_, EVal (cu, id, t)) ->
    no_args ();
    exp loc Fv.empty (Typed.ExtVar (cu, id, t))
  | `Comp (_, EOCamlComponent s) -> extern loc env0 s args
  | _ -> Cduce_error.raise_err_loc ~loc Typer_Error "This dot notation does not refer to a value"

and extern loc env s args =
  let args = List.map (typ env) args in
  try
    let i, t =
      let i, t = Externals.resolve s args in
        if !has_static_external s then
          (`Builtin (s,i), t)
        else
          (`Ext i, t)
    in
    exp loc Fv.empty (Typed.External (t, i))
  with
  | Cduce_error.Error (Unlocated, (err, arg)) -> Cduce_error.raise_err_loc ~loc err arg
  | Cduce_error.Error (Located loc, (err, arg)) -> Cduce_error.raise_err_loc ~loc err arg
  | Cduce_error.Error (PreciselyLocated (loc, _), (err, arg)) -> Cduce_error.raise_err_loc ~loc err arg
  | exn -> Cduce_error.raise_err_loc ~loc Other_Exn exn

and var env loc s =
  let id = ident env loc s in
  match is_op env id with
  | Some (s, arity) ->
    let e =
      match s with
      | "print_xml"
      | "print_xml_utf8" ->
        Typed.NsTable (env.ns, Typed.Op (s, arity, []))
      | "load_xml" when env.keep_ns -> Typed.Op ("!load_xml", arity, [])
      | _ -> Typed.Op (s, arity, [])
    in
    exp loc Fv.empty e
  | None -> (
      try
        match Env.find id env.ids with
        | Val _ -> exp loc (Fv.singleton id) (Typed.Var id)
        | EVal (cu, id, t) -> exp loc Fv.empty (Typed.ExtVar (cu, id, t))
        | _ -> Cduce_error.raise_err_loc ~loc Typer_Error "This identifier does not refer to a value"
      with
      | Not_found -> Cduce_error.raise_err_loc ~loc Typer_UnboundId (id, false))

and abstraction env loc a =
  (* When entering a function (fun 'a 'b ... .('a -> 'b -> … ))
     for each variable 'x from the interface
     - if 'x is in env.poly_vars, it is bound higher in the AST, we need
       to keep the associated unique variable
     - if 'x is not in env.poly_vars or 'x is in a.fun_poly, a fresh
       name must be generated and kept for subsequent use of the variable.
  *)
  let vset =
    (* collect all type variables from the interface*)
    List.fold_left
      (fun acc (t1, t2) -> collect_vars (collect_vars acc t1) t2)
      USet.empty a.fun_iface
  in
  let vset =
    (* remove variables that are in scope *)
    List.fold_left (fun acc (v, _) -> USet.remove v acc) vset env.poly_vars
  in
  let vset = List.fold_left (fun acc v -> USet.add v acc) vset a.fun_poly in
  (* add those that are explicitely polymorphic. *)
  let all_vars =
    USet.fold (fun v acc -> (v, Var.mk (U.get_str v)) :: acc) vset env.poly_vars
  in
  let iface =
    List.map
      (fun (t1, t2) -> (var_typ all_vars env t1, var_typ all_vars env t2))
      a.fun_iface
  in
  let env = { env with poly_vars = all_vars } in
  let t =
    List.fold_left
      (fun accu (t1, t2) -> Types.cap accu (Types.arrow t1 t2))
      Types.any iface
  in
  let iface =
    List.map (fun (t1, t2) -> (Types.descr t1, Types.descr t2)) iface
  in
  let fun_name = fun_name env a in
  let env' =
    match fun_name with
    | None -> env
    | Some f -> enter_values_dummy [ f ] env
  in
  let fv0, body = branches env' a.fun_body in
  let fv =
    match fun_name with
    | None -> fv0
    | Some f -> Fv.remove f fv0
  in
  let e =
    Typed.Abstraction
      {
        Typed.fun_name;
        Typed.fun_iface = iface;
        Typed.fun_body = body;
        Typed.fun_typ = t;
        Typed.fun_fv = fv;
        Typed.fun_is_poly = not (Var.Set.is_empty (Types.Subst.vars t));
      }
  in
  exp loc fv e

and branches env b =
  let fv = ref Fv.empty in
  let accept = ref Types.empty in
  let branch (p, e) =
    let cur_br = !cur_branch in
    cur_branch := [];
    let ploc = p.loc in
    let p = pat env p in
    let fvp = Patterns.fv p in
    let fv2, e = expr (enter_values_dummy (fvp :> Id.t list) env) noloc e in
    let br_loc = merge_loc ploc e.Typed.exp_loc in
    (match Fv.pick (Fv.diff fvp fv2) with
     | None -> ()
     | Some x ->
       let x = Ident.to_string x in
       warning br_loc
         ("The capture variable " ^ x
          ^ " is declared in the pattern but not used in the body of this \
             branch. It might be a misspelled or undeclared type or name (if it \
             isn't, use _ instead)."));
    let fv2 = Fv.diff fv2 fvp in
    fv := Fv.cup !fv fv2;
    accept := Types.cup !accept (Types.descr (Patterns.accept p));
    let ghost = br_loc == noloc in
    let br =
      {
        Typed.br_loc;
        Typed.br_used = ghost;
        Typed.br_ghost = ghost;
        Typed.br_vars_empty = fvp;
        Typed.br_pat = p;
        Typed.br_body = e;
      }
    in
    cur_branch := Branch (br, !cur_branch) :: cur_br;
    br
  in
  let b = List.map branch b in
  ( !fv,
    {
      Typed.br_typ = Types.empty;
      Typed.br_branches = b;
      Typed.br_accept = !accept;
    } )

and select_from_where env loc e from where =
  let env = ref env in
  let all_fv = ref Fv.empty in
  let bound_fv = ref Fv.empty in
  let clause (p, e) =
    let ploc = p.loc in
    let p = pat !env p in
    let fvp = Patterns.fv p in
    let fv2, e = expr !env noloc e in
    env := enter_values_dummy (fvp :> Id.t list) !env;
    all_fv := Fv.cup (Fv.diff fv2 !bound_fv) !all_fv;
    bound_fv := Fv.cup fvp !bound_fv;
    (ploc, p, fvp, e)
  in
  let from = List.map clause from in
  let where = List.map (expr !env noloc) where in

  let put_cond rest (fv, cond) =
    all_fv := Fv.cup (Fv.diff fv !bound_fv) !all_fv;
    if_then_else loc cond rest exp_nil
  in
  let aux (ploc, p, fvp, e) (where, rest) =
    (* Put here the conditions that depends on variables in fvp *)
    let above, here = List.partition (fun (v, _) -> Fv.disjoint v fvp) where in
    (* if cond then ... else [] *)
    let rest = List.fold_left put_cond rest here in
    (* transform e with p -> ... *)
    let br =
      {
        Typed.br_loc = ploc;
        Typed.br_used = false;
        Typed.br_ghost = false;
        Typed.br_vars_empty = fvp;
        Typed.br_pat = p;
        Typed.br_body = rest;
      }
    in
    cur_branch := [ Branch (br, !cur_branch) ];
    let b =
      {
        Typed.br_typ = Types.empty;
        Typed.br_branches = [ br ];
        Typed.br_accept = Types.descr (Patterns.accept p);
      }
    in
    let br_loc = merge_loc ploc e.Typed.exp_loc in
    (above, exp' br_loc (Typed.Transform (e, b)))
  in
  let cur_br = !cur_branch in
  cur_branch := [];
  let fv, e = expr !env noloc (Pair (e, cst_nil)) in
  cur_branch := !cur_branch @ cur_br;
  let where, rest = List.fold_right aux from (where, e) in
  (* The remaining conditions are constant. Gives a warning for that. *)
  (match where with
   | (_, e) :: _ ->
     warning e.Typed.exp_loc
       "This 'where' condition does not depend on any captured variable"
   | _ -> ());
  let rest = List.fold_left put_cond rest where in
  (Fv.cup !all_fv (Fv.diff fv !bound_fv), rest)

let expr env e = snd (expr env noloc e)

let let_decl env p e =
  { Typed.let_pat = pat env p; Typed.let_body = expr env e }

(* Hide global "typing/parsing" environment *)

(* III. Type-checks *)

open Typed

let any_node = Types.(cons any)

let localize loc f x =
  try f x with
  | Cduce_error.Error (_, (Typer_Error, msg)) -> Cduce_error.raise_err_loc ~loc Typer_Error msg
  | Cduce_error.Error (_,(Typer_Constraint, arg)) -> Cduce_error.raise_err_loc ~loc Typer_Constraint arg


let raise_constraint_exn ?loc ?precise t s =
  let open Cduce_error in
  let r  (type a) (e : a error_t) (a : a) =
    match loc, precise with 
      None, _ -> raise_err e a
    | Some loc, Some precise -> raise_err_precise ~loc precise e a
    | Some loc, None -> raise_err_loc ~loc e a
  in
  if
    Var.Set.is_empty (Types.Subst.vars t)
    && Var.Set.is_empty (Types.Subst.vars s)
  then r Typer_Constraint (t, s)
  else r Typer_ShouldHave2 (t, "but its inferred type is:", s)

let require loc t s =
  if not (Types.subtype t s) then raise_constraint_exn ~loc t s

let verify loc t s =
  require loc t s;
  t

let verify_noloc t s =
  if not (Types.subtype t s) then raise_constraint_exn t s;
  t

let check_str loc ofs t s =
  if not (Types.subtype t s) then raise_constraint_exn ~loc ~precise:(`Char ofs) t s;
  t

let should_have loc constr s = Cduce_error.raise_err_loc ~loc Typer_ShouldHave (constr, s)

let should_have_str loc ofs constr s =
  Cduce_error.raise_err_precise  ~loc (`Char ofs) Typer_ShouldHave (constr, s)

let flatten arg loc constr precise =
  let open Types in
  let constr' =
    Sequence.star (Sequence.approx (Types.cap Sequence.any constr))
  in
  let sconstr' = Sequence.star constr' in
  let exact = Types.subtype constr' constr in
  if exact then
    let t = arg loc sconstr' precise in
    if precise then Sequence.flatten t else constr
  else
    let t = arg loc sconstr' true in
    verify loc (Sequence.flatten t) constr

let pat_any () =
  let n = Patterns.make IdSet.empty in
  Patterns.define n (Patterns.constr Types.any);
  n

let pat_var id =
  let s = IdSet.singleton id in
  let n = Patterns.make s in
  Patterns.define n (Patterns.capture id);
  n

let pat_node d fv =
  let n = Patterns.make fv in
  Patterns.define n d;
  n

let pat_pair kind e1 e2 fv =
  pat_node
    ((match kind with
        | `Normal -> Patterns.times
        | `Xml -> Patterns.xml)
       e1 e2)
    fv

let pat_cap p1 p2 =
  let fv1 = Patterns.fv p1 in
  let fv2 = Patterns.fv p2 in
  pat_node Patterns.(cap (descr p1) (descr p2)) (IdSet.cup fv1 fv2)

let rec pat_of_expr fv te =
  match te.exp_descr with
  | Var s when not (IdSet.mem fv s) -> pat_var s
  | Pair (e1, e2)
  | Xml (e1, e2, _) ->
    let kind =
      match te.exp_descr with
      | Pair _ -> `Normal
      | _ -> `Xml
    in
    let p1 = pat_of_expr fv e1
    and p2 = pat_of_expr fv e2 in
    let fv1 = Patterns.fv p1 in
    let fv2 = Patterns.fv p2 in
    pat_pair kind p1 p2 (IdSet.cup fv1 fv2)
  | RecordLitt lmap ->
    List.fold_left
      (fun acc (lab, tel) ->
         let pel = pat_of_expr fv tel in
         let prec = pat_node (Patterns.record lab pel) (Patterns.fv pel) in
         pat_cap prec acc)
      (pat_any ()) (LabelMap.get lmap)
  | _ -> pat_any ()

let refine_pat p1 op2 =
  match op2 with
  | None -> p1
  | Some te ->
    let fv = Patterns.fv p1 in
    let p2 = pat_of_expr fv te in
    pat_cap p1 p2

let rec type_check env e constr precise =
  let d = type_check' e.exp_loc env e.exp_descr constr precise in
  let d = if precise then d else constr in
  e.exp_typ <- Types.cup e.exp_typ d;
  d

and type_check' loc env e constr precise =
  match e with
  | Forget (e, t) ->
    let t = Types.descr t in
    ignore (type_check env e t false);
    verify loc t constr
  | Check (t0, e, t) ->
    if Var.Set.is_empty Types.(Subst.vars (descr t)) then (
      let te = type_check env e Types.any true in
      t0 := Types.cup !t0 te;
      verify loc (Types.cap te (Types.descr t)) constr)
    else
      Cduce_error.raise_err_loc
        ~loc Typer_Error "Polymorphic type variables cannot occur in dynamic type-checks"
  | Abstraction a ->
    let t =
      if Types.subtype a.fun_typ constr then a.fun_typ
      else
        let name =
          match a.fun_name with
          | Some s -> "abstraction " ^ Ident.to_string s
          | None -> "the abstraction"
        in
        should_have loc constr
          (Format.asprintf
             "but the interface (%a) of %s is not compatible with type\n\
             \            (%a)" Types.Print.print a.fun_typ name
             Types.Print.print constr)
    in
    let env =
      {
        env with
        mono_vars = Var.Set.cup env.mono_vars (Types.Subst.vars a.fun_typ);
      }
    in
    let env =
      match a.fun_name with
      | None -> env
      | Some f -> enter_value f a.fun_typ env
    in
    List.iter
      (fun (t1, t2) ->
         let acc = a.fun_body.br_accept in
         if not (Types.subtype t1 acc) then
           Cduce_error.(raise_err_loc  ~loc Typer_NonExhaustive (Types.diff t1 acc));
         ignore (type_check_branches loc env t1 a.fun_body t2 false))
      a.fun_iface;
    t
  | Match (e, b) ->
    let t = type_check env e b.br_accept true in
    type_check_branches loc env t b constr precise
  | Try (e, b) ->
    let te = type_check env e constr precise in
    let tb = type_check_branches loc env Types.any b constr precise in
    Types.cup te tb
  | Pair (e1, e2) -> type_check_pair loc env e1 e2 constr precise
  | Xml (e1, e2, _) -> type_check_pair ~kind:`XML loc env e1 e2 constr precise
  | RecordLitt r -> type_record loc env r constr precise
  | Map (e, b) -> type_map loc env false e b constr precise
  | Transform (e, b) ->
    localize loc (flatten (fun l -> type_map loc env true e b) loc constr) precise
  | Apply (e1, e2) ->
    let t1 = type_check env e1 Types.Function.any true in
    let t1arrow = Types.Arrow.get t1 in
    let dom = Types.Arrow.domain t1arrow in
    let t2 = type_check env e2 Types.any true in
    let res =
      if
        Var.Set.is_empty (Types.Subst.vars t1)
        && Var.Set.is_empty (Types.Subst.vars t2)
      then
        (* TODO don't retype e2 *)
        let t2 = type_check env e2 dom true in
        Types.Arrow.apply t1arrow t2
      else
        match Types.Tallying.apply_raw env.mono_vars t1 t2 with
        | None -> raise_constraint_exn ~loc t2 dom
        | Some (subst, tx, ty, res) ->
          List.iter
            (fun s ->
               Var.Map.iteri
                 (fun wv t ->
                    match Var.kind wv with
                    | `generated
                    | `user ->
                      ()
                    | `weak ->
                      let tt = Types.Subst.clean_type env.mono_vars t in
                      env.weak_vars <-
                        Var.Map.update
                          (fun prev next ->
                             match (prev, next) with
                             | None, Some t -> next
                             | Some tp, Some tn when Types.equiv tp tn -> next
                             | Some tp, Some tn ->
                               Cduce_error.(raise_err_loc ~loc
                                              Typer_Error (Format.asprintf
                                                             "the weak polymorphic variable %a is \
                                                              instantiated several times in the \
                                                              same expression."
                                                             Var.print wv))
                             | _ -> assert false)
                          wv (Some tt) env.weak_vars)
                 s)
            subst;
          res
        (*
        if Types.Arrow.need_arg t1 then
          let t2 = type_check env e2 dom true in
          Types.Arrow.apply t1 t2
        else (
          ignore (type_check env e2 dom false);
          Types.Arrow.apply_noarg t1) *)
    in
    verify loc res constr
  | Var s -> verify loc (find_value s env) constr
  | ExtVar (cu, s, t) -> verify loc t constr
  | Cst c -> verify loc (Types.constant c) constr
  | Abstract (t, _) -> verify loc Types.(abstract (AbstractSet.atom t)) constr
  | String (i, j, s, e) -> type_check_string loc env 0 s i j e constr precise
  | Dot (e, l) -> (
      let expect_rec = Types.record l (Types.cons constr) in
      let expect_elt =
        Types.xml any_node
          (Types.cons (Types.times (Types.cons expect_rec) any_node))
      in
      let t = type_check env e (Types.cup expect_rec expect_elt) precise in
      let t_elt =
        let t = Types.Product.pi2 (Types.Product.get ~kind:`XML t) in
        let t = Types.Product.pi1 (Types.Product.get t) in
        t
      in
      if not precise then constr
      else
        try Types.Record.project (Types.cup t t_elt) l with
        | Not_found -> assert false)
  | RemoveField (e, l) ->
    let t = type_check env e Types.Rec.any true in
    let t = Types.Record.remove_field t l in
    verify loc t constr
  | Xtrans (e, b) ->
    let t = type_check env e Types.Sequence.any true in
    let t =
      try
        Types.Sequence.map_tree constr
          (fun cstr t ->
             let resid = Types.diff t b.br_accept in
             let res = type_check_branches loc env t b cstr true in
             (res, resid))
          t
      with
      | Types.Sequence.Error _ as exn -> (
          let rec find_loc = function
            | Cduce_error.Error (PreciselyLocated (loc, precise), _) -> ((loc, precise), exn)
            | Types.Sequence.(Error (Types.Sequence.UnderTag (t, exn))) ->
              let l, exn = find_loc exn in
              (l, Types.Sequence.Error (Types.Sequence.UnderTag (t, exn)))
            | exn -> raise Not_found
          in
          try
            let (loc, precise), exn = find_loc exn in
            Cduce_error.raise_err_precise ~loc precise Other_Exn exn
          with
          | Not_found -> Cduce_error.raise_err_loc ~loc Other_Exn exn)
    in
    verify loc t constr
  | Validate (e, t, _) ->
    ignore (type_check env e Types.any false);
    verify loc t constr
  | Ref (e, t) ->
    ignore (type_check env e (Types.descr t) false);
    verify loc (Builtin_defs.ref_type t) constr
  | External (t, _) -> verify loc t constr
  | Op (op, _, args) ->
    let args : type_fun list = List.map (fun e (_:Cduce_loc.loc) -> type_check env e ) args in
    let t = localize loc (typ_op op args loc constr) precise in
    verify loc t constr
  | NsTable (ns, e) -> type_check' loc env e constr precise

and type_check_pair ?(kind = `Normal) loc env e1 e2 constr precise =
  let rects = Types.Product.normal ~kind constr in
  (if Types.Product.is_empty rects then
     match kind with
     | `Normal -> should_have loc constr "but it is a pair"
     | `XML -> should_have loc constr "but it is an XML element");
  let need_s = Types.Product.need_second rects in
  let t1 = type_check env e1 (Types.Product.pi1 rects) (precise || need_s) in
  let c2 = Types.Product.constraint_on_2 rects t1 in
  if Types.is_empty c2 then
    Cduce_error.raise_err_loc ~loc
      Typer_ShouldHave2 (constr, "but the first component has type:", t1);
  let t2 = type_check env e2 c2 precise in
  if precise then
    match kind with
    | `Normal -> Types.times (Types.cons t1) (Types.cons t2)
    | `XML -> Types.xml (Types.cons t1) (Types.cons t2)
  else constr

and type_check_string loc env ofs s i j e constr precise =
  if U.equal_index i j then type_check env e constr precise
  else
    let rects = Types.Product.normal constr in
    if Types.Product.is_empty rects then
      should_have_str loc ofs constr "but it is a string"
    else
      let ch, i' = U.next s i in
      let ch = CharSet.V.mk_int ch in
      let tch = Types.constant (Types.Char ch) in
      let t1 = check_str loc ofs tch (Types.Product.pi1 rects) in
      let c2 = Types.Product.constraint_on_2 rects t1 in
      let t2 = type_check_string loc env (ofs + 1) s i' j e c2 precise in
      if precise then Types.times (Types.cons t1) (Types.cons t2) else constr

and type_record loc env r constr precise =
  if not (Types.Record.has_record constr) then
    should_have loc constr "but it is a record";
  let rconstr, res =
    List.fold_left
      (fun (rconstr, res) (l, e) ->
         let r = Types.Record.focus rconstr l in
         let pi = Types.Record.get_this r in
         (if Types.is_empty pi then
            let l = Label.string_of_attr l in
            should_have loc constr
              (Printf.sprintf "Field %s is not allowed here." l));
         let t = type_check env e pi (precise || Types.Record.need_others r) in
         let rconstr = Types.Record.constraint_on_others r t in
         (if Types.is_empty rconstr then
            let l = Label.string_of_attr l in
            should_have loc constr
              (Printf.sprintf "Type of field %s is not precise enough." l));

         let res = if precise then LabelMap.add l (Types.cons t) res else res in
         (rconstr, res))
      (constr, LabelMap.empty) (LabelMap.get r)
  in
  if not (Types.Record.has_empty_record rconstr) then
    should_have loc constr "More fields should be present";
  if precise then Types.record_fields (false, res) else constr

and type_check_branches ?expr loc env targ brs constr precise =
  if Types.is_empty targ then Types.empty
  else (
    brs.br_typ <- Types.cup brs.br_typ targ;
    branches_aux expr loc env targ
      (if precise then Types.empty else constr)
      constr precise brs.br_branches)

and branches_aux expr loc env targ tres constr precise = function
  | [] -> tres
  | b :: rem ->
    let p = refine_pat b.br_pat expr in
    let acc = Types.descr (Patterns.accept p) in
    let targ' = Types.cap targ acc in
    if Types.is_empty targ' then
      branches_aux expr loc env targ tres constr precise rem
    else (
      b.br_used <- true;
      let res = Patterns.filter targ' p in
      let res = IdMap.map Types.descr res in

      b.br_vars_empty <-
        IdMap.domain
          (IdMap.filter
             (fun x t -> Types.(subtype t Sequence.nil_type))
             (IdMap.restrict res b.br_vars_empty));

      let env' = enter_values (IdMap.get res) env in
      let t = type_check env' b.br_body constr precise in
      let tres = if precise then Types.cup t tres else tres in
      let targ'' = Types.diff targ acc in
      if Types.non_empty targ'' then
        branches_aux expr loc env targ'' tres constr precise rem
      else tres)

and type_map loc env def e b constr precise =
  let open Types in
  let acc = if def then Sequence.any else Sequence.star b.br_accept in
  let t = type_check env e acc true in

  let constr' = Sequence.approx (Types.cap Sequence.any constr) in
  let exact = Types.subtype (Sequence.star constr') constr in
  (* Note:
     - could be more precise by integrating the decomposition
       of constr inside Sequence.map.
  *)
  let res =
    Sequence.map
      (fun t ->
         let res =
           type_check_branches loc env t b constr' (precise || not exact)
         in
         if def && not (Types.subtype t b.br_accept) then (
           require loc Sequence.nil_type constr';
           Types.cup res Sequence.nil_type)
         else res)
      t
  in
  if exact then res else verify loc res constr

and type_let_decl env l =
  let acc = Types.descr (Patterns.accept l.let_pat) in
  let t = type_check env l.let_body acc true in
  let res = Patterns.filter t l.let_pat in
  IdMap.mapi_to_list (fun x t -> (x, Types.descr t)) res

and type_rec_funs env l =
  let typs =
    List.fold_left
      (fun accu -> function
         | {
           exp_descr = Abstraction { fun_typ = t; fun_name = Some f };
           exp_loc = loc;
         } ->
           if not (value_name_ok f env) then
             Cduce_error.raise_err_loc ~loc
               Typer_Error "This function name clashes with another kind of identifier";
           (f, t) :: accu
         | _ -> assert false)
      [] l
  in
  let env = enter_values typs env in
  List.iter (fun e -> ignore (type_check env e Types.any false)) l;
  typs

let rec unused_branches b =
  List.iter
    (fun (Branch (br, s)) ->
       if br.br_ghost then ()
       else if not br.br_used then warning br.br_loc "This branch is not used"
       else (
         (if not (IdSet.is_empty br.br_vars_empty) then
            let msg =
              try
                let l =
                  List.map
                    (fun x ->
                       let x = Ident.to_string x in
                       if String.compare x "$$$" = 0 then raise Exit else x)
                    (br.br_vars_empty :> Id.t list)
                in
                let l = String.concat "," l in
                "The following variables always match the empty sequence: " ^ l
              with
              | Exit -> "This projection always returns the empty sequence"
            in
            warning br.br_loc msg);
         unused_branches s))
    b

let report_unused_branches () =
  unused_branches !cur_branch;
  cur_branch := []

let clear_unused_branches () = cur_branch := []

(* API *)
let update_weak_variables env =
  let to_patch, to_keep =
    Var.Map.split
      (fun v -> function
         | Some _ -> true
         | None -> false)
      env.weak_vars
  in
  let to_patch =
    Var.Map.map
      (function
        | Some t -> t
        | None -> assert false)
      to_patch
  in
  {
    env with
    ids =
      Env.mapi
        (fun v -> function
           | Val t ->
             let tt = Types.Subst.apply_full to_patch t in
             Val tt
           | x -> x)
        env.ids;
    weak_vars = to_keep;
  }

let type_expr env e =
  clear_unused_branches ();
  let e = expr env e in
  let t = type_check env e Types.any true in
  report_unused_branches ();
  (update_weak_variables env, e, t)

let type_let_decl env p e =
  clear_unused_branches ();
  let decl = let_decl env p e in
  let typs = type_let_decl env decl in
  report_unused_branches ();
  let is_value = Typed.is_value decl.let_body in
  (* patch env to update weak variables *)
  let env = update_weak_variables env in
  let typs =
    List.map
      (fun (id, t) ->
         let tt = Types.Subst.clean_type Var.Set.empty t in
         let vars = Types.Subst.vars tt in
         if (not (Var.Set.is_empty vars)) && not is_value then
           let weak_vars, all_weak_vars, _ =
             Var.Set.fold
               (fun (acc, accw, i) v ->
                  let wv = Var.mk ~kind:`weak ("_weak" ^ string_of_int i) in
                  ((v, Types.var wv) :: acc, Var.Map.add wv None accw, i + 1))
               ([], env.weak_vars, Var.Map.length env.weak_vars)
               vars
           in
           let () = env.weak_vars <- all_weak_vars in
           let subst = Types.Subst.from_list weak_vars in
           (id, Types.Subst.apply_full subst tt)
         else
           (* raise_loc_generic p.loc
              (Format.asprintf
                 "The type of identifier %a is %a.@\n\
                  It contains polymorphic variables that cannot be generalized."
                 Ident.print id Types.Print.print tt);*)
           (id, tt))
      typs
  in
  let env = enter_values typs env in
  ( {
    env with
    ids_loc =
      List.fold_left
        (fun acc (id, _) -> Env.add id p.loc acc)
        env.ids_loc typs;
    mono_vars = Var.Set.empty;
    poly_vars = [];
  },
    decl,
    typs )

let type_let_funs env funs =
  clear_unused_branches ();
  let rec id = function
    | Ast.LocatedExpr (_, e) -> id e
    | Ast.Abstraction a -> fun_name env a
    | _ -> assert false
  in
  let ids =
    List.fold_left
      (fun accu f ->
         match id f with
         | Some x -> x :: accu
         | None -> accu)
      [] funs
  in
  let env' = enter_values_dummy ids env in
  let funs = List.map (expr env') funs in
  let typs = type_rec_funs env funs in
  report_unused_branches ();
  let env = update_weak_variables env in
  let env = enter_values typs env in
  let env = { env with mono_vars = Var.Set.empty; poly_vars = [] } in
  (env, funs, typs)

(*
let find_cu x env =
  match find_cu noloc x env with
    | ECDuce cu -> cu
    | _ -> raise (Error ("Cannot find external unit " ^ (U.to_string x)))
*)

let check_weak_variables env =
  Env.iter
    (fun id -> function
       | Val t ->
         let vrs = Types.Subst.vars t in
         Var.Set.iter
           (fun v ->
              match Var.kind v with
              | `generated
              | `user ->
                ()
              | `weak ->
                let loc = Env.find id env.ids_loc in
                Cduce_error.(raise_err_loc ~loc Typer_WeakVar (id, t)))
           vrs
       | _ -> ())
    env.ids
