module Loc = Cduce_core.Cduce_loc
module Ocaml = Ocaml_common

(* Unfolding of OCaml types *)

exception PolyAbstract of string

let error s = Cduce_core.Cduce_error.raise_err Ocamliface s
let unsupported s =
  Cduce_core.(Cduce_error.raise_err Ocamliface_unsupported s)

let env_initial, update_env =
  let ocaml_env = ref None in
  let env_initial () =
    match !ocaml_env with
    | Some env -> env
    | None ->
      let env =
        try
          let env = Mlcompat.Mltypes.type_mod_initial_env
              ~loc:(Ocaml.Location.in_file "Ocaml/Cduce interface")
              ~initially_opened_module:None ~open_implicit_modules:[];
          in 
          Mlcompat.Mltypes.load_path ();
          env
        with
        | Ocaml.Env.Error err ->
          error @@ Format.asprintf "%a" Ocaml.Env.report_error err
        | e -> error "Cannot intialise OCaml environment"
      in
      ocaml_env := Some env;
      env
  in
  let update_env sg =
    let env = Ocaml.Env.add_signature sg (env_initial ()) in
    ocaml_env := Some env
  in
  (env_initial, update_env)

type t = {
  uid : int;
  mutable recurs : int;
  mutable def : def;
}

and def =
  | Link of t
  | Arrow of string * t * t
  | Tuple of t list
  | PVariant of (string * t option) list (* Polymorphic variant *)
  | Variant of string * (Ocaml.Ident.t * t list * t option) list * bool
  | Record of string * (Ocaml.Ident.t * t) list * bool
  | Builtin of string * t list
  | Abstract of string
  | Var of int

let for_all2 f l1 l2 =
  try List.for_all2 f l1 l2 with
  | _ -> false

let for_opt f o1 o2 =
  match (o1, o2) with
  | Some v1, Some v2 -> f v1 v2
  | _ -> false

module IntMap = Map.Make (struct
    type t = int

    let compare : t -> t -> int = compare
  end)

module IntSet = Set.Make (struct
    type t = int

    let compare : t -> t -> int = compare
  end)

module StringSet = Set.Make (struct
    type t = string

    let compare : t -> t -> int = compare
  end)

let rec print_sep f sep ppf = function
  | [] -> ()
  | [ x ] -> f ppf x
  | x :: tl ->
    Format.fprintf ppf "%a%s" f x sep;
    print_sep f sep ppf tl

let printed = ref IntMap.empty

let rec print_slot ppf slot =
  if slot.recurs > 0 then
    if IntMap.mem slot.uid !printed then Format.fprintf ppf "X%i" slot.uid
    else (
      printed := IntMap.add slot.uid () !printed;
      Format.fprintf ppf "X%i:=%a" slot.uid print_def slot.def)
  else print_def ppf slot.def

and print_def ppf = function
  | Link t -> Format.fprintf ppf "Link(%a)" print_slot t
  | Arrow (l, t, s) ->
    Format.fprintf ppf "%s:%a -> %a" l print_slot t print_slot s
  | Tuple tl -> Format.fprintf ppf "(%a)" (print_sep print_slot ",") tl
  | PVariant l -> Format.fprintf ppf "[%a]" (print_sep print_palt " | ") l
  | Variant (p, l, _) ->
    Format.fprintf ppf "[%s:%a]" p (print_sep print_alt " | ") l
  | Record (p, l, _) ->
    Format.fprintf ppf "{%s:%a}" p (print_sep print_field " ; ") l
  | Builtin (p, tl) ->
    Format.fprintf ppf "%s(%a)" p (print_sep print_slot ",") tl
  | Abstract s -> Format.fprintf ppf "%s" s
  | Var i -> Format.fprintf ppf "'a%i" i

and print_palt ppf = function
  | lab, None -> Format.fprintf ppf "`%s" lab
  | lab, Some t -> Format.fprintf ppf "`%s of %a" lab print_slot t

and print_alt ppf = function
  | lab, [], _ -> Format.fprintf ppf "%s" (Ocaml.Ident.name lab)
  | lab, l, _ ->
    Format.fprintf ppf "%s of [%a]" (Ocaml.Ident.name lab)
      (print_sep print_slot ",") l

and print_field ppf (lab, t) =
  Format.fprintf ppf "%s:%a" (Ocaml.Ident.name lab) print_slot t

let print ppf t =
  printed := IntMap.empty;
  print_slot ppf t

let equal_type t1 t2 =
  let visited = Hashtbl.create 17 in
  let rec loop t1 t2 =
    if t1 == t2 || t1.uid = t2.uid then true
    else if Hashtbl.mem visited (t1.uid, t2.uid) then true
    else
      let () = Hashtbl.add visited (t1.uid, t2.uid) () in
      loop_def t1 t2
  and loop_def t1 t2 =
    match (t1.def, t2.def) with
    | Link tt1, Link tt2 -> loop tt1 tt2
    | Link tt1, _ -> loop tt1 t2
    | _, Link tt2 -> loop t1 tt2
    | Arrow (s1, t1, u1), Arrow (s2, t2, u2) ->
      s1 = s2 && loop t1 t2 && loop u1 u2
    | Tuple tl1, Tuple tl2 -> for_all2 loop tl1 tl2
    | PVariant l1, PVariant l2 ->
      for_all2 (fun (s1, o1) (s2, o2) -> s1 = s2 && for_opt loop o1 o2) l1 l2
    | Variant (s1, l1, b1), Variant (s2, l2, b2) ->
      s1 = s2 && b1 = b2
      && for_all2
        (fun (i1, ll1, o1) (i2, ll2, o2) ->
           Ocaml.Ident.same i1 i2 && for_opt loop o1 o2
           && for_all2 loop ll1 ll2)
        l1 l2
    | Record (s1, f1, b1), Record (s2, f2, b2) ->
      s1 = s2 && b1 = b2
      && for_all2
        (fun (i1, t1) (i2, t2) -> Ocaml.Ident.same i1 i2 && loop t1 t2)
        f1 f2
    | Builtin (s1, l1), Builtin (s2, l2) -> s1 = s2 && for_all2 loop l1 l2
    | Abstract s1, Abstract s2 -> s1 = s2
    | Var i1, Var i2 -> i1 = i2
    | _, _ -> false
  in
  loop t1 t2

module HashType = Hashtbl.Make (struct
    type key = t
    type t = key

    let rec hash t =
      match t.def with
      | Link tt -> hash tt
      | Arrow _ -> Hashtbl.hash "ARROW"
      | Tuple _ -> Hashtbl.hash "TUPLE"
      | Variant _ -> Hashtbl.hash "VARIANT"
      | PVariant _ -> Hashtbl.hash "PVARIANT"
      | Record _ -> Hashtbl.hash "RECORD"
      | Builtin _ -> Hashtbl.hash "BUILTIN"
      | Abstract _ -> Hashtbl.hash "ABSTRACT"
      | Var _ -> Hashtbl.hash "VAR"

    let equal t1 t2 = equal_type t1 t2
  end)

let counter = ref 0

let new_slot () =
  incr counter;
  { uid = !counter; recurs = 0; def = Abstract "DUMMY" }

let reg_uid t =
  let saved = ref [] in
  let rec aux t =
    if t.recurs < 0 then ()
    else begin
      if t.uid > !counter then counter := t.uid;
      saved := (t, t.recurs) :: !saved;
      t.recurs <- -1;
      match t.def with
      | Link t -> aux t
      | Arrow (_, t1, t2) ->
        aux t1;
        aux t2
      | Tuple tl -> List.iter aux tl
      | PVariant pl ->
        List.iter
          (function
            | _, Some t -> aux t
            | _ -> ())
          pl
      | Variant (_, pl, _) ->
        List.iter
          (function
            | _, tl, Some o -> List.iter aux (tl @ [ o ])
            | _, tl, None -> List.iter aux tl)
          pl
      | Record (_, tl, _) -> List.iter (fun (_, t) -> aux t) tl
      | Builtin (_, tl) -> List.iter aux tl
      | _ -> ()
    end
  in
  aux t;
  List.iter (fun (t, recurs) -> t.recurs <- recurs) !saved

let builtins =
  List.fold_left
    (fun m x -> StringSet.add x m)
    StringSet.empty
    [
      "bool";
      "int";
      "char";
      "string";
      "list";
      "Stdlib.ref";
      "Stdlib.Seq.t";
      "unit";
      "array";
      "Z.t";
      "option";
      "Cduce_core.Value.t";
      "Cduce_types.Encodings.Utf8.t";
      "Cduce_types.Atoms.V.t";
    ]

let vars = ref []

let get_var id =
  try List.assq id !vars with
  | Not_found ->
    let i = List.length !vars in
    vars := (id, i) :: !vars;
    i

exception Skip

let constr_table = Hashtbl.create 1024

type env = {
  constrs : StringSet.t;
  seen : IntSet.t;
  vars : t IntMap.t;
}

(* Take the file p, if it is from the builtins, open it; else *)
let rec unfold_constr env p args =
  let args = List.map (unfold env) args in
  let pn = Ocaml.Path.name p in
  if StringSet.mem pn builtins then (
    let slot = new_slot () in
    slot.def <- Builtin (pn, args);
    slot)
  else
    let args_id = List.map (fun t -> t.uid) args in
    let k = (pn, args_id) in
    try Hashtbl.find constr_table k with
    | Not_found ->
      if StringSet.mem pn env.constrs then raise Skip
      (*failwith ("Polymorphic recursion forbidden : " ^ pn)*);
      let slot = new_slot () in
      slot.recurs <- 1;
      Hashtbl.add constr_table k slot;

      let decl =
        try Ocaml.Env.find_type p (env_initial ()) with
        | Not_found -> error ("Cannot resolve path " ^ pn)
      in

      let env =
        {
          env with
          constrs = StringSet.add pn env.constrs;
          vars =
            List.fold_left2
              (fun vars a t ->
                 IntMap.add (Mlcompat.Mltypes.get_type_expr_id a) t vars)
              env.vars decl.type_params args;
        }
      in

      let prefix =
        match p with
        | Ocaml.Path.Pident _ -> ""
        | Ocaml.Path.Pdot _ ->
          let p = Mlcompat.Mltypes.get_path_from_pdot p in
          Ocaml.Path.name p ^ "."
        | _ -> assert false
      in

      slot.def <-
        (match (decl.type_kind, decl.type_manifest) with
         | (Type_variant _ as t), _ ->
           let cstrs = Mlcompat.Mltypes.get_type_variant_cstr t in
           let cstrs =
             (* TODO: Check this solution *)
             let open Ocaml.Types in
             List.map
               (function
                 | { cd_id; cd_args; cd_res; _ } ->
                   let lst =
                     match cd_args with
                     | Cstr_tuple l -> l
                     | Cstr_record _ ->
                       unsupported "inline records"
                   in
                   let tres =
                     match cd_res with
                     | Some o -> Some (unfold env o)
                     | None -> None
                   in
                   (cd_id, List.map (unfold env) lst, tres))
               cstrs
           in
           Variant (prefix, cstrs, true)
         | Type_record (f, _), _ ->
           let open Ocaml.Types in
           let f =
             List.map
               (fun { ld_id; ld_type; _ } -> (ld_id, unfold env ld_type))
               f
           in
           Record (prefix, f, true)
         | x, Some t when Mlcompat.Mltypes.is_type_abstract x -> Link (unfold env t)
         | x, None when Mlcompat.Mltypes.is_type_abstract x -> (
             match args with
             | [] -> Abstract pn
             | _l -> raise (PolyAbstract pn))
         | Type_open, _ -> raise Skip
         | _ -> assert false);
      slot

and unfold env ty =
  let tid = Mlcompat.Mltypes.get_type_expr_id ty in
  if IntSet.mem tid env.seen then error "Unguarded recursion";
  let env = { env with seen = IntSet.add tid env.seen } in
  let slot = new_slot () in
  slot.def <-
    (match Mlcompat.Mltypes.get_type_expr_desc ty with
     | Tarrow (Optional _, _, t2, _) -> (unfold env t2).def
     | Tarrow (l, t1, t2, _) ->
       let t1 = unfold env t1 in
       let t2 = unfold env t2 in

       Arrow
         ( (match l with
               | Labelled s -> "~" ^ s
               | _ -> ""),
           t1,
           t2 )
     | Ttuple tyl -> Tuple (List.map (unfold env) tyl)
     | Tvariant rd ->
       let fields =
         List.fold_left
           (fun accu (lab, f) ->
              match f with
              | Ocaml.Types.Rpresent (Some t) ->
                (lab, Some (unfold env t)) :: accu
              | Rpresent None -> (lab, None) :: accu
              | Rabsent ->
                Printf.eprintf "Warning: Rabsent not supported";
                accu
              | Reither _ -> (
                  let b, l = Mlcompat.Mltypes.extract_Reither f in
                  match (b, l) with
                  | true, [ t ] -> (lab, Some (unfold env t)) :: accu
                  | true, [] -> (lab, None) :: accu
                  | _ ->
                    Printf.eprintf "Warning: Reither not supported";
                    accu))
           []
           (Mlcompat.Mltypes.get_row_fields rd)
       in
       PVariant fields
     | Tvar _s -> (
         try
           Link (IntMap.find (Mlcompat.Mltypes.get_type_expr_id ty) env.vars)
         with
         | Not_found -> Var (get_var (Mlcompat.Mltypes.get_type_expr_id ty)))
     | Tconstr (p, args, _) -> Link (unfold_constr env p args)
     | _ -> raise Skip);
  slot

let unfold ty =
  vars := [];
  Hashtbl.clear constr_table;
  (* Get rid of that (careful with exceptions) *)
  let t =
    unfold
      { seen = IntSet.empty; constrs = StringSet.empty; vars = IntMap.empty }
      ty
  in
  let n = List.length !vars in
  vars := [];
  (t, n)

(* Reading .cmi *)


let has_cmi name =
  Mlcompat.Mltypes.load_path ();
  try
    ignore (Mlcompat.Mltypes.find_in_path (name ^ ".cmi"));
    true
  with
  | Not_found -> false

let find_value v =
  Mlcompat.Mltypes.load_path ();
  let li = Mlcompat.longident_parse v in
  let _, vd = Mlcompat.Mltypes.lookup_value li (env_initial ()) in
  unfold vd.Ocaml.Types.val_type

let values_of_sig name sg =
  List.fold_left
    (fun accu v ->
       match v with
       | Ocaml.Types.Sig_value _ as s
         when not (Mlcompat.Mltypes.is_sig_value_deprecated s) -> (
           let id, _ = Mlcompat.Mltypes.get_id_t_from_sig_value v in
           let id = Ocaml.Ident.name id in
           match id.[0] with
           | 'a' .. 'z'
           | '_' -> (
               let n = name ^ "." ^ id in
               try (n, fst (find_value n)) :: accu with
               | Skip
               | PolyAbstract _ ->
                 accu)
           | _ -> accu
           (* operator *))
       | _ -> accu)
    [] sg

let find_value n =
  try find_value n with
  | PolyAbstract s -> unsupported @@ "polymorphic abstract type " ^ s

let load_module name =
  Mlcompat.Mltypes.load_path ();
  let li = Mlcompat.longident_parse name in
  let path = Mlcompat.Mltypes.lookup_module li (env_initial ()) in
  let rec loop p =
    match (Ocaml.Env.find_module p (env_initial ())).md_type with
    | Ocaml.Types.Mty_signature sg -> values_of_sig name sg
    | Ocaml.Types.Mty_alias _ as alias ->
      loop (Mlcompat.Mltypes.get_path_from_mty_alias alias)
    | _ ->
      Cduce_core.(Cduce_error.raise_err Ocamliface
                    (Printf.sprintf "Module %s is not a structure" name))
  in
  loop path

let load_module name =
  try load_module name with
  | Ocaml.Env.Error e ->
    Ocaml.Env.report_error Format.str_formatter e;
    let s = Format.flush_str_formatter () in
    let s =
      Printf.sprintf "Error while reading OCaml interface %s: %s" name s
    in
    Cduce_core.Cduce_error.raise_err Ocamliface s

let build_type_decl id t rs =
  match Mlcompat.Mltypes.tree_of_type_declaration id t rs with
  | Outcometree.Osig_type (otdecl, ors) -> Ast_helper.Str.type_
  | _ -> assert false

let read_cmi name =
  Mlcompat.Mltypes.load_path ();
  let filename = Mlcompat.Mltypes.find_in_path (name ^ ".cmi") in
  let sg = Mlcompat.Mltypes.env_read_signature name filename in
  update_env sg;
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let values = ref [] in
  List.iter
    (function
      | Ocaml.Types.Sig_value _ as s
        when Mlcompat.Mltypes.is_sig_value_val_reg s -> (
          if not (Mlcompat.Mltypes.is_sig_value_deprecated s) then
            let id, t = Mlcompat.Mltypes.get_id_t_from_sig_value s in
            try
              let unf, n = unfold t in
              if n != 0 then unsupported "polymorphic value";
              values := (Ocaml.Ident.name id, t, unf) :: !values
            with
            | Skip -> ())
      | Sig_type _ as s ->
        let id, t, rs = Mlcompat.Mltypes.get_sig_type s in
        Format.fprintf ppf "%a@." (Mlcompat.Mltypes.format_doc_compat !Ocaml.Oprint.out_sig_item)
          (Mlcompat.Mltypes.tree_of_type_declaration id t rs)
      | Sig_value _ -> unsupported "external value"
      | Sig_typext _ -> unsupported "extensible type"
      | Sig_module _ -> unsupported "module"
      | Sig_modtype _ -> unsupported "module type"
      | Sig_class _ -> unsupported "class"
      | Sig_class_type _ -> unsupported "class type")
    sg;
  (Buffer.contents buf, !values)

let read_cmi name =
  try read_cmi name with
  | Ocaml.Env.Error e ->
    Ocaml.Env.report_error Format.str_formatter e;
    let s = Format.flush_str_formatter () in
    let s =
      Printf.sprintf "Error while reading OCaml interface %s: %s" name s
    in
    Cduce_core.(Cduce_error.raise_err Ocamliface s)

let print_ocaml = Ocaml.Printtyp.type_expr
