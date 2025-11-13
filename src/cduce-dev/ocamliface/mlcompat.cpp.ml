(*
#ifdef COMPILER_LIBS_true
*)
open Cduce_core

(*
#if OCAML_VERSION < 411
*)
let longident_parse = Ocaml_common.Longident.parse

(*
#else
*)
let longident_parse s =
  let open Ocaml_common in
  let rec loop first s =
    match String.rindex_opt s '.' with
    | None -> Longident.Lident s
    | Some i -> (
        try Parse.longident (Lexing.from_string s) with
        | _ -> (
            try Parse.val_ident (Lexing.from_string s) with
            | _ -> (
                try Parse.constr_ident (Lexing.from_string s) with
                | e ->
                  (* last resort *)
                  if first then
                    let p, s =
                      ( String.sub s 0 i,
                        String.sub s (i + 1) (String.length s - i - 1) )
                    in
                    let s = p ^ ".(" ^ s ^ ")" in
                    loop false s
                  else raise e)))
  in
  loop true s
(*
#endif
*)

module Mlstub = struct
(*
#if OCAML_VERSION < 502
*)
  let exp_fun_ = Ocaml_common.Ast_helper.Exp.fun_
(*
#else
*)
  let exp_fun_ ?loc
      ?attrs
      lbl
      expopt
      pat
      body
    =
    let open Ocaml_common in
    Ast_helper.Exp.function_ ?loc ?attrs
      [ { Parsetree.pparam_loc = Location.none;
          Parsetree.pparam_desc = Pparam_val (lbl, expopt, pat) } ]
      None
      (Parsetree.Pfunction_body body)
(*
#endif
*)

(*
#if OCAML_VERSION < 410
*)
  let noloc id = id

(*
#else
*)
  let noloc id = Some id

(*
#endif
*)
  let str_open l =
    let open Ocaml_common.Ast_helper in
    Str.open_ (Opn.mk (Mod.ident l))
(*
#if OCAML_VERSION < 413
*)
  let pat_construct lid pat =
    let open Ocaml_common.Ast_helper in
    Pat.construct lid pat

(*
#else
*)
  let pat_construct lid pat =
    let open Ocaml_common.Ast_helper in
    Pat.construct lid
      (match pat with
       | None -> None
       | Some p -> Some ([], p))
(*
#endif
*)
end

module Mltypes = struct
  open Ocaml_common

  (*
#if OCAML_VERSION < 502
*)

(*
#if OCAML_VERSION < 500
*)
let load_path_init cb lst = Load_path.init lst

(*
#else
*)
let load_path_init cb lst = Load_path.init ~auto_include:cb lst

(*
#endif
*)
  let get_paths = Load_path.get_paths
  let add_dir s = Load_path.add_dir s
  let find_in_path_uncap = Misc.find_in_path_uncap
  let is_type_abstract t = match t with Types.Type_abstract -> true | _ -> false
  let env_read_signature modname filename = Env.read_signature modname filename

(*
#else
*)
  let get_paths = Load_path.get_path_list
  let add_dir s = Load_path.add_dir ~hidden:false s
  let find_in_path_uncap = Misc.find_in_path_normalized
  let load_path_init cb lst =
    Load_path.init ~auto_include:cb ~visible:lst ~hidden:[]
  let is_type_abstract t = match t with Types.Type_abstract _ -> true | _ -> false
  let env_read_signature _ filename = Env.read_signature (Unit_info.Artifact.from_filename filename)

  (*
#endif
*)
  let get_path_from_mty_alias = function
    | Types.Mty_alias p -> p
    | _ -> assert false

  let load_path () =
    let add_dir s =
      if not (List.mem s (get_paths ())) then add_dir s
    in
    List.iter add_dir (List.rev (Cduce_loc.get_obj_path()));
    add_dir Config.standard_library;;

  let find_in_path file = find_in_path_uncap (get_paths ()) file

  let get_path_from_pdot e =
    match e with
    | Path.Pdot (p, _) -> p
    | _ -> assert false

  let is_sig_value_val_reg e =
    match e with
    | Types.Sig_value (_, { val_type = _; val_kind = Val_reg }, _) -> true
    | _ -> false

  let get_id_t_from_sig_value e =
    match e with
    | Types.Sig_value (id, { val_type = t }, _) -> (id, t)
    | _ -> assert false

  let get_sig_type e =
    match e with
    | Types.Sig_type (id, t, rs, _) -> (id, t, rs)
    | _ -> assert false

  let is_sig_value_deprecated e =
    match e with
    | Types.Sig_value (_, { val_attributes; _ }, _) ->
      List.exists
        (fun att ->
           let txt = Parsetree.(att.attr_name.txt) in
           txt = "ocaml.deprecated" || txt = "deprecated")
        val_attributes
    | _ -> assert false

(*
#if OCAML_VERSION < 410
*)
  let lookup_value li env = Env.lookup_value li env
  let lookup_module li env = Env.lookup_module ~load:true li env

(*
#else
*)
  let lookup_value li env = Env.find_value_by_name li env

  let lookup_module li env =
    let loc =
      Warnings.
        {
          loc_start = Lexing.dummy_pos;
          loc_end = Lexing.dummy_pos;
          loc_ghost = true;
        }
    in
    Env.lookup_module_path ~use:true ~load:true ~loc li env

(*
#endif
*)
  (*
     #if OCAML_VERSION < 413
  *)
  let get_type_variant_cstr = function
    | Types.Type_variant cstr -> cstr
    | _ -> assert false

(*
#else
*)
  let get_type_variant_cstr = function
    | Types.Type_variant (cstr, _) -> cstr
    | _ -> assert false
(*
#endif
*)

(*
#if OCAML_VERSION < 414
*)
  let get_type_expr_id t = t.Types.id
  let get_type_expr_desc t = t.Types.desc
  let get_row_fields r = r.Types.row_fields

  let extract_Reither = function
    | Types.Reither (b, l, _, _) -> (b, l)
    | _ -> assert false

(*
#else
*)
  let get_type_expr_id t = Types.get_id t
  let get_type_expr_desc t = Types.get_desc t

  let get_row_fields r =
    List.map (fun (l, f) -> (l, Types.row_field_repr f)) (Types.row_fields r)

  let extract_Reither = function
    | Types.Reither (b, l, _) -> (b, l)
    | _ -> assert false
(*
#endif
*)
(*
#if OCAML_VERSION < 500
*)
  let type_mod_initial_env ~loc ~initially_opened_module ~open_implicit_modules =
    Typemod.initial_env
      ~loc
      ~safe_string:(Config.safe_string || not !Clflags.unsafe_string)
      ~initially_opened_module
      ~open_implicit_modules

(*
#else
*)

  let type_mod_initial_env ~loc ~initially_opened_module ~open_implicit_modules =
    load_path_init (Load_path.auto_include_otherlibs ignore) [];
    Typemod.initial_env
      ~loc
      ~initially_opened_module
      ~open_implicit_modules

(*
#endif
*)
(*
#if OCAML_VERSION < 503
*)
  let tree_of_type_declaration = Printtyp.tree_of_type_declaration
  let format_doc_compat x = x

(*
#else
*)
  let tree_of_type_declaration  = Out_type.tree_of_type_declaration
  let format_doc_compat x = Format_doc.compat x
(*
#endif
*)
end
(*
#endif
*)
