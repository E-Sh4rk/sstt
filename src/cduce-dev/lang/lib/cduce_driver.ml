open Cduce_loc
open Cduce_types
open Ident
module U = Encodings.Utf8

(* let () = Stats.gettimeofday := Unix.gettimeofday *)

let extra_specs = ref []

(* if set to false toplevel exception aren't cought.
 * Useful for debugging with OCAMLRUNPARAM="b" *)
let catch_exceptions = true

(* retuns a filename without the suffix suff if any *)
let _prefix filename suff =
  if Filename.check_suffix filename suff then
    try Filename.chop_extension filename with
    | Invalid_argument _filename -> failwith "Not a point in the suffix?"
  else filename

let toplevel = ref false
let verbose = ref false
let silent = ref false
let typing_env = ref Builtin.env
let compile_env = ref Compile.empty_toplevel
let get_global_value _cenv v = Eval.eval_var (Compile.find v !compile_env)
let _get_global_type v = Typer.find_value v !typing_env

let rec _is_abstraction = function
  | Ast.Abstraction _ -> true
  | Ast.LocatedExpr (_, e) -> _is_abstraction e
  | _ -> false

let print_norm ppf d = Types.Print.print ppf (*Types.normalize*) d
let print_sample ppf s = Types.Print.print ppf s
let print_protect ppf s = Format.fprintf ppf "%s" s
let print_value ppf v = Value.print ppf v

let dump_value ppf x t v =
  Format.fprintf ppf "@[val %a : @[%a = %a@]@]@." Ident.print x print_norm t
    print_value v

let dump_env ppf tenv cenv =
  Format.fprintf ppf "Types:%a@." Typer.dump_types tenv;
  Format.fprintf ppf "Namespace prefixes:@\n%a" Typer.dump_ns tenv;
  Format.fprintf ppf "Namespace prefixes used for pretty-printing:@.%t"
    Ns.InternalPrinter.dump;
  Format.fprintf ppf "Values:@.";
  Typer.iter_values tenv (fun x t ->
      dump_value ppf x t (get_global_value cenv x))

let directive_help ppf =
  Format.fprintf ppf
    "Toplevel directives:\n\
    \  #quit;;                 quit the interpreter\n\
    \  #env;;                  dump current environment\n\
    \  #reinit_ns;;            reinitialize namespace processing\n\
    \  #help;;                 shows this help message\n\
    \  #print_type <type>;;\n\
    \  #silent;;               turn off outputs from the toplevel\n\
    \  #verbose;;              turn on outputs from the toplevel\n\
    \  #builtins;;             shows embedded OCaml values\n"

let eval_quiet tenv cenv e =
  let _, e, _ = Typer.type_expr tenv e in
  Compile.compile_eval_expr cenv e

let debug ppf tenv _cenv = function
  | `Subtype (t1, t2) ->
    Format.fprintf ppf "[DEBUG:subtype]@.";
    let t1 = Types.descr (Typer.typ tenv t1) in
    let t1_vars = Types.Subst.vars t1 in
    let t1_vmap =
      Var.Set.fold (fun acc v -> (U.mk (Var.name v), v) :: acc) [] t1_vars
    in
    let t2 = Types.descr (Typer.var_typ t1_vmap tenv t2) in
    let s = Types.subtype t1 t2 in
    Format.fprintf ppf "%a %a %a : %b@." print_norm t1 print_protect "<="
      print_norm t2 s
  | `Sample t -> (
      let open Types in
      Format.fprintf ppf "[DEBUG:sample]@.";
      try
        let t = Types.descr (Typer.typ tenv t) in
        Format.fprintf ppf "%a@." print_sample (Sample.get t);
        Format.fprintf ppf "witness: %a@." Types.print_witness t
      with
      | Not_found -> Format.fprintf ppf "Empty type : no sample !@.")
  | `Filter (t, p) ->
    let t = Typer.typ tenv t
    and p = Typer.pat tenv p in
    Format.fprintf ppf "[DEBUG:filter t=%a p=%a]@." Types.Print.print
      (Types.descr t) Patterns.Print.print (Patterns.descr p);
    let f = Patterns.filter (Types.descr t) p in
    IdMap.iteri
      (fun x t ->
         Format.fprintf ppf " %a:%a@." Ident.print x print_norm (Types.descr t))
      f
  | `Accept p ->
    Format.fprintf ppf "[DEBUG:accept]@.";
    let p = Typer.pat tenv p in
    let t = Patterns.accept p in
    Format.fprintf ppf " %a@." Types.Print.print (Types.descr t)
  | `Compile (t, pl) ->
    Format.fprintf ppf "[DEBUG:compile]@.";
    let no = ref (-1) in
    let t = Types.descr (Typer.typ tenv t)
    and pl =
      List.map
        (fun p ->
           incr no;
           (Typer.pat tenv p, !no))
        pl
    in

    let state, rhs = Patterns.Compile.make_branches t pl in
    Array.iteri
      (fun i r ->
         Format.fprintf ppf "Return code %i:" i;
         match r with
         | Auto_pat.Fail -> Format.fprintf ppf "Fail@."
         | Auto_pat.Match (_, n) -> Format.fprintf ppf "Pat(%i)@." n)
      rhs;
    Format.fprintf ppf "@.Dispatcher:@.%a@." Print_auto.print_state state
  | `Single t -> (
      Format.fprintf ppf "[DEBUG:single]@.";
      let t = Typer.typ tenv t in
      try
        let c = Types.Sample.single (Types.descr t) in
        Format.fprintf ppf "Constant:%a@." Types.Print.print_const c
      with
      | Exit -> Format.fprintf ppf "Non constant@."
      | Not_found -> Format.fprintf ppf "Empty@.")
  | `Tallying (var_order, delta, tlist) ->
    Format.fprintf ppf "[DEBUG:tallying]@.";
    let delta, vlist =
      List.fold_left
        (fun (acc_d, acc_v) u ->
           let v = Var.mk (U.get_str u) in
           (Var.Set.add v acc_d, (u, v) :: acc_v))
        (Var.Set.empty, []) delta
    in
    let var_order, vlist =
      List.fold_left
        (fun (acc_d, acc_v) u ->
           let v, acc_v =
             try (List.assoc u acc_v, acc_v) with
             | Not_found ->
               let v = Var.mk (U.get_str u) in
               (v, (u, v) :: acc_v)
           in
           (v :: acc_d, acc_v))
        ([], vlist) var_order
    in
    let var_order = List.rev var_order in
    let tprobs, _ =
      List.fold_left
        (fun (acc_t, acc_v) (p1, p2) ->
           let t1 = Types.descr @@ Typer.var_typ acc_v tenv p1 in
           let nv1 = Types.Subst.vars t1 in
           let acc_v =
             Var.Set.fold
               (fun acc_v v ->
                  let u = U.mk (Var.name v) in
                  if List.mem_assoc u acc_v then acc_v else (u, v) :: acc_v)
               acc_v nv1
           in
           let t2 = Types.descr @@ Typer.var_typ acc_v tenv p2 in
           let nv2 = Types.Subst.vars t2 in
           let acc_v =
             Var.Set.fold
               (fun acc_v v ->
                  let u = U.mk (Var.name v) in
                  if List.mem_assoc u acc_v then acc_v else (u, v) :: acc_v)
               acc_v nv2
           in
           ((t1, t2) :: acc_t, acc_v))
        ([], vlist) tlist
    in
    let subst = Types.Tallying.tallying ~var_order delta tprobs in
    Format.fprintf ppf "Result:%a@." Types.Subst.print_list subst;
    Format.fprintf ppf "Cleaned result:%a@." Types.Subst.print_list
      (List.map
         (fun s -> Var.Map.map (fun t -> Types.Subst.clean_type delta t) s)
         subst)

let flush_ppf ppf = Format.fprintf ppf "@."

let directive ppf tenv cenv = function
  | `Debug d -> debug ppf tenv cenv d
  | `Quit -> if !toplevel then raise End_of_file
  | `Env -> dump_env ppf tenv cenv
  | `Print_type t ->
    let t = Typer.typ tenv t in
    Format.fprintf ppf "%a@." Types.Print.print_noname (Types.descr t)
  | `Reinit_ns -> Typer.set_ns_table_for_printer tenv
  | `Help -> directive_help ppf
  | `Dump pexpr ->
    Value.dump_xml ppf (eval_quiet tenv cenv pexpr);
    flush_ppf ppf
  | `Silent -> silent := true
  | `Verbose -> silent := false
  | `Builtins ->
    let b = Librarian.get_builtins () in
    Format.fprintf ppf "Embedded OCaml values: ";
    List.iter
      (fun s ->
         let t = Externals.typ s [] in
         Format.fprintf ppf "%s : %a@\n" s Types.Print.print t)
      b;
    Format.fprintf ppf "@."

let print_id_opt ppf = function
  | None -> Format.fprintf ppf "-"
  | Some id -> Format.fprintf ppf "val %a" Ident.print id

let print_value_opt ppf = function
  | None -> ()
  | Some v -> Format.fprintf ppf " = %a" print_value v

let show ppf id t v =
  if !silent then ()
  else
    Format.fprintf ppf "@[%a : @[%a%a@]@]@." print_id_opt id print_norm t
      print_value_opt v

let ev_top ~run ~show ?directive phs =
  let tenv, cenv, _ =
    Compile.comp_unit ~run ~show ?directive !typing_env !compile_env phs
  in
  typing_env := tenv;
  compile_env := cenv

let phrases ppf phs =
  ev_top ~run:true ~show:(show ppf) ~directive:(directive ppf) phs

let catch_exn ppf_err exn =
  if not catch_exceptions then begin
    if Printexc.backtrace_status () then Printexc.print_backtrace stderr;
    raise exn
  end;
  match exn with
  | (End_of_file | Failure _ | Not_found | Invalid_argument _ | Sys.Break) as e
    ->
    if Printexc.backtrace_status () then Printexc.print_backtrace stderr;
    raise e
  | Cduce_error.Error (loc, err) -> Cduce_error.print_error_loc ppf_err loc err
  | exn ->
    Cduce_error.print_exn ppf_err exn;
    Format.fprintf ppf_err "@."

let parse rule input =
  try rule input with
  | e ->
    Parse.sync ();
    raise e

let run rule ppf ppf_err input =
  try
    phrases ppf (parse rule input);
    true
  with
  | Cduce_error.Error (_, (Driver_Escape, exn)) -> raise exn
  | exn ->
    catch_exn ppf_err exn;
    false

let topinput ?(source=Cduce_loc.toplevel_source) = run (Parse.top_phrases ~source)
let compile src out_dir =
  try
    if not (Filename.check_suffix src ".cd") then
      Cduce_error.(raise_err Driver_InvalidInputFilename src);
    let cu = Filename.chop_suffix (Filename.basename src) ".cd" in
    let out_dir =
      match out_dir with
      | None -> Filename.dirname src
      | Some x -> x
    in
    let out = Filename.concat out_dir (cu ^ ".cdo") in
    let name = U.mk_latin1 cu in
    Librarian.compile_save !verbose name src out;
    exit 0
  with
  | exn ->
    catch_exn Format.err_formatter exn;
    exit 1

let compile_run src =
  try
    let name =
      if src = "" then "<stdin>"
      else if not (Filename.check_suffix src ".cd") then
        Cduce_error.(raise_err Driver_InvalidInputFilename src)
      else Filename.chop_suffix (Filename.basename src) ".cd"
    in
    let name = U.mk_latin1 name in
    Librarian.compile_run !verbose name src
  with
  | exn ->
    catch_exn Format.err_formatter exn;
    exit 1

let run obj =
  Cduce_loc.add_to_obj_path (Filename.dirname obj);
  let obj = Filename.basename obj in
  try
    if not (Filename.check_suffix obj ".cdo") then
      Cduce_error.(raise_err Driver_InvalidObjectFilename obj);
    let name = Filename.chop_suffix (Filename.basename obj) ".cdo" in
    let name = U.mk_latin1 name in
    Librarian.load_run name
  with
  | exn ->
    catch_exn Format.err_formatter exn;
    exit 1

let dump_env ppf = dump_env ppf !typing_env !compile_env

let eval s =
  let st = String.to_seq s in
  let phs = parse Parse.prog st in
  let vals = ref [] in
  let show id _t v =
    match (id, v) with
    | Some id, Some v -> vals := (Some (AtomSet.V.mk id), v) :: !vals
    | None, Some v -> vals := (None, v) :: !vals
    | _ -> assert false
  in
  ev_top ~run:true ~show phs;
  List.rev !vals

let eval s =
  try eval s with
  | exn ->
    let b = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer b in
    Cduce_error.print_exn ppf exn;
    Format.fprintf ppf "@.";
    Value.failwith' (Buffer.contents b)

let argv args = Value.sequence (List.rev_map Value.string_latin1 args)
let set_argv args = Builtin.argv := argv args

let () =
  Operators.register_fun "eval_expr" Builtin_defs.string_latin1 Types.any
    (fun v ->
       match eval (Value.cduce2ocaml_string v) with
       | [ (None, v) ] -> v
       | _ -> Value.failwith' "eval: the string must evaluate to a single value")
