let std_compare = compare

open Cduce_loc
open Cduce_types
open Ident
module U = Encodings.Utf8

let run_loaded = ref false

type t = {
  name : U.t;
  descr : Compunit.t;
  typing : Typer.t;
  compile : Compile.env;
  code : Lambda.code_item list;
  ext_info : Externals.ext_info option;
  mutable digest : Digest.t option;
  vals : Value.t array;
  (* Exported values *)
  mutable exts : Value.t array;
  mutable depends : (U.t * string) list;
  mutable status : [ `Evaluating | `Unevaluated | `Evaluated ];
}

let digest c =
  match c.digest with
  | None -> assert false
  | Some x -> x

module Tbl = Hashtbl.Make (U)

let tbl = Tbl.create 64

module CTbl = Hashtbl.Make (Compunit)

let ctbl = CTbl.create 64

let mk name descr typing compile code ext_info depends =
  let exts =
    let len = match ext_info with None -> 0 | Some l -> List.length l in
    Array.make len Value.Absent
  in
  let vals =
    Array.make (Compile.global_size compile) Value.Absent
  in
  {
    name;
    descr;
    typing;
    compile;
    code;
    ext_info;
    digest = None;
    vals;
    exts;
    depends;
    status = `Unevaluated;
  }

let magic = "CDUCE:compunit:0000C"

let has_obj n =
  let base = U.to_string n ^ ".cdo" in
  List.exists (fun p -> Sys.file_exists (Filename.concat p base)) (Cduce_loc.get_obj_path())

let find_obj n =
  let base = U.to_string n ^ ".cdo" in
  let p =
    List.find (fun p -> Sys.file_exists (Filename.concat p base)) (Cduce_loc.get_obj_path())
  in
  Filename.concat p base

let check_digest c dig = if digest c <> dig then
    Cduce_error.(raise_err Librarian_InconsistentCrc c.name)

let show ppf id t _v =
  match id with
  | Some id ->
    Format.fprintf ppf "@[val %a : @[%a@]@." Ident.print id Types.Print.print
      t
  | None -> ()

let compile verbose name source =
  let ic =
    if source = Cduce_loc.stdin_source then stdin
    else
      try
        let ic = open_in source in
        Cduce_loc.add_to_obj_path (Filename.dirname source);
        ic
      with
      | Sys_error _ -> Cduce_error.(raise_err Librarian_CannotOpen source)
  in
  let input = Parse.seq_of_in_channel ic in
  let p =
    Fun.protect ~finally:(  if source <> Cduce_loc.stdin_source then fun () -> close_in ic else ignore)
      (fun () -> Parse.prog ~source input)
  in
  let show = if verbose then Some (show Format.std_formatter) else None in
  Compunit.enter ();
  let descr = Compunit.current () in
  let ty_env, c_env, code =
    Compile.comp_unit ?show Builtin.env (Compile.empty descr) p
  in
  Typer.check_weak_variables ty_env;
  Compunit.leave ();
  let ext = Externals.get () in
  let depends = Tbl.fold (fun name c accu -> (name, digest c) :: accu) tbl [] in

  mk name descr ty_env c_env code ext depends

let set_hash c =
  let h = Hashtbl.hash_param 128 256 (c.typing, c.name) in
  let max_rank =
    Tbl.fold (fun _ c accu -> max accu (fst (Compunit.get_hash c.descr))) tbl 0
  in
  Compunit.set_hash c.descr (succ max_rank) h

(* This invalidates all hash tables on types ! *)

let compile_save verbose name src out =

  let c = compile verbose name src in
  set_hash c;
  let pools = Value.extract_all () in

  let oc = open_out_bin out in
  output_string oc magic;

  Marshal.to_channel oc (pools, c) [];
  let digest = Digest.file out in
  Marshal.to_channel oc digest [];
  close_out oc

let from_descr descr : t =
  try CTbl.find ctbl descr with
  | Not_found ->
    let i1, i2 = Compunit.get_hash descr in
    failwith (Printf.sprintf "Can't find cu(%i,%i)" i1 i2)

let register c =
  (* Look for an already loaded unit with the same descriptor *)
  if CTbl.mem ctbl c.descr then failwith "Collision on unit descriptors";
  CTbl.add ctbl c.descr c

let reg_types = ref true

let rec real_load src =
  let ic =
    try open_in_bin src with
    | Sys_error _ -> Cduce_error.(raise_err Librarian_CannotOpen src)
  in
  try
    let s = Bytes.of_string magic in
    really_input ic s 0 (Bytes.length s);
    if s <> Bytes.unsafe_of_string magic then Cduce_error.(raise_err Librarian_InvalidObject src);
    let pools, c = Marshal.from_channel ic in
    let digest = Marshal.from_channel ic in
    c.digest <- Some digest;
    Value.intract_all pools;
    close_in ic;
    c
  with
  | Failure _
  | End_of_file ->
    Cduce_error.(raise_err Librarian_InvalidObject src)

and load name =
  try Tbl.find tbl name with
  | Not_found ->
    let src =
      try find_obj name with
      | Not_found -> Cduce_error.(raise_err Librarian_NoImplementation name)
    in
    let c = real_load src in
    register c;
    (* Register types *)
    if !reg_types then
      Typer.register_types (U.to_string c.name ^ ".") c.typing;
    (* Load dependencies *)
    List.iter (fun (name, dig) -> check_digest (load name) dig) c.depends;
    Tbl.add tbl name c;
    c



let static_externals = Hashtbl.create 17
let virtual_prefixes = Hashtbl.create 17
let reverse_prefixes = Hashtbl.create 17
let has_virtual_prefix n = Hashtbl.mem virtual_prefixes n
let exists_with_prefix n = Hashtbl.mem reverse_prefixes n

let rec run c =
  match c.status with
  | `Unevaluated -> begin
      match c.ext_info with
        Some l when l <> [] ->
        List.iter (fun (vn, n, i, _) ->
            if c.exts.(i) = Value.Absent then
              let v =
                try
                  Hashtbl.find static_externals vn with
                  Not_found ->
                  try
                    Hashtbl.find static_externals n
                  with
                    Not_found ->
                    failwith (Printf.sprintf "The CDuce unit `%s' needs the static external `%s` in the runtime"
                                (U.to_string c.name) vn)
              in c.exts.(i) <- v
          ) l;
        if Array.exists (fun v -> v = Value.Absent) c.exts then
          failwith
            (Printf.sprintf "The CDuce unit `%s' needs externals"
               (U.to_string c.name))
      | _ -> ()
    end;
    (* Run dependencies *)
    List.iter (fun (name, _) -> run (load name)) c.depends;

    c.status <- `Evaluating;
    Eval.eval_unit c.vals c.code;
    c.status <- `Evaluated
  | `Evaluating ->
    failwith ("Librarian.run. Already running:" ^ U.to_string c.name)
  | `Evaluated -> ()

let compile_run verbose name src =
  let c = compile verbose name src in
  register c;
  run c

let load_run name =
  reg_types := false;
  run (load name)

let register_static_external n v =
  let n =
    match String.split_on_char '!' n with
    | [ prefix; rest ] ->
      Hashtbl.replace virtual_prefixes prefix ();
      Hashtbl.replace reverse_prefixes rest prefix;
      prefix ^ "." ^ rest
    | _ -> n
  in
  Hashtbl.add static_externals n v

let get_builtins () =
  List.sort std_compare
    (Hashtbl.fold (fun n _ accu -> n :: accu) static_externals [])

let () =
  (Typer.from_comp_unit := fun d -> (from_descr d).typing);
  (Typer.load_comp_unit :=
     fun name ->
       if has_obj name then (
         let cu = load name in
         if !run_loaded then run cu;
         cu.descr)
       else raise Not_found);
  Typer.has_static_external := Hashtbl.mem static_externals;
  (Compile.from_comp_unit := fun d -> (from_descr d).compile);
  (Eval.get_globals := fun d -> (from_descr d).vals);
  (Eval.get_external := fun d i -> (from_descr d).exts.(i));
  Eval.get_builtin := Hashtbl.find static_externals

let stub_ml =
  ref (fun _ _ _ _ _ _ _ ->
      Printf.eprintf "Fatal error: no support for the OCaml interface.\n";
      exit 2)

let prepare_stub binary src =
  let c = real_load src in

  (* Create stub types in a fresh compilation unit *)
  Compunit.enter ();
  let i1, i2 = Compunit.get_hash c.descr in
  Compunit.set_hash (Compunit.current ()) (-i1) i2;
  !stub_ml binary src (U.get_str c.name) c.typing c.compile c.ext_info
    (fun types ->
       Compunit.leave ();
       Marshal.to_string (Value.extract_all (), types, c) [])

(* TODO: could remove typing and compile env *)

let ocaml_stub stub =
  let pools, types, (c : t) = Marshal.from_string stub 0 in
  if Tbl.mem tbl c.name then
    failwith ("CDuce unit " ^ U.get_str c.name ^ " already loaded");
  Value.intract_all pools;
  register c;
  List.iter
    (fun (name, dig) ->
       let c =
         try Tbl.find tbl name with
         | Not_found -> failwith ("CDuce unit " ^ U.get_str name ^ " not loaded")
       in
       check_digest c dig)
    c.depends;
  Tbl.add tbl c.name c;
  (types, (fun a -> c.exts <- a), c.vals, fun () ->
      try run c with e ->
        failwith (Format.asprintf "%a" Cduce_error.print_exn e))

let name d = (from_descr d).name
let run d = run (from_descr d)

let make_wrapper =
  ref (fun _ _ -> failwith "OCaml/CDuce interface not available")
