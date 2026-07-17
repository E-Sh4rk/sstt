open Sstt_repl
open Output
open Sstt

let default_timeout = 10
let raw_output = true

let pp_hr fmt f =
  let open Format in
  if f >= 1e6 then fprintf fmt "%.01fM" (f /. 1e6)
  else if f >= 1e3 then fprintf fmt "%.01fk" (f /. 1e3)
  else fprintf fmt "%.00f" f

(* Parsing of benchmark files *)
exception InvalidFormat
type ('v,'rv,'t) bench =
  { vars:'v list ; mono:'v list ; rvars:'rv list ; rmono: 'rv list ; cs:('t*'t) list; src : string }
let parse_string xml : string =
  match xml with `String str -> str | _ -> raise InvalidFormat
let parse_ty xml =
  let str = parse_string xml in
  (* Format.printf "%s@." str ; *)
  IO.parse_type str
let parse_list f xml =
  match xml with
  | `List lst -> List.map f lst
  | _ -> raise InvalidFormat
let parse_pair f1 f2 xml =
  match xml with
  | `List [e1;e2] -> (f1 e1, f2 e2)
  | _ -> raise InvalidFormat
let extract_opt_string_list field xml =
  List.assoc_opt field xml |> Option.map (parse_list parse_string)
let extract_string_list field xml =
  extract_opt_string_list field xml |> Option.value ~default:[]

let rec list_to_string = function
    `String (s:string) -> s
  | `List l -> List.map list_to_string l |> String.concat "; "
  | _ -> raise InvalidFormat
let parse_bench xml =
  try match xml with
    | `Assoc assoc ->
      let vars, mono = extract_string_list "vars" assoc, extract_string_list "mono" assoc in
      let rvars, rmono = extract_string_list "rvars" assoc, extract_string_list "rmono" assoc in
      let src = List.assoc "constr" assoc in
      let cs = src |> parse_list (parse_pair parse_ty parse_ty) in
      { vars ; mono ; rvars ; rmono ; cs; src = list_to_string src}
    | _ -> raise InvalidFormat
  with Invalid_argument _ -> raise InvalidFormat
let parse_file fn =
  let xml = Yojson.Safe.from_file fn in
  match xml with
  | `List lst -> List.map parse_bench lst
  | _ -> raise InvalidFormat

(* Build types in benchmarks *)
let build_bench b =
  let venv, rvenv = ref Ast.StrMap.empty, ref Ast.StrMap.empty in
  let var str =
    match Ast.StrMap.find_opt str !venv with
    | Some v -> v
    | None ->
      let v = Var.mk str in
      venv := Ast.StrMap.add str v !venv ;
      v
  in
  let rvar str =
    match Ast.StrMap.find_opt str !rvenv with
    | Some v -> v
    | None ->
      let v = RowVar.mk str in
      rvenv := Ast.StrMap.add str v !rvenv ;
      v
  in
  let vars, rvars = List.map var b.vars, List.map rvar b.rvars in
  let mono, rmono = List.map var b.mono, List.map rvar b.rmono in
  let env = ref { Ast.empty_env with venv = !venv ; mvenv = !venv ; rvenv = !rvenv ; mrvenv = !rvenv } in
  let cs = b.cs |> List.map (fun (s,t) ->

      let (s,env') = Ast.build_ty !env s in
      let (t,env') = Ast.build_ty env' t in
      env := env' ; (s,t)
    ) in
  { vars ; mono ; rvars ; rmono ; cs; src = b.src }
let build_bench_cduce b =
  let env, _ = CAst.resolve_vars CAst.empty_env b.vars in
  let env = ref env in
  let resolve_vars names =
    let env', vs = CAst.resolve_vars !env names in
    env := env' ;  vs
  in
  let vars, mono = resolve_vars b.vars, resolve_vars b.mono in
  let cs = b.cs |> List.map (fun (s,t) ->
      let env',tys = CAst.build_tys !env [s;t] in
      env := env' ;
      match tys with [s;t] -> (s,t) | _ -> assert false
    ) in
  { vars ; mono ; rvars=([]: unit list) ; rmono=[] ; cs; src = b.src }

(* Command line *)
let usage_msg = "sstt-bench [<file1>] [<file2>] ..."
let input_files = ref []

let anon_fun filename =
  input_files := filename::!input_files

let speclist = [ ]
let size acc t = acc := !acc + Marshal.(total_size (to_bytes t [Closures]) 0)
module type Backend =
sig
  type var
  type row_var
  type t
  type var_set
  type sub
  val build_bench : (string, string, Ast.ty) bench -> (var, row_var, t) bench
  val build_delta : var list -> row_var list -> var_set
  val tally : var_set -> (t * t) list -> sub list
  val apply_sub : sub -> t -> t
end
module SsttBackend : Backend =
struct
  type var = Var.t
  type row_var = RowVar.t
  type t = Ty.t
  type var_set = MixVarSet.t
  type sub = Subst.t
  let build_bench = build_bench
  let build_delta = MixVarSet.of_list
  let tally = Tallying.tally
  let apply_sub = Subst.apply
end
module CDuceBackend : Backend =
struct
  type var = CAst.TVar.t
  type row_var = unit
  type t = CAst.ty
  type var_set = CAst.TVarSet.t
  type sub = CAst.Subst.t
  let build_bench = build_bench_cduce
  let build_delta v _ = CAst.TVarSet.construct v
  let tally = CAst.tally
  let apply_sub = CAst.Subst.apply
end
exception Timeout
let with_timeout seconds f x =
  let handler = Sys.Signal_handle (fun _ -> raise Timeout) in
  let old_handler = Sys.signal Sys.sigalrm handler in
  let reset () =
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_handler
  in
  ignore (Unix.alarm seconds);
  match f x with
  | result -> reset (); result
  | exception e -> reset (); raise e

let () =
  Arg.parse speclist anon_fun usage_msg ;
  if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
  try
    let run () =
      let fns = List.rev !input_files in
      fns |> List.iter (fun fn ->
          if not raw_output then print Info "Processing %s" fn ;
          (* let time0 = Unix.gettimeofday () in *)
          let backend = if Config.use_cduce_backend then (module CDuceBackend : Backend) else (module SsttBackend) in
          let module B : Backend = (val backend) in
          let bench = parse_file fn in
          let time1 = Unix.gettimeofday () in
          let errors = ref [] in
          let bench = bench |> List.filter_map (fun b ->
              try
                Some (b, B.build_bench b)
              with
                CAst.Unsupported msg ->
                errors := (msg ^ ":" ^ b.src) :: !errors; None
            ) in
          let time2 = Unix.gettimeofday () in
          let n = List.length bench in
          let nsols = ref 0 in
          let isize = ref 0 in
          let osize = ref 0 in
          let ssize = ref 0 in
          let timeout = ref [] in
          if not raw_output then print Msg "Num of instances: %i" n ;
          let avg t1 t2 = (t2 -. t1) *. 1000000.0 /. (float_of_int n) in
          let all t1 t2 = (t2 -. t1) in
          let size_avg c = (float !c) /. (float n) in
          (* print Msg "Parsing (average): %.00f (%.03f)" (all time0 time1) (avg time0 time1) ; *)
          if raw_output then
            print Msg "Building (average): %.03f" (all time1 time2) 
          else print Msg "Building (average): %.03f (%.00fus)" (all time1 time2) (avg time1 time2) ;
          bench |> List.iteri (fun i (src, b) ->
              let run () =
                let mono, cs = B.build_delta b.mono b.rmono, b.cs in
                let sols = B.tally mono cs in
                nsols := !nsols + (List.length sols) ;
                let res = List.map (fun (s, t) ->
                      List.fold_left (fun acc sub ->
                          B.(apply_sub sub s, apply_sub sub t)::acc
                        ) [] sols
                    ) cs
                in
                if Config.benchmark_size then begin
                  size isize cs;
                  size osize sols;
                  size ssize res;
                end
              in
              match with_timeout default_timeout run () with
                () -> ()
              | exception Timeout ->
                timeout := (i,src.src) :: !timeout;
            ) ;
          let time3 = Unix.gettimeofday () in
          let num_timeouts = List.length !timeout in
          let num_errors = List.length !errors in
          let time3 = time3 -. (float (default_timeout * num_timeouts)) in
          if raw_output then begin 
            print Msg "Tallying (average): %.03f" (all time2 time3);
            print Msg "Total (average): %.03f" (all time1 time3);
            print Msg "Total solutions: %i" (!nsols); 
            if Config.benchmark_size then begin
              print Msg "Total space: %a" pp_hr (float (!ssize + !isize + !osize));
              print Msg "Average space: %a" pp_hr (size_avg (ref (!ssize + !isize + !osize)));
              if Config.use_cduce_backend then print Msg "Peak space: N/A"
              else
                print Msg "Peak space: %a" pp_hr (float (!Config.max_ty_size));
            end;
            print Msg "Total errors: %d" (max num_errors num_timeouts);
          end else begin
            print Msg "Tallying (average): %.03f (%.03fus)" (all time2 time3) (avg time2 time3) ;
            print Msg "Total (average): %.03f (%.03fus)" (all time1 time3) (avg time1 time3) ;
            print Msg "Total solutions: %i" (!nsols); 
            if Config.benchmark_size then begin
              print Msg "Input nodes (average): %d (%.00f)" !isize (size_avg isize);
              print Msg "Output nodes (average): %d (%.00f)" !osize (size_avg osize);
              print Msg "Subst nodes (average): %d (%.00f)" !ssize (size_avg ssize);
              print Msg "Total space (average): %d (%.00f)" (!ssize + !isize + !osize)
                (size_avg (ref (!ssize + !isize + !osize)));
              if Config.use_cduce_backend then print Msg "Peak space: N/A"
              else
                print Msg "Peak space: %a" pp_hr (float (!Config.max_ty_size));
            end;
            print Msg "Total timouts: %d" num_timeouts;
            print Msg "Total errors: %d" num_errors;
            if num_timeouts <> 0 || num_errors <> 0 then begin
              print Msg "Failures:";
              List.iter (fun (i,s) -> print Msg "  %d: %s" i s) !timeout;
              if true then List.iter (fun (s) -> print Msg "  %s" s) !errors;
            end;
          end
        )
    in
    with_rich_output Format.std_formatter run ()
  with
  | IO.LexicalError (p, msg)
  | IO.SyntaxError (p, msg) ->
    Format.printf "@.%s: %s@." (Position.string_of_pos p) msg
  | e ->
    let msg = Printexc.to_string e in
    Format.printf "@.Uncaught exception: %s@." msg
