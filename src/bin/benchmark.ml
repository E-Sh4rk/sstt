open Sstt_repl
open Output
open Sstt

(* Parsing of benchmark files *)
exception InvalidFormat
type ('v,'t) bench = { vars:'v list ; mono:'v list ; cs:('t*'t) list ; prio:'v list option }
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
let parse_bench xml =
    try match xml with
    | `Assoc assoc ->
        let vars = List.assoc "vars" assoc |> parse_list parse_string in
        let mono = List.assoc "mono" assoc |> parse_list parse_string in
        let cs = List.assoc "constr" assoc |> parse_list (parse_pair parse_ty parse_ty) in
        let prio = List.assoc_opt "prio" assoc |> Option.map (parse_list parse_string) in
        { vars ; mono ; cs ; prio }
    | _ -> raise InvalidFormat
    with Invalid_argument _ -> raise InvalidFormat
let parse_file fn =
    let xml = Yojson.Safe.from_file fn in
    match xml with
    | `List lst -> List.map parse_bench lst
    | _ -> raise InvalidFormat

(* Build types in benchmarks *)
let build_bench b =
    let venv = b.vars |> List.map (fun str -> str, Var.mk str) |> Ast.StrMap.of_list in
    let venv = ref venv in
    let var str =
        match Ast.StrMap.find_opt str !venv with
        | Some v -> v
        | None ->
            let v = Var.mk str in
            venv := Ast.StrMap.add str v !venv ;
            v
    in
    let vars, mono = List.map var b.vars, List.map var b.mono in
    let prio = b.prio |> Option.map (List.map var) in
    let env = ref { Ast.empty_env with venv = !venv ; mvenv = !venv } in
    let cs = b.cs |> List.map (fun (s,t) ->
            
        let (s,env') = Ast.build_ty !env s in
        let (t,env') = Ast.build_ty env' t in
        env := env' ; (s,t)
    ) in
    { vars ; mono ; cs ; prio }

(* Command line *)
let usage_msg = "sstt-bench [<file1>] [<file2>] ..."
let input_files = ref []

let anon_fun filename =
    input_files := filename::!input_files

let speclist = [ ]

let () =
    Arg.parse speclist anon_fun usage_msg ;
    if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
    try
        let run () =
            let fns = List.rev !input_files in
            fns |> List.iter (fun fn ->
                print Info "Processing %s" fn ;
                (* let time0 = Unix.gettimeofday () in *)
                let bench = parse_file fn in
                let time1 = Unix.gettimeofday () in
                let n = List.length bench in
                print Msg "Num of instances: %i" n ;
                let avg t1 t2 = (t2 -. t1) *. 1000000.0 /. (float_of_int n) in
                let all t1 t2 = (t2 -. t1) (* *. 1000.0 *) in
                (* print Msg "Parsing (average): %.02fs (%.00fus)" (all time0 time1) (avg time0 time1) ; *)
                let bench = bench |> List.map build_bench in
                let time2 = Unix.gettimeofday () in
                print Msg "Building (average): %.02fs (%.00fus)" (all time1 time2) (avg time1 time2) ;
                bench |> List.iter (fun b ->
                    let mono, cs = VarSet.of_list b.mono, b.cs in
                    let _ = match b.prio with
                    | None -> Tallying.tally mono cs
                    | Some prio -> Tallying.tally_with_priority prio mono cs
                    in
                    ()
                ) ;
                let time3 = Unix.gettimeofday () in
                print Msg "Tallying (average): %.02fs (%.00fus)" (all time2 time3) (avg time2 time3) ;
                print Msg "Total (average): %.02fs (%.00fus)" (all time1 time3) (avg time1 time3)
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
