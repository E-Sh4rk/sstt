open Cduce_types
open Cduce_core
open Operators
open Builtin_defs
open Ident

let variant_type_ascii l =
  List.fold_left
    (fun accu (l, t) ->
      Types.cup accu
        (Types.times
           (Types.cons (Types.atom (AtomSet.atom (AtomSet.V.mk_ascii l))))
           (Types.cons t)))
    Types.empty l

let record_type_ascii l =
  Types.record_fields
    ( false,
      LabelMap.from_list_disj
        (List.map (fun (l, t) -> (Value.label_ascii l, Types.cons t)) l) )

module Reader = struct
  let b = Buffer.create 10240
  let buf = Bytes.create 1024

  let rec read_loop ic =
    let i = input ic buf 0 (Bytes.length buf) in
    if i > 0 then (
      Buffer.add_string b (Bytes.sub_string buf 0 i);
      read_loop ic)

  let ic ic =
    read_loop ic;
    let s = Buffer.contents b in
    Buffer.clear b;
    s
end

let run_process cmd =
  let ((sout, sin, serr) as h) =
    Unix.open_process_full cmd (Unix.environment ())
  in
  Unix.close (Unix.descr_of_out_channel sin);

  (* used to be: (close_out sin), but OCaml 3.09.2 seems to segfault
     on double closing channels. *)
  let sout = Reader.ic sout in
  let serr = Reader.ic serr in
  (sout, serr, Unix.close_process_full h)

let process_status = function
  | Unix.WEXITED n ->
      Value.pair (Value.atom_ascii "exited") (Value.ocaml2cduce_int n)
  | Unix.WSTOPPED n ->
      Value.pair (Value.atom_ascii "stopped") (Value.ocaml2cduce_int n)
  | Unix.WSIGNALED n ->
      Value.pair (Value.atom_ascii "signaled") (Value.ocaml2cduce_int n)

let system_out =
  record_type_ascii
    [
      ("stdout", string_latin1);
      ("stderr", string_latin1);
      ( "status",
        variant_type_ascii
          [ ("exited", int); ("stopped", int); ("signaled", int) ] );
    ]

let use () =
  let () =
    register_fun "system" string_latin1 system_out (fun v ->
        let cmd = Value.get_string_latin1 v in
        let sout, serr, ps = run_process cmd in
        Value.record_ascii
          [
            ("stdout", Value.string_latin1 sout);
            ("stderr", Value.string_latin1 serr);
            ("status", process_status ps);
          ])
  in
  let () =
    register_fun "exit" unsigned_byte_int Types.empty (fun v ->
        exit (Value.cduce2ocaml_int v))
  in
  let exn_not_found =
    Value.CDuceExn (Value.Atom (AtomSet.V.mk_ascii "Not_found"))
  in
  let () =
    register_fun "getenv" string_latin1 string_latin1 (fun e ->
        let var = Value.get_string_latin1 e in
        try Value.string_latin1 (Sys.getenv var) with
        | Not_found -> raise exn_not_found)
  in
  let () =
    register_fun "argv" nil (Types.Sequence.star string_latin1) (fun _e ->
        !Builtin.argv)
  in
  ()

let () = Cduce_config.register "system" "System calls" use
