open Builtin_defs
open Encodings

let eval = ref (fun ppf err s -> assert false)

(* Types *)

let stringn = Types.cons string
let namespaces = Types.Sequence.star (Types.times stringn stringn)

let types =
  [
    ("Empty", Types.empty);
    ("Any", any);
    ("Int", int);
    ("Char", Types.char CharSet.any);
    ("Byte", char_latin1);
    ("Atom", atom);
    ("Pair", Types.Times.any);
    ("Arrow", Types.Function.any);
    ("Record", Types.Rec.any);
    ("String", string);
    ("Latin1", string_latin1);
    ("Bool", bool);
    ("Float", float);
    ("AnyXml", any_xml);
    ("Namespaces", namespaces);
    ("Caml_int", caml_int);
  ]

let env =
  List.fold_left
    (fun accu (n, t) ->
      let n = (Ns.empty, Utf8.mk n) in
      Types.Print.register_global "" n t;
      Typer.enter_type n (t, []) accu)
    Typer.empty_env types

(* Operators *)

open Operators

let binary_op_gen = register_binary
let unary_op_gen = register_unary

let binary_op name t1 t2 f run =
  binary_op_gen name
    (fun arg1 arg2 loc _ _ -> f (arg1 loc t1 true) (arg2 loc t2 true))
    run

let binary_op_cst = register_op2

let binary_op_warning2 name t1 t2 w2 t run =
  binary_op_gen name
    (fun arg1 arg2 loc _ _ ->
      ignore (arg1 loc t1 false);
      let r = arg2 loc t2 true in
      if not (Types.subtype r w2) then
        Cduce_error.(warning ~loc "This operator may fail" ());
      t)
    run

let unary_op_warning name targ w t run =
  unary_op_gen name
    (fun arg loc _ _ ->
      let res = arg loc targ true in
      if not (Types.subtype res w) then
        Cduce_error.(warning ~loc "This operator may fail" ());
      t)
    run

open Ident

let raise_gen exn =
  raise (Value.CDuceExn (Value.string_latin1 (Printexc.to_string exn)))

let exn_load_file_utf8 =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "load_file_utf8"))
          (Value.string_latin1 "File is not a valid UTF-8 stream")))

let exn_int_of =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "Invalid_argument"))
          (Value.string_latin1 "int_of")))

let exn_char_of =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "Invalid_argument"))
          (Value.string_latin1 "char_of")))

let exn_float_of =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "Invalid_argument"))
          (Value.string_latin1 "float_of")))

let exn_namespaces =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "Invalid_argument"))
          (Value.string_latin1 "namespaces")))

let exn_cdata_of =
  lazy
    (Value.CDuceExn
       (Value.pair
          (Value.Atom (AtomSet.V.mk_ascii "Invalid_argument"))
          (Value.string_latin1 "cdata_of")))

let eval_load_file ~utf8 e =
  let fn = Value.get_string_latin1 e in
  let s = Url.load_url fn in
  if utf8 then
    match Utf8.mk_check s with
    | Some s -> Value.string_utf8 s
    | None -> raise (Lazy.force exn_load_file_utf8)
  else Value.string_latin1 s

let () = ()

(* Comparison operators *)
;;

binary_op "=" any any
  (fun t1 t2 -> if Types.is_empty (Types.cap t1 t2) then false_type else bool)
  (fun v1 v2 -> Value.vbool (Value.compare v1 v2 == 0))
;;

binary_op_cst "<=" any any bool (fun v1 v2 ->
    Value.vbool (Value.compare v1 v2 <= 0))
;;

binary_op_cst "<" any any bool (fun v1 v2 ->
    Value.vbool (Value.compare v1 v2 < 0))
;;

binary_op_cst ">=" any any bool (fun v1 v2 ->
    Value.vbool (Value.compare v1 v2 >= 0))
;;

binary_op_cst ">" any any bool (fun v1 v2 ->
    Value.vbool (Value.compare v1 v2 > 0))

(* I/O *)
;;

register_fun "char_of_int" int (Types.char CharSet.any) (function
  | Value.Integer x -> (
      try Value.Char (CharSet.V.mk_int (Intervals.V.get_int x)) with
      | Failure _ -> raise (Lazy.force exn_int_of))
  | _ -> assert false)
;;

register_fun "int_of_char" (Types.char CharSet.any) int (function
  | Value.Char x -> Value.Integer (Intervals.V.from_int (CharSet.V.to_int x))
  | _ -> assert false)
;;

register_fun "string_of" any string_latin1 (fun v ->
    let b = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer b in
    Value.print ppf v;
    Format.pp_print_flush ppf ();
    Value.string_latin1 (Buffer.contents b))
;;

register_fun "load_xml" string_latin1 any_xml (fun v ->
    Load_xml.load_xml (Value.get_string_latin1 v))
;;

register_fun "!load_xml" string_latin1 any_xml (fun v ->
    Load_xml.load_xml ~ns:true (Value.get_string_latin1 v))
;;

register_fun "load_html" string_latin1 Types.Sequence.any (fun v ->
    Load_xml.load_html (Value.get_string_latin1 v))
;;

register_fun "load_file_utf8" string_latin1 string (eval_load_file ~utf8:true);;

register_fun "load_file" string_latin1 string_latin1
  (eval_load_file ~utf8:false)

let argv = ref Value.Absent;;

register_fun "print_xml" Types.any string_latin1 (fun v ->
    Print_xml.print_xml ~utf8:false !Eval.ns_table v)
;;

register_fun "print_xml_utf8" Types.any string (fun v ->
    Print_xml.print_xml ~utf8:true !Eval.ns_table v)
;;

register_fun "dump_xml" Types.any nil (fun v ->
    Print_xml.dump_xml ~utf8:false !Eval.ns_table v)
;;

register_fun "dump_xml_utf8" Types.any nil (fun v ->
    Print_xml.dump_xml ~utf8:true !Eval.ns_table v)
;;

register_fun "print" string_latin1 nil (fun v ->
    print_string (Value.get_string_latin1 v);
    flush stdout;
    Value.nil)
;;

register_fun "print_utf8" string nil (fun v ->
    let s = Value.cduce2ocaml_string_utf8 v in
    print_string (Utf8.get_str s);
    flush stdout;
    Value.nil)
;;

unary_op_warning "int_of" string intstr int (fun v ->
    let s, _ = Value.get_string_utf8 v in
    let str = Utf8.get_str s in
    try
      let modifier = str.[String.index str '0' + 1] in
      if
        modifier = 'x' || modifier = 'X' || modifier = 'b' || modifier = 'B'
        || modifier = 'o' || modifier = 'O'
      then Value.Integer (Intervals.V.from_int (int_of_string str))
      else Value.Integer (Intervals.V.mk str)
    with
    | _ -> (
        try Value.Integer (Intervals.V.mk str) with
        | Failure _ -> raise (Lazy.force exn_int_of)))

(*  It was like that                                    *)
(*     try Value.Integer (Intervals.V.mk (Utf8.get_str s)) *)
(*                 UTF-8 is ASCII compatible !          *)
(* modified to allow 0x 0b 0o notations                 *)

(*
register_fun "atom_of"
  string atom
  (fun v ->
     let (s,_) = Value.get_string_utf8 v in 
     Value.Atom (AtomSet.V.mk Ns.empty s));;
*)
;;

register_fun "split_atom" atom (Types.times stringn stringn) (function
  | Value.Atom q ->
      let ns, l = AtomSet.V.value q in
      Value.pair (Value.string_utf8 (Ns.Uri.value ns)) (Value.string_utf8 l)
  | _ -> assert false)
;;

register_fun "make_atom" (Types.times stringn stringn) atom (fun v ->
    let v1, v2 = Value.get_pair v in
    let ns, _ = Value.get_string_utf8 v1 in
    let l, _ = Value.get_string_utf8 v2 in
    (* TODO: check that l is a correct Name wrt XML *)
    Value.Atom (AtomSet.V.mk (Ns.Uri.mk ns, l)))
;;

binary_op_warning2 "dump_to_file" string_latin1 string string_latin1 nil
  (fun f v ->
    try
      let oc = open_out (Value.get_string_latin1 f) in
      output_string oc (Value.get_string_latin1 v);
      close_out oc;
      Value.nil
    with
    | exn -> raise_gen exn)
;;

binary_op_cst "dump_to_file_utf8" string_latin1 string nil (fun f v ->
    try
      let oc = open_out (Value.get_string_latin1 f) in
      let v, _ = Value.get_string_utf8 v in
      output_string oc (Utf8.get_str v);
      close_out oc;
      Value.nil
    with
    | exn -> raise_gen exn)

(* Integer operators *)
;;

binary_op_gen "+"
  (fun arg1 arg2 loc constr precise ->
    let t1 = arg1 loc (Types.cup number Types.Rec.any) true in
    if Types.subtype t1 int then
      let t2 = arg2 loc number true in
      if Types.subtype t2 int then
        Types.interval (Intervals.add (Types.Int.get t1) (Types.Int.get t2))
      else float
    else if Types.subtype t1 number then
      let _ = arg2 loc number true in
      float
    else if Types.subtype t1 Types.Rec.any then
      let t2 = arg2 loc Types.Rec.any true in
      Types.Record.merge t1 t2
    else
      Cduce_error.(raise_err_loc ~loc Typer_Error "The first argument mixes numbers and records"))
  Value.add
;;

binary_op_gen "-"
  (fun arg1 arg2 loc _ _ ->
    let t1 = arg1 loc number true in
    let t2 = arg2 loc number true in
    if Types.subtype t1 int && Types.subtype t2 int then
      Types.interval (Intervals.sub (Types.Int.get t1) (Types.Int.get t2))
    else float)
  Value.sub
;;

binary_op_gen "*"
  (fun  arg1 arg2 loc _ _ ->
    let t1 = arg1 loc number true in
    let t2 = arg2 loc number true in
    if Types.subtype t1 int && Types.subtype t2 int then
      Types.interval (Intervals.mul (Types.Int.get t1) (Types.Int.get t2))
    else float)
  Value.mul

let type_div_mod arg1 arg2 loc _ _ =
  let t1 = arg1 loc number true in
  let t2 = arg2 loc number true in
  if Types.subtype t1 int && Types.subtype t2 int then begin
    if not (Types.subtype t2 non_zero_int) then
      Cduce_error.(warning ~loc "This operator may fail" ());
    int
  end
  else float
;;

binary_op_gen "/" type_div_mod Value.div;;
binary_op_gen "%" type_div_mod Value.modulo;;

binary_op_gen "@"
  (fun arg1 arg2 loc constr precise ->
    let constr' = Types.Sequence.ub_concat constr in
    let exact = Types.subtype constr' constr in
    if exact then
      let t1 = arg1 loc constr' precise
      and t2 = arg2 loc constr' precise in
      if precise then Types.Sequence.concat t1 t2 else constr
    else
      (* Note:
         the knownledge of t1 may makes it useless to
         check t2 with 'precise' ... *)
      let t1 = arg1 loc constr' true
      and t2 = arg2 loc constr' true in
      Types.Sequence.concat t1 t2)
  Value.concat
;;

unary_op_gen "flatten" Typer.flatten Value.flatten;;
register_fun "raise" any Types.empty (fun v -> raise (Value.CDuceExn v));;

register_fun "namespaces" any_xml namespaces (function
  | Value.XmlNs (_, _, _, ns) ->
      Value.sequence_rev
        (List.map
           (fun (pr, ns) ->
             Value.pair (Value.string_utf8 pr)
               (Value.string_utf8 (Ns.Uri.value ns)))
           (Ns.get_table ns))
  | Value.Xml _ -> raise (Lazy.force exn_namespaces)
  | _ -> assert false)
;;

register_fun2 "set_namespaces" namespaces any_xml any_xml (fun ns -> function
  | Value.XmlNs (v1, v2, v3, _)
  | Value.Xml (v1, v2, v3) ->
      let ns = Value.get_sequence_rev ns in
      let ns =
        List.map
          (fun v ->
            let pr, ns = Value.get_pair v in
            let pr, _ = Value.get_string_utf8 pr in
            let ns, _ = Value.get_string_utf8 ns in
            (pr, Ns.Uri.mk ns))
          ns
      in
      Value.XmlNs (v1, v2, v3, Ns.mk_table ns)
  | _ -> assert false)

(* Float *)
;;

register_fun "float_of" string float (fun v ->
    let s, _ = Value.get_string_utf8 v in
    try Value.float (float_of_string (Utf8.get_str s)) with
    | Failure _ -> raise (Lazy.force exn_float_of))

(* cdata *)
;;

register_fun "cdata_of" string string (fun v ->
    let s, _ = Value.get_string_utf8 v in
    try Value.cdata (Utf8.get_str s) with
    | Failure _ -> raise (Lazy.force exn_cdata_of))
