open Cduce_loc

include Cduce_error_typ

(* TODO: In tests, it seems that for some reason the datatype inside the exception is not printed. However, running the normal ocaml repl it is printed *)
(* TODO: More tests? Not too many `bad` tests *)
(* TODO: Replace `failwith`s? *)
(* TODO: What about warning defined in typer line 11? *)
(* TODO: there are many different `raise*` functions that take an exception and a location. This should be centralized here *)
(* TODO: Patterns_Error has a pretty big error message as payload and location is being added manually *)
(* TODO: also compile/operators -> Typer_Error (line 16 and 28) *)
(* TODO: also typing/typer line 196 *)

(* exception UnknownPrefix of U.t -> exception defined at ns.ml *)

(* exception Failed of t on explain.ml *)

type loc_error_t =
  | Unlocated
  | Located of loc
  | PreciselyLocated of loc * precise

exception Error : loc_error_t * ('a error_t * 'a) -> exn
type pack = P : 'a error_t * 'a -> pack
let raise_err err  arg = raise (Error (Unlocated, (err, arg)))
let raise_err_loc ~loc err arg  = raise (Error (Located loc, (err, arg)))
let raise_err_generic msg = raise_err Generic msg
let raise_err_generic_loc ~loc msg = raise_err_loc ~loc Generic msg
let raise_err_precise ~loc precise err arg =
  raise (Error (PreciselyLocated (loc, precise), (err, arg)))
(*let raise_err_loc_source i j err arg =
  raise (Error (Located ((get_source (), i, j)), (err, arg)))
*)

let print_value ppf v = Value.print ppf v

let print_norm ppf d = Types.Print.print ppf d
let print_sample ppf s = Types.Print.print ppf s
let print_protect ppf s = Format.fprintf ppf "%s" s

open Format
let print_loc_error_t ppf =
  function
  | Unlocated -> ()
  | Located loc -> print_loc ppf (loc, `Full)
  | PreciselyLocated (loc, precise) -> print_loc ppf (loc, precise)


let pr_encoding ppf x =
  let s = match x with
  `Ascii -> "ascii"
  | `Latin1 -> "latin1"
  | `Utf8 -> "utf-8"
in fprintf ppf "%s" s

let rec print_error ppf (P (e, arg)) =
  let open Cduce_error_typ in
  match e, arg with
  (* Typer errors *)
  | Typer_Error, s -> fprintf ppf "%s" s

  | Typer_UnboundId, (x, tn) ->
    if tn then
      fprintf ppf "Type name %a is unbound" Ident.print x
    else
      fprintf ppf "Unbound identifier %a" Ident.print x
  | Typer_InvalidConstant, () ->
    fprintf ppf "This pattern should be a scalar or structured constant"

  | Typer_InvalidRecInst, id ->
    fprintf ppf
      "Invalid instantiation of type '%s' during its recursive definition"
      id
  | Typer_InvalidInstArity, (id, i, j) ->
    fprintf ppf "Polymorphic type '%s' expects %d parameters, but was given %d"
      id i j

  | Typer_MultipleTypeDef, (id, loc) ->
    fprintf ppf "Multiple definitions for identifier %a (another definition is at %a)"
      Ident.print id
      Cduce_loc.print_loc (loc, `Full)

  | Typer_CaptureNotAllowed, id ->
    fprintf ppf "Capture variable not allowed: %a" Ident.print id
  | Typer_UnboundTypeVariable, (id, v) ->
    fprintf ppf "Unbound type variable %a in the definition of type %a"
      Var.print v Ident.print id
  | Typer_WrongLabel, (t, l) ->
    fprintf ppf "Wrong record selection; field %a " Ns.Label.print_attr l;
    fprintf ppf "not present in an expression of type:@, %a"
      print_norm t
  | Typer_ShouldHave, (t, msg) ->
    fprintf ppf "This expression should have type:@,  %a@,%a"
      print_norm t print_protect msg
  | Typer_ShouldHave2, (t1, msg, t2) ->
    fprintf ppf "This expression should have type:@,  %a@,%a@,  %a"
      print_norm t1 print_protect msg print_norm t2
  | Typer_Constraint, (s, t) ->
    fprintf ppf "This expression should have type:@,  %a@," print_norm t;
    fprintf ppf "but its inferred type is:@,  %a@," print_norm s;
    fprintf ppf "which is not a subtype, as shown by the sample:@   %a"
      print_sample
      (Types.Sample.get (Types.diff s t))
  | Typer_NonExhaustive, t ->
    fprintf ppf "This pattern matching is not exhaustive@\n";
    fprintf ppf "Residual type:%a@\n" print_norm t;
    fprintf ppf "Sample:%a" print_sample (Types.Sample.get t)
  | Typer_WeakVar, (id, t) ->
    fprintf ppf
      "Identifier %a has type:@\n\
       %a@\n\
       which contains weak polymorphic variables."
      Ident.print id Types.Print.print t

  | Typer_Pattern, s -> fprintf ppf "%s" s

  | Ast_Parsing_error, s ->
    if s = "" then fprintf ppf ""
    else fprintf ppf "%a" print_protect s

  | Parse_Invalid_byte, (s, e) ->
    fprintf ppf "Invalid byte sequence %S for encoding %a" s pr_encoding e

  | Parse_Failure, () ->  fprintf ppf ""

  | Sedlexer_Error, s -> fprintf ppf "%s" s

  | Librarian_InconsistentCrc, name ->
    fprintf ppf "Inconsistent checksum (compilation unit: %a)"
      U.print name
  | Librarian_InvalidObject, f -> fprintf ppf "Invalid object file %s" f
  | Librarian_CannotOpen, f -> fprintf ppf "Cannot open file %s" f
  | Librarian_NoImplementation, name ->
    fprintf ppf "No implementation found for compilation unit: %a"
      U.print name

  | Driver_Escape, _ -> failwith "do not exist on cduce_driver.print_exn"
  | Driver_InvalidInputFilename, _f ->
    fprintf ppf "Source filename must have extension .cd"
  | Driver_InvalidObjectFilename, _f ->
    fprintf ppf "Object filename must have extension .cdo"

  | Ocamliface, msg -> fprintf ppf "%s" msg
  | Ocamliface_unsupported, f ->
    fprintf ppf "Unsupported feature (%s) found in .cmi" f
  | Schema_common_XSD_validation_error, s -> fprintf ppf "%s" s
  | Schema_common_XSI_validation_error, s -> fprintf ppf "%s" s
  | Schema_validator_Failure, () -> ()
  | Schema_validator_Facet_error, s -> fprintf ppf "%s" s
  | Schema_builtin_Error, s -> fprintf ppf "%s" s
  | Schema_builtin_Malformed_URL, s -> fprintf ppf "%s" (Bytes.to_string s)
  | Schema_xml_Error, s -> fprintf ppf "%s" s


  | Url_Malformed_URL, s -> fprintf ppf "Malformed URL: `%s'" s

  | Other_Exn, (e : exn) -> print_exn ppf e
  | Generic, s -> fprintf ppf "%s" s
  
  | Explain_Failed, _
  | Typepat_Unify, _
  | Typepat_FoundFv, _ -> fprintf ppf "Internal error, please report"


and print_exn ppf = function
  | Value.CDuceExn v ->
    fprintf ppf "Uncaught CDuce exception: @[%a@]" print_value v
  | Types.Sequence.Error (Types.Sequence.CopyTag (t, expect)) ->
    let open Types in
    fprintf ppf
      "Tags in %a will be copied, but only %a are allowed.Counter-example:@ @[%a@]"
      Print.print t Print.print expect Types.Print.print
      (Sample.get (Types.diff t expect))
  | Types.Sequence.Error (Types.Sequence.CopyAttr (t, expect)) ->
    let open Types in
    fprintf ppf
      "Attributes in %a will be copied, but only %a are allowed. Counter-example:@ @[%a@]"
      Print.print t Print.print expect Types.Print.print
      (Sample.get (Types.diff t expect))
  | Types.Sequence.Error (Types.Sequence.UnderTag (t, exn)) ->
    fprintf ppf "Under tag %a:" Types.Print.print t;
    print_exn ppf exn
  | Ns.Label.Not_unique ((ns1, s1), (ns2, s2)) ->
    fprintf ppf "Collision on label hash: {%a}:%a, {%a}:%a" U.print
      (Ns.Uri.value ns1) U.print s1 U.print (Ns.Uri.value ns2) U.print s2
  | Ns.Uri.Not_unique (ns1, ns2) ->
    fprintf ppf "Collision on namespaces hash: %a, %a" U.print ns1
      U.print ns2
  | Error (loc, (e,arg)) ->
    print_loc_error_t ppf loc;
    print_error ppf (P (e, arg))

  | exn ->
    fprintf ppf "%a" print_protect (Printexc.to_string exn);
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace exn bt

let print_error_loc (type a) ppf loc ((e, arg) : ('a error_t * 'a)) =
  fprintf ppf "@[%a: %s error:@\n@  @[<v>%a@]@]@."
    print_loc_error_t loc
    (Cduce_error_typ.phase_name e)
    print_error (P (e, arg))


let warning (type a) ~loc s x =
  (* TODO FIX FORMATTER *)
  fprintf Format.err_formatter "@[%a:@\nWarning: %s@]@."
    print_loc_error_t (Located loc)
    s;
  x

let mk_loc loc v = Error (Located loc, v)