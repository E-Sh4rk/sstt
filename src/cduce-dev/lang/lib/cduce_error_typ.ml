open Cduce_loc

module U = Encodings.Utf8

type _ error_t =
  | Typer_WrongLabel : (Types.descr * Ident.label) error_t
  | Typer_ShouldHave : (Types.descr * string) error_t
  | Typer_ShouldHave2 : (Types.descr * string * Types.descr) error_t
  | Typer_Constraint : (Types.descr * Types.descr) error_t
  | Typer_NonExhaustive : Types.descr error_t
  | Typer_Error : string error_t
  | Typer_UnboundId : (Ident.id * bool) error_t
  | Typer_InvalidConstant : unit error_t
  | Typer_InvalidRecInst : string error_t
  | Typer_InvalidInstArity : (string * int * int) error_t
  | Typer_MultipleTypeDef : (Ident.id * loc) error_t
  | Typer_CaptureNotAllowed : Ident.id error_t
  | Typer_UnboundTypeVariable : (Ident.id * Var.t) error_t
  | Typer_WeakVar : (Ns.QName.t * Types.t) error_t
  | Typer_Pattern : (string) error_t

  | Ast_Parsing_error : string error_t
  | Parse_Invalid_byte : (string * [`Ascii | `Latin1 | `Utf8]) error_t
  | Parse_Failure : unit error_t
  | Sedlexer_Error : string error_t

  | Librarian_InconsistentCrc : U.t error_t
  | Librarian_InvalidObject : string error_t
  | Librarian_CannotOpen : string error_t
  | Librarian_NoImplementation : U.t error_t

  | Driver_Escape : exn error_t
  | Driver_InvalidInputFilename : string error_t
  | Driver_InvalidObjectFilename : string error_t


  | Ocamliface : string error_t
  | Ocamliface_unsupported : string error_t

  | Schema_common_XSD_validation_error : string error_t
  | Schema_common_XSI_validation_error : string error_t
  | Schema_validator_Failure : unit error_t
  | Schema_validator_Facet_error : string error_t
  | Schema_builtin_Error : string error_t
  | Schema_builtin_Malformed_URL : bytes error_t
  | Schema_xml_Error : string error_t

  | Url_Malformed_URL : string error_t

  (* | Ns_Label_Not_unique : Ns.Label.value * Ns.Label.value *)
  (* | Ns_Uri_Not_unique : Ns.Uri.value * Ns.Uri.value *)
  (* | Types_Sequence_Error : Types.Sequence.error *)
  (* Later: Replace the raising : exceptions above with Error parametrized by the corresponding value *)

  (* New exceptions: *)
  | Explain_Failed : (Value.t * string) list error_t
  | Typepat_Unify : unit error_t
  | Typepat_FoundFv : Ns.QName.t error_t

  (* NOTE: using as replace for now : Cduce_loc.Generic in places outside : Cduce_loc *)
  | Generic : string error_t

  (* NOTE: Pack other exceptions being raised. Example: typing/typer.ml line 1032 *)
  (* Also being used to handle Types.Sequence.Error (typer.ml line 1514) *)
  | Other_Exn : exn error_t


let phase_name (type a) (e : a error_t) =
  match e with
  | Typer_WrongLabel
  | Typer_ShouldHave
  | Typer_ShouldHave2
  | Typer_Constraint
  | Typer_NonExhaustive
  | Typer_Error
  | Typer_UnboundId
  | Typer_InvalidConstant
  | Typer_InvalidRecInst
  | Typer_InvalidInstArity
  | Typer_MultipleTypeDef
  | Typer_CaptureNotAllowed
  | Typer_UnboundTypeVariable
  | Typer_WeakVar
  | Typer_Pattern -> "Typing"

  | Ast_Parsing_error
  | Parse_Invalid_byte
  | Parse_Failure
  | Sedlexer_Error -> "Syntax"

  | Librarian_InconsistentCrc
  | Librarian_InvalidObject
  | Librarian_CannotOpen
  | Librarian_NoImplementation -> "Linking"

  | Driver_Escape
  | Driver_InvalidInputFilename
  | Driver_InvalidObjectFilename -> "Program"

  | Ocamliface
  | Ocamliface_unsupported -> "OCaml binding"


  | Schema_common_XSD_validation_error
  | Schema_common_XSI_validation_error
  | Schema_validator_Failure
  | Schema_validator_Facet_error
  | Schema_builtin_Error
  | Schema_builtin_Malformed_URL
  | Schema_xml_Error -> "Schema validation"

  (* | Ns_Label_Not_unique : Ns.Label.value * Ns.Label.value *)
  (* | Ns_Uri_Not_unique : Ns.Uri.value * Ns.Uri.value *)
  (* | Types_Sequence_Error : Types.Sequence.error *)
  (* Later: Replace the raising : exceptions above with Error parametrized by the corresponding value *)

  (* New exceptions: *)
  | Explain_Failed
  | Typepat_Unify
  | Typepat_FoundFv -> "Internal"

  | Url_Malformed_URL (* Can be rased dynamically or statically when using explicit url in namespace definitions. *)

  (* NOTE: using as replace for now : Cduce_loc.Generic in places outside : Cduce_loc *)
  | Generic

  (* NOTE: Pack other exceptions being raised. Example: typing/typer.ml line 1032 *)
  (* Also being used to handle Types.Sequence.Error (typer.ml line 1514) *)
  | Other_Exn -> "Runtime"
