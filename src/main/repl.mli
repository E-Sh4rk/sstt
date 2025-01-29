open Sstt_parsing.Ast
open Sstt_types

val empty_env : env
val treat_elt : ?pparams:Printer.params -> env -> elt -> env
