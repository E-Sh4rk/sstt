open Ast

exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

val parse_program_file : string -> program
val parse_program : string -> program
val parse_type : string -> ty
val parse_command : Lexing.lexbuf -> command
