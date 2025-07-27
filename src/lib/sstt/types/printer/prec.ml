(** Precedence of operators and associativity *)

(** Unary operators *)
type unop =
  | Neg

(** Binary operators *)
type binop =
  | Diff | Arrow

(** Variadic operators *)
type varop =
  | Tuple | Cup | Cap

(** Associativity *)
type assoc = Left | Right | NoAssoc

(** Returns the separator, the priority (as an integer) and the associativity of
    a variadic operator. *)
let varop_info v = match v with
  | Tuple -> ", ", 0, NoAssoc
  | Cup -> " | ", 2, NoAssoc
  | Cap -> " & ", 3, NoAssoc

(** Returns the separator, the priority (as an integer) and the associativity of
    a binary operator. *)
let binop_info b = match b with
  | Arrow -> " -> ", 1, Right
  | Diff -> " \\ ", 4, Left

(** Returns the separator, the priority (as an integer) and the associativity of
    a unary operator. *)
let unop_info u = match u with
  | Neg -> "~", 5, NoAssoc

(** Maximum priority *)
let max_prec = 100

(** Minimum priority *)
let min_prec = (-1)

(** [need_parentheses lvl assoc info] returns [true] if the operator described
    by [info] needs parentheses for the current printing level [lvl] and the
    current associativity [assoc].
*)
let need_parentheses (prec:int) assoc ((_:string),prec',assoc') =
  prec' < prec || prec' = prec && (assoc' <> assoc || assoc' = NoAssoc)

(** [fprintf lvl assoc info fmt f …] works as [Format.fprintf] but will add
    parentheses around the formatted output if required by [lvl] [assoc] and
    [info].
*)
let fprintf prec assoc opinfo fmt f =
  if need_parentheses prec assoc opinfo
  then Format.fprintf fmt ("("^^f^^")")
  else Format.fprintf fmt f