(** Precedence of operators and associativity *)

(** Unary operators *)
type unop =
  | Neg

(** Unary field operators *)
type funop =
  | FNeg

(** Binary operators *)
type binop =
  | Diff | Arrow

(** Binary field operators *)
type fbinop =
  | FDiff

(** Variadic operators *)
type varop =
  | Tuple | Cup | Cap

(** Variadic field operators *)
type fvarop =
  | FCup | FCap

(** Associativity *)
type assoc = Left | Right | NoAssoc

let fs = format_of_string

(** Returns the separator, the priority (as an integer) and the associativity of
    a variadic operator. *)
let varop_info v = match v with
  | Tuple -> fs ",@ ", 0, NoAssoc
  | Cup -> fs "@ |@ ", 2, NoAssoc
  | Cap -> fs "@ &@ ", 3, NoAssoc

(** Same as {!varop_info}, for the variadic operators on field types. *)
let fvarop_info v = match v with
  | FCup -> fs "@ |@ ", 2, NoAssoc
  | FCap -> fs "@ &@ ", 3, NoAssoc

(** Returns the separator, the priority (as an integer) and the associativity of
    a binary operator. *)
let binop_info b = match b with
  | Arrow -> fs "@ ->@ ", 1, Right
  | Diff -> fs "@ \\@ ", 4, Left

(** Same as {!binop_info}, for the binary operators on field types. *)
let fbinop_info b = match b with
  | FDiff -> fs "@ \\@ ", 4, Left

(** Returns the separator, the priority (as an integer) and the associativity of
    a unary operator. *)
let unop_info u = match u with
  | Neg -> fs "~", 5, NoAssoc

(** Same as {!unop_info}, for the unary operators on field types.
    @raise Invalid_argument as there is no syntax for the negation of a field
    type (it is printed as a difference with [any]). *)
let funop_info u = match u with
  | FNeg -> raise (Invalid_argument "no syntax for field negation operator")

(** Maximum priority *)
let max_prec = 100

(** Minimum priority *)
let min_prec = (-1)

(** [need_parentheses lvl assoc info] returns [true] if the operator described
    by [info] needs parentheses for the current printing level [lvl] and the
    current associativity [assoc].
*)
let need_parentheses (prec:int) assoc ((_: _ format4),prec',assoc') =
  prec' < prec || prec' = prec && (assoc' <> assoc || assoc' = NoAssoc)

(** [fprintf lvl assoc info fmt f …] works as [Format.fprintf] but will add
    parentheses around the formatted output if required by [lvl] [assoc] and
    [info].
*)
let fprintf prec assoc opinfo fmt f =
  if need_parentheses prec assoc opinfo
  then Format.fprintf fmt ("("^^f^^")")
  else Format.fprintf fmt f

(** [print_seq f sym fmt l] prints the elements of [l] with [f], separated by
    the format [sym]. *)
let print_seq f sym fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt sym) f fmt l

(** {1 General printing operators}

    All the operators below take a printing function for their operands, the
    current precedence and associativity, and add parentheses when needed.
    In each family:
    - the primed variants ([print_nary'], [print_binary'], …) take one printing
      function per operand instead of a single one shared by all of them;
    - the [_op] (resp. [_fop]) variants take an operator instead of its
      description, and look the latter up in the tables above for the operators
      on types (resp. on field types).
*)

(** [print_atomic_str str] prints [str], ignoring precedence and
    associativity. *)
let print_atomic_str str (_ : int) (_ : assoc) fmt () = Format.fprintf fmt "%s" str

(** [print_nary' fs prec assoc opinfo fmt vs] prints the operands [vs], each
    with its own printing function taken from [fs], separated by the operator
    described by [opinfo].
    @raise Invalid_argument if [vs] is empty. *)
let print_nary' fs prec assoc (sym,prec',_ as opinfo) fmt vs =
  match fs, vs with
  | [], [] -> invalid_arg ("N-ary operator cannot be empty")
  | [ f ], [ v ] -> Format.fprintf fmt "%a" (f prec assoc) v
  | fs, vs ->
    let fvs = List.combine fs vs in
    let f fmt (f,v) = f prec' NoAssoc fmt v in
    fprintf prec assoc opinfo fmt "%a" (print_seq f sym) fvs

(** Same as {!print_nary'}, with a single printing function for all the
    operands. *)
let print_nary f prec assoc opinfo fmt vs =
  let fs = vs |> List.map (Fun.const f) in
  print_nary' fs prec assoc opinfo fmt vs
let print_nary_op' fs prec assoc op fmt vs =
  print_nary' fs prec assoc (varop_info op) fmt vs
let print_nary_op f prec assoc op fmt vs =
  print_nary f prec assoc (varop_info op) fmt vs
let print_nary_fop' fs prec assoc op fmt vs =
  print_nary' fs prec assoc (fvarop_info op) fmt vs
let print_nary_fop f prec assoc op fmt vs =
  print_nary f prec assoc (fvarop_info op) fmt vs

(** [print_unary f prec assoc opinfo fmt v] prints the prefix operator
    described by [opinfo] applied to the operand [v]. *)
let print_unary f prec assoc (sym,prec',_ as opinfo) fmt v =
  fprintf prec assoc opinfo fmt "%(%)%a" sym (f prec' NoAssoc) v
let print_unary_op f prec assoc op fmt v =
  print_unary f prec assoc (unop_info op) fmt v
let print_unary_fop f prec assoc op fmt v =
  print_unary f prec assoc (funop_info op) fmt v

(** [print_binary' f1 f2 prec assoc opinfo fmt v1 v2] prints the operands [v1]
    and [v2], with [f1] and [f2] respectively, separated by the operator
    described by [opinfo]. *)
let print_binary' f1 f2 prec assoc (sym,prec',_ as opinfo) fmt v1 v2 =
  fprintf prec assoc opinfo fmt "%a%(%)%a"
    (f1 prec' Left) v1 sym
    (f2 prec' Right) v2

(** Same as {!print_binary'}, with a single printing function for both
    operands. *)
let print_binary f prec assoc opinfo fmt v1 v2 =
  print_binary' f f prec assoc opinfo fmt v1 v2
let print_binary_op' f1 f2 prec assoc op fmt v1 v2 =
  print_binary' f1 f2 prec assoc (binop_info op) fmt v1 v2
let print_binary_op f prec assoc op fmt v1 v2 =
  print_binary f prec assoc (binop_info op) fmt v1 v2
let print_binary_fop' f1 f2 prec assoc op fmt v1 v2 =
  print_binary' f1 f2 prec assoc (fbinop_info op) fmt v1 v2
let print_binary_fop f prec assoc op fmt v1 v2 =
  print_binary f prec assoc (fbinop_info op) fmt v1 v2

(** {1 Printing of set-theoretic operators}

    Shortcuts for the operators of the type algebra. As above, the [f]-prefixed
    ones work on field types.
*)

let print_cup f prec assoc fmt vs = print_nary_op f prec assoc Cup fmt vs
let print_cap f prec assoc fmt vs = print_nary_op f prec assoc Cap fmt vs
let print_neg f prec assoc fmt v = print_unary_op f prec assoc Neg fmt v
let print_fcup f prec assoc fmt vs = print_nary_fop f prec assoc FCup fmt vs
let print_fcap f prec assoc fmt vs = print_nary_fop f prec assoc FCap fmt vs
let print_fneg f prec assoc fmt v = print_unary_fop f prec assoc FNeg fmt v

(** {1 Auxiliary printing operators} *)

(** [print_lit f prec assoc fmt (pos,a)] prints the atom [a] if [pos], and its
    negation otherwise. *)
let print_lit f prec assoc fmt (pos,a) =
  if pos then f prec assoc fmt a
  else print_neg f prec assoc fmt a

(** [print_line ~any f prec assoc fmt (ps,ns)] prints the intersection of the
    atoms [ps] and of the negations of the atoms [ns]. The string [any] is used
    when [ps] is empty. *)
let print_line ~any f prec assoc fmt (ps,ns) =
  let ps, ns = List.map (fun d -> true, d) ps, List.map (fun d -> false, d) ns in
  let sym,prec',_ as opinfo = varop_info Cap in
  let fprintf, prec =
    if ns <> [] || List.length ps > 1
    then fprintf prec assoc opinfo, prec'
    else Format.fprintf, prec
  in
  fprintf fmt "%s%(%)%a"
    (if ps = [] then any else "")
    (if ps = [] && ns <> [] then sym else "")
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt sym)
       (print_lit f prec NoAssoc))
    (ps@ns)

(** [print_non_empty_dnf ~any f prec assoc fmt dnf] prints the union of the
    lines of [dnf].
    @raise Invalid_argument if [dnf] is empty; use {!print_dnf} otherwise. *)
let print_non_empty_dnf ~any f prec assoc fmt dnf =
  print_cup (print_line ~any f) prec assoc fmt dnf

(** Same as {!print_non_empty_dnf}, but prints the string [empty] when [dnf] is
    empty. *)
let print_dnf ~empty ~any f prec assoc fmt dnf =
  match dnf with
  | [] -> Format.fprintf fmt "%s" empty
  | dnf -> print_non_empty_dnf ~any f prec assoc fmt dnf
