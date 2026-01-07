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

let fs = format_of_string

(** Returns the separator, the priority (as an integer) and the associativity of
    a variadic operator. *)
let varop_info v = match v with
  | Tuple -> fs ",@ ", 0, NoAssoc
  | Cup -> fs "@ |@ ", 2, NoAssoc
  | Cap -> fs "@ &@ ", 3, NoAssoc

(** Returns the separator, the priority (as an integer) and the associativity of
    a binary operator. *)
let binop_info b = match b with
  | Arrow -> fs "@ ->@ ", 1, Right
  | Diff -> fs "@ \\@ ", 4, Left

(** Returns the separator, the priority (as an integer) and the associativity of
    a unary operator. *)
let unop_info u = match u with
  | Neg -> fs "~", 5, NoAssoc

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

let print_seq f sym fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt sym) f fmt l

let print_nary_op name f prec assoc op fmt vs =
  match vs with
  | [] -> invalid_arg (name ^ " cannot be empty")
  | [ v ] -> Format.fprintf fmt "%a" (f prec assoc) v
  | vs ->
    let sym,prec',_ as opinfo = varop_info op in
    fprintf prec assoc opinfo fmt "%a"
      (print_seq (f prec' NoAssoc) sym) vs

let print_cup f prec assoc fmt vs = print_nary_op "Union" f prec assoc Cup fmt vs

let print_cap f prec assoc fmt vs = print_nary_op "Intersection" f prec assoc Cup fmt vs

let print_neg f prec assoc fmt v =
  let sym,prec',_ as opinfo = unop_info Neg in
  fprintf prec assoc opinfo fmt "%(%)%a" sym (f prec' NoAssoc) v

let print_lit f prec assoc fmt (pos,a) =
  if pos then f prec assoc fmt a
  else print_neg f prec assoc fmt a

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

let print_non_empty_dnf ~any f prec assoc fmt dnf =
  print_cup (print_line ~any f) prec assoc fmt dnf

let print_dnf ~empty ~any f prec assoc fmt dnf =
  match dnf with
  | [] -> Format.fprintf fmt "%s" empty
  | dnf -> print_non_empty_dnf ~any f prec assoc fmt dnf
