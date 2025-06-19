type unop =
| Neg
type binop =
| Diff | Arrow
type varop =
| Tuple | Cup | Cap

type assoc = Left | Right | NoAssoc

let varop_info v = match v with
| Tuple -> ", ", 0, NoAssoc
| Cup -> " | ", 2, NoAssoc
| Cap -> " & ", 3, NoAssoc

let binop_info b = match b with
| Arrow -> " -> ", 1, Right
| Diff -> " \\ ", 4, Left

let unop_info u = match u with
| Neg -> "~", 5, NoAssoc

let max_prec = 100
let min_prec = (-1)

let need_parentheses prec assoc (_,prec',assoc') =
  prec' < prec || prec' = prec && (assoc' <> assoc || assoc' = NoAssoc)

let fprintf prec assoc opinfo fmt f =
  if need_parentheses prec assoc opinfo
  then Format.fprintf fmt ("("^^f^^")")
  else Format.fprintf fmt f