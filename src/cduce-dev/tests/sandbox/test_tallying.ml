open Cduce_types
open Cduce_lib

let () = Format.set_margin 200

let parse_type map str =
  match Parse.pat (String.to_seq str) with
  | exception _ -> Types.empty
  | p -> Types.descr (Typer.var_typ map Builtin.env p)

let mk_map l = List.map (fun s -> (Ast.U.mk s, Var.mk s)) l
let map = mk_map [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ]
let t = parse_type map "('a & 'b)\\[]"
let atoms = Types.Atom.get_vars t
let () = Sys.argv.(0) <- "DEBUG"
let u = Types.cup t Builtin_defs.nil
let uatoms = Types.Atom.get_vars u
let () = Format.eprintf "%a@\n" Types.Atom.Dnf.dump atoms
let () = Format.eprintf "%a@\n" Types.Atom.Dnf.dump uatoms

(*****)
let map = mk_map [ "i "; "c"; "a"; "b"; "h"; "f"; "g"; "d"; "e" ]
let var_order = [] (*List.map snd map*)
let t = parse_type map "(('a -> 'b) -> X1) -> X1 where X1 = ('a -> 'b) & 'c"

let s =
  parse_type map
    "(([ Any* ] & 'd -> [ 'e* ]) & ('f -> [ 'g* ]) -> ([ 'f Any* ] & 'h -> [ \
     'g* 'e* ]) &('h \\ [ Any* ] -> [ 'h \\ [ Any* ] ]) & ([  ] & 'h -> [  ])) \
     & (Any -> ([  ] & 'h -> [  ]) &('h \\ [ Any* ] -> [ 'h \\ [ Any* ] ])) -> \
     'i"

let () = Format.eprintf "@[%a@]@\n" Types.Print.print t
let () = Format.eprintf "@[%a@]@\n" Types.Print.print s
let delta = Var.Set.empty
let sl = Types.Tallying.tallying ~var_order delta [ (t, s) ]

let () =
  List.iter
    (fun subst ->
      let tt = Types.Subst.apply subst t in
      let ss = Types.Subst.apply subst s in
      let res = Types.subtype tt ss in
      Format.eprintf "Using substitution:@\n@[%a@]@\n" Types.Subst.print subst;
      Format.eprintf "@[%a@] @\n%s a subtype of@\n@[%a@]@\n" Types.Print.print
        tt
        (if res then "is" else "is NOT")
        Types.Print.print ss)
    sl
