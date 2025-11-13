(* TODO:
   - optimizations: generate labels and atoms only once.
   - translate record to open record on positive occurence
*)

open Mltypes
open Ident
module U = Encodings.Utf8

module IntMap = Map.Make (struct
    type t = int

    let compare : t -> t -> int = compare
  end)

module IntHash = Hashtbl.Make (struct
    type t = int

    let hash i = i
    let equal i j = i == j
  end)

(* Compute CDuce type *)

let vars = ref [||]
let memo_typ = IntHash.create 13
let atom lab = Types.(atom AtomSet.(atom (V.mk_ascii lab)))
let label lab = Label.mk (Ns.empty, U.mk lab)

let bigcup f l =
  let open Cduce_types in
  List.fold_left (fun accu x -> Types.cup accu (f x)) Types.empty l

let ident_to_string list =
  let rec _ident_to_string list res =
    match list with
    | (id, x) :: rest ->
      _ident_to_string rest (res @ [ (Ocaml_common.Ident.name id, x) ])
    | [] -> res
  in
  _ident_to_string list []

let rec typ t =
  try IntHash.find memo_typ t.uid with
  | Not_found ->
    (*    print_int t.uid; print_char ' '; flush stdout; *)
    let node = Cduce_types.Types.make () in
    IntHash.add memo_typ t.uid node;
    Cduce_types.Types.define node (typ_descr t.def);
    node

and typ_descr = function
  | Link t -> typ_descr t.def
  | Arrow (_, t, s) -> Types.arrow (typ t) (typ s)
  | Tuple tl -> Types.tuple (List.map typ tl)
  | PVariant l -> bigcup pvariant l
  | Variant (_, l, _) -> bigcup variant l
  | Record (_, l, _) ->
    let l = ident_to_string l in
    let l = List.map (fun (lab, t) -> (label lab, typ t)) l in
    Types.record_fields (false, LabelMap.from_list_disj l)
  | Builtin ("bool", []) -> Builtin_defs.bool
  | Builtin ("int", []) -> Builtin_defs.caml_int
  | Builtin ("char", []) -> Builtin_defs.char_latin1
  | Builtin ("string", []) -> Builtin_defs.string_latin1
  | Abstract s -> Types.abstract (AbstractSet.atom s)
  | Builtin ("list", [ t ])
  | Builtin ("array", [ t ]) ->
    Types.descr (Types.Sequence.star_node (typ t))
  | Builtin ("Stdlib.ref", [ t ]) -> Builtin_defs.ref_type (typ t)
  | Builtin ("Z.t", []) -> Builtin_defs.int
  | Builtin ("Value.t", []) -> Types.any
  | Builtin ("Cduce_types.Encodings.Utf8.t", []) -> Builtin_defs.string
  | Builtin ("Cduce_types.Atoms.V.t", []) -> Builtin_defs.atom
  | Builtin ("unit", []) -> Types.Sequence.nil_type
  | Builtin ("option", [ t ]) -> Types.Sequence.option (typ t)
  | Builtin ("Stdlib.Seq.t", [ t ]) -> Builtin_defs.seq_type (typ t)
  | Var i -> Types.descr !vars.(i)
  | _ -> assert false

and pvariant = function
  | lab, None -> atom lab
  | lab, Some t -> Types.times (Types.cons (atom lab)) (typ t)

and variant = function
  | lab, [], None -> atom (Ocaml_common.Ident.name lab)
  | lab, [], Some o ->
    Types.tuple
      (Types.cons (atom (Ocaml_common.Ident.name lab)) :: List.map typ [ o ])
  | lab, c, Some o ->
    Types.tuple
      (Types.cons (atom (Ocaml_common.Ident.name lab))
       :: List.map typ (c @ [ o ]))
  | lab, c, None ->
    Types.tuple
      (Types.cons (atom (Ocaml_common.Ident.name lab)) :: List.map typ c)

(* Syntactic tools *)
let var_counter = ref 0

let mk_var _ =
  incr var_counter;
  Printf.sprintf "x%i" !var_counter

let mk_vars = List.map mk_var

module ML = struct
  open Ocaml_common.Ast_helper
  open Ocaml_common

  let lid s = Location.mknoloc (Mlcompat.longident_parse s)

  type arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  let no_label = Obj.magic Nolabel
  let labelled s = Obj.magic (Labelled s)
  let optional s = Obj.magic (Optional s)

  type rec_flag =
    | Nonrecursive
    | Recursive

  let non_rec = Obj.magic Nonrecursive
  let rec_ = Obj.magic Recursive
  let pat_var s = Pat.var (Location.mknoloc s)
  let var_hash = Hashtbl.create 17

  let var s =
    let e = Exp.ident (lid s) in
    Hashtbl.add var_hash e true;
    e

  let is_var e = Hashtbl.mem var_hash e
  let apply e args = Exp.apply e args
  let sapply f args = Exp.apply (var f) (List.map (fun e -> (no_label, e)) args)
  let str s = Exp.constant (Const.string s)
  let str_e s = str (String.escaped s)
  let fun_ x e = Mlcompat.Mlstub.exp_fun_ no_label None (pat_var x) e
  let fun_unit e = Mlcompat.Mlstub.exp_fun_ no_label None (Pat.construct (lid "()") None) e
  let fun_l l x e = Mlcompat.Mlstub.exp_fun_ (labelled l) None (pat_var x) e
  let fun_o l x e = Mlcompat.Mlstub.exp_fun_ (optional l) None (pat_var x) e
  let fun_od l c x e = Mlcompat.Mlstub.exp_fun_ (optional l) (Some c) (pat_var x) e
  let tuple l = Exp.tuple l
  let constr s a = Exp.construct (lid s) a
  let variant s a = Exp.variant s a
  let record fields = Exp.record fields None
  let pconstr s a = Mlcompat.Mlstub.pat_construct (lid s) a
  let pany () = Pat.any ()
  let some e = constr "Some" (Some e)
  let none = constr "None" None
  let cons e1 e2 = constr "::" (Some (tuple [ e1; e2 ]))
  let nil = constr "[]" None
  let unit = constr "()" None
  let true_ = constr "true" None
  let false_ = constr "false" None
  let bool b = if b then true_ else false_
  let pat_tuple l = Pat.tuple l
  let bind p e = Vb.mk p e

  let let_in ?(r = false) pat e1 e2 =
    Exp.let_ (if r then rec_ else non_rec) [ bind pat e1 ] e2

  let pmatch e l = Exp.match_ e l
  let list_list el = List.fold_right (fun a e -> cons a e) el nil

  let protect f e =
    if is_var e then f e
    else
      let x = mk_var () in
      let_in (pat_var x) e (f (var x))

  let int n = Exp.constant (Const.int n)
  let case pat e = Exp.case pat e
  let field e l = Exp.field e l
  let setfield e1 l e2 = Exp.setfield e1 l e2
  let pstr s = Pat.constant (Const.string s)
  let assert_false = Exp.assert_ (constr "false" None)
  let seq e1 e2 = Exp.sequence e1 e2
end

module CD = struct
  let atom_ascii lab = ML.sapply "Value.atom_ascii" [ ML.str_e lab ]
  let label_ascii lab = ML.sapply "Value.label_ascii" [ ML.str_e lab ]
  let pair e1 e2 = ML.sapply "Value.pair" [ e1; e2 ]

  let rec tuple = function
    | [ v ] -> v
    | v :: l -> pair v (tuple l)
    | [] -> assert false

  let rec matches ine oute = function
    | [ v1; v2 ] ->
      ML.let_in
        (ML.pat_tuple [ ML.pat_var v1; ML.pat_var v2 ])
        (ML.sapply "Value.get_pair" [ ine ])
        oute
    | v :: vl ->
      let r = mk_var () in
      let oute = matches (ML.var r) oute vl in
      ML.let_in
        (ML.pat_tuple [ ML.pat_var v; ML.pat_var r ])
        (ML.sapply "Value.get_pair" [ ine ])
        oute
    | [] -> assert false
end

(* Registered types *)

let gen_types = ref true

(* currently always off *)

let registered_types = ref []
let nb_registered_types = ref 0

let register_type t =
  assert !gen_types;
  let _, n =
    try
      List.find (fun (s, i) -> Cduce_types.Types.equiv t s) !registered_types
    with
    | Not_found ->
      let i = !nb_registered_types in
      let kv = (t, i) in
      registered_types := kv :: !registered_types;
      incr nb_registered_types;
      kv
  in
  ML.(sapply "Array.get" [ var "types"; int n ])

let get_registered_types () =
  let a = Array.make !nb_registered_types Types.empty in
  List.iter (fun (t, i) -> a.(i) <- t) !registered_types;
  a

(*
let registered_types = HashTypes.create 13

let nb_registered_types = ref 0

let register_type t =
  assert !gen_types;
  let n =
    try HashTypes.find registered_types t
    with Not_found ->
      let i = !nb_registered_types in
      HashTypes.add registered_types t i;
      incr nb_registered_types;
      i
  in
  ML.(sapply "Array.get" [ var "types"; int n ])

let get_registered_types () =
  let a = Array.make !nb_registered_types Types.empty in
  HashTypes.iter (fun t i -> a.(i) <- t) registered_types;
  a
*)

let is_recursive (t : Mltypes.t) =
  match t.def with
  | Abstract _
  | Builtin (_, []) ->
    false
  | _ -> t.recurs > 0

(* OCaml -> CDuce conversions *)

let to_cd_hash = HashType.create 17
let to_cd_fun_name t = Printf.sprintf "to_cd_%i" t.uid

let rec to_cd_fun t =
  match t.def with
  | Builtin ("bool", []) -> "Value.ocaml2cduce_bool"
  | Builtin ("int", []) -> "Value.ocaml2cduce_int"
  | Builtin ("char", []) -> "Value.ocaml2cduce_char"
  | Builtin ("string", []) -> "Value.ocaml2cduce_string"
  | Builtin ("Z.t", []) -> "Value.ocaml2cduce_bigint"
  | Builtin ("Cduce_types.Encodings.Utf8.t", []) ->
    "Value.ocaml2cduce_string_utf8"
  | Builtin ("Cduce_types.Atoms.V.t", []) -> "Value.ocaml2cduce_atom"
  | Link tt -> to_cd_fun tt
  | _ -> (
      try HashType.find to_cd_hash t with
      | Not_found ->
        let n = to_cd_fun_name t in
        HashType.add to_cd_hash t n;
        n)

let to_ml_hash = HashType.create 17
let to_ml_fun_name t = Printf.sprintf "to_ml_%i" t.uid

let rec to_ml_fun t =
  match t.def with
  | Abstract _ -> "Value.get_abstract"
  | Builtin ("Z.t", []) -> "Value.cduce2ocaml_bigint"
  | Builtin ("bool", []) -> "Value.cduce2ocaml_bool"
  | Builtin ("int", []) -> "Value.cduce2ocaml_int"
  | Builtin ("char", []) -> "Value.cduce2ocaml_char"
  | Builtin ("string", []) -> "Value.cduce2ocaml_string"
  | Builtin ("Cduce_types.Encodings.Utf8.t", []) ->
    "Value.cduce2ocaml_string_utf8"
  | Builtin ("Cduce_types.Atoms.V.t", []) -> "Value.cduce2ocaml_atom"
  | Link tt -> to_ml_fun tt
  | _ -> (
      try HashType.find to_ml_hash t with
      | Not_found ->
        let n = to_ml_fun_name t in
        HashType.add to_ml_hash t n;
        n)

let call_lab f l x =
  if l = "" then ML.apply f [ (ML.no_label, x) ]
  else
    let ll = String.sub l 1 (String.length l - 1) in
    if l.[0] = '?' then ML.apply f [ (ML.optional ll, x) ]
    else ML.apply f [ (ML.labelled ll, x) ]

let abstr_lab l x res =
  if l = "" then ML.fun_ x res
  else
    let ll = String.sub l 1 (String.length l - 1) in
    if l.[0] = '?' then ML.fun_o ll x res else ML.fun_l ll x res

let rec to_cd e t =
  (* Format.fprintf Format.err_formatter "to_cd %a [uid=%i; recurs=%i]@."
     Mltypes.print t t.uid t.recurs; *)
  if t.recurs > 0 then ML.sapply (to_cd_fun t) [ e ] else to_cd_descr e t

and to_cd_descr e t =
  match t.def with
  | Link t -> to_cd e t
  | Arrow (l, t, s) ->
    (* let y = <...> in Value.Abstraction ([t,s], fun x -> s(y ~l:(t(x))) *)
    ML.protect
      (fun y ->
         let x = mk_var () in
         let arg = to_ml (ML.var x) t in
         let res = to_cd (call_lab y l arg) s in
         let abs = abstr_lab "" x res in
         let iface, is_poly =
           if !gen_types then
             let cd_t = Types.descr (typ t) in
             let cd_s = Types.descr (typ s) in
             let tt = register_type cd_t in
             let ss = register_type cd_s in
             ( ML.(some (cons (tuple [ tt; ss ]) nil)),
               ML.bool
                 (not
                    (Var.Set.is_empty (Types.Subst.vars cd_t)
                     && Var.Set.is_empty (Types.Subst.vars cd_s))) )
           else (ML.none, ML.false_)
         in
         ML.constr "Value.Abstraction"
           (Some ML.(tuple [ iface; abs; is_poly ])))
      e
  | Tuple tl ->
    (* let (x1,...,xn) = ... in Value.Pair (t1(x1), Value.Pair(...,tn(xn))) *)
    let vars = mk_vars tl in
    ML.(
      let_in
        (pat_tuple (List.map ML.pat_var vars))
        e
        (CD.tuple (tuple_to_cd tl vars)))
  | PVariant l ->
    (* match <...> with
       | `A -> Value.atom_ascii "A"
       | `B x -> Value.Pair (Value.atom_ascii "B",t(x))
    *)
    let cases =
      List.map
        (function
          | lab, None -> ML.(case (pconstr lab None) (CD.atom_ascii lab))
          | lab, Some t ->
            ML.(
              case
                (pconstr lab (Some (pat_var "x")))
                CD.(pair (atom_ascii lab) (to_cd (var "x") t))))
        l
    in
    ML.pmatch e cases
  | Variant (p, l, _) ->
    (* match <...> with
       | P.A -> Value.atom_ascii "A"
       | P.B (x1,x2,..) -> Value.Pair (Value.atom_ascii "B",...,Value.Pair(tn(x)))
    *)
    let cases =
      List.map
        (fun (lab, args, res) ->
           let lab = Ocaml_common.Ident.name lab in
           match (args, res) with
           | [], None -> ML.(case (pconstr (p ^ lab) None) (CD.atom_ascii lab))
           | tl, Some o ->
             let vars = mk_vars (tl @ [ o ]) in
             ML.(
               case
                 (pconstr (p ^ lab)
                    (Some (pat_tuple (List.map pat_var vars))))
                 CD.(tuple (atom_ascii lab :: tuple_to_cd (tl @ [ o ]) vars)))
           | tl, None ->
             let vars = mk_vars tl in
             ML.(
               case
                 (pconstr (p ^ lab)
                    (Some (pat_tuple (List.map pat_var vars))))
                 CD.(tuple (atom_ascii lab :: tuple_to_cd tl vars))))
        l
    in
    ML.pmatch e cases
  | Record (p, l, _) ->
    (* let x = <...> in Value.record [ l1,t1(x.P.l1); ...; ln,x.P.ln ] *)
    ML.protect
      (fun x ->
         let l =
           List.map
             (fun (lab, t) ->
                let lab = Ocaml_common.Ident.name lab in
                let e = to_cd ML.(field x (lid (p ^ lab))) t in
                ML.tuple [ CD.label_ascii lab; e ])
             l
         in
         ML.sapply "Value.record" [ ML.list_list l ])
      e
  | Builtin ("list", [ t ]) ->
    (* Value.sequence_rev (List.rev_map fun_t <...>) *)
    ML.(
      sapply "Value.sequence_rev"
        [ sapply "Stdlib.List.rev_map" [ var (to_cd_fun t); e ] ])
  | Builtin ("array", [ t ]) ->
    ML.(
      sapply "Value.sequence_rev"
        [
          sapply "Stdlib.List.rev_map"
            [ var (to_cd_fun t); sapply "Stdlib.Array.to_list" [ e ] ];
        ])
  | Builtin ("Stdlib.ref", [ t ]) ->
    (* let x = <...> in
       Value.mk_ext_ref t (fun () -> t(!x)) (fun y -> x := t'(y)) *)
    (* protect e
     * (fun e ->
     *    let y = mk_var () in
     *    let tt = if !gen_types then
     *      let t = register_type (Types.descr (typ t)) in
     *      <:expr< Some $t$ >>
     *    else
     *      <:expr< None >> in
     *    let get_x = <:expr< $e$.val >> in
     *    let get = <:expr< fun () -> $to_cd get_x t$ >> in
     *    let tr_y = to_ml <:expr< $lid:y$ >> t in
     *    let set = <:expr< fun $lid:y$ -> $e$.val := $tr_y$ >> in
     *    <:expr< Value.mk_ext_ref $tt$ $get$ $set$ >>
     * ) *)
    ML.(
      protect
        (fun e ->
           let tt =
             if !gen_types then
               let t = register_type (Types.descr (typ t)) in
               some t
             else none
           in
           let get_x = field e (lid "contents") in
           let get = fun_unit (to_cd get_x t) in
           let y = mk_var () in
           let tr_y = to_ml (var y) t in
           let set = fun_ y (setfield e (lid "contents") tr_y) in
           ML.sapply "Value.mk_ext_ref" [ tt; get; set ])
        e)
  | Builtin ("unit", []) -> ML.(let_in (pany ()) e (var "Value.nil"))
  | Builtin ("Stdlib.Seq.t", [ t ]) ->
    ML.(sapply "Value.ocaml2cduce_seq" [ var (to_cd_fun t); e ])
  | Builtin ("option", [ t ]) ->
    ML.sapply "Value.ocaml2cduce_option" [ ML.var (to_cd_fun t); e ]
  | Builtin ("Value.t", []) -> e
  | Abstract s -> ML.sapply "Value.abstract" [ ML.str_e s; e ]
  | Var _ -> e
  (* the remaining cases are handled by to_cd_fun *)
  | Builtin (_, []) -> ML.sapply (to_cd_fun t) [ e ]
  | _ -> assert false

and tuple_to_cd tl vars = List.map2 (fun t id -> to_cd (ML.var id) t) tl vars

(* CDuce -> OCaml conversions *)
and to_ml e (t : Mltypes.t) =
  (*Format.fprintf Format.err_formatter "to_ml %a@."
    Mltypes.print t;*)
  if is_recursive t then ML.sapply (to_ml_fun t) [ e ] else to_ml_descr e t

and to_ml_descr e t =
  match t.def with
  | Link t -> to_ml e t
  | Arrow (l, t, s) ->
    (* let y = <...> in fun ~l:x -> s(Eval.eval_apply y (t(x))) *)
    ML.protect
      (fun y ->
         let x = mk_var () in
         let arg = to_cd (ML.var x) t in
         let res = to_ml (ML.sapply "Eval.eval_apply" [ y; arg ]) s in
         abstr_lab l x res)
      e
  | Tuple tl ->
    (* let (x1,r) = Value.get_pair <...> in
              let (x2,r) = Value.get_pair r in
              ...
              let (xn-1,xn) = Value.get_pair r in
       (t1(x1),...,tn(xn)) *)
    let vars = mk_vars tl in
    CD.(matches e (tuple_to_ml tl vars) vars)
  | PVariant l ->
    (* match Value.get_variant <...> with
       | "A",None -> `A
       | "B",Some x -> `B (t(x))
       | _ -> assert false
    *)
    let cases =
      List.map
        (function
          | lab, None ->
            ML.(
              case
                (pat_tuple [ ML.pstr lab; ML.pconstr "None" None ])
                (variant lab None))
          | lab, Some t ->
            let x = mk_var () in
            ML.(
              case
                (pat_tuple
                   [ ML.pstr lab; ML.pconstr "Some" (Some (pat_var x)) ])
                (variant lab (Some (to_ml (var x) t)))))
        l
    in
    let cases = cases @ [ ML.(case (pany ()) assert_false) ] in
    ML.pmatch (ML.sapply "Value.get_variant" [ e ]) cases
  | Variant (_, l, false) -> failwith "Private Sum type"
  | Variant (p, l, true) ->
    let cases =
      List.map
        (fun (lab, args, res) ->
           let lab = Ocaml_common.Ident.name lab in
           let ml_lab = p ^ lab in
           match (args, res) with
           | [], None ->
             ML.(
               case
                 (pat_tuple [ ML.pstr lab; ML.pconstr "None" None ])
                 (constr ml_lab None))
           | [ t ], None
           | [], Some t ->
             let x = mk_var () in
             ML.(
               case
                 (pat_tuple
                    [ ML.pstr lab; ML.pconstr "Some" (Some (pat_var x)) ])
                 (constr ml_lab (Some (to_ml (var x) t))))
           | tl, o ->
             let tl =
               match o with
               | None -> tl
               | Some o -> tl @ [ o ]
             in

             (* `B, (v1, (v2, (..., vn))) =>
                let x1, r = x in
                let x2, r = r in
                ..
                let xn-1, xn = r in
                B(t(x1), (t(x2)), …   t(xn))
             *)
             let vars = mk_vars tl in
             let x = mk_var () in
             ML.(
               case
                 (pat_tuple
                    [ ML.pstr lab; ML.pconstr "Some" (Some (pat_var x)) ])
                 (CD.matches (var x)
                    (let res_tuple =
                       List.fold_right
                         (fun (ti, xi) acc -> to_ml (var xi) ti :: acc)
                         (List.combine tl vars) []
                     in
                     constr ml_lab (Some (tuple res_tuple)))
                    vars)))
        l
    in
    let cases = cases @ [ ML.(case (pany ()) assert_false) ] in
    ML.pmatch (ML.sapply "Value.get_variant" [ e ]) cases
  | Record (_, l, false) -> failwith "Private Record type"
  | Record (p, l, true) ->
    (* (\* let x = <...> in
     *    { P.l1 = t1(Value.get_field x "l1"); ... } *\)
     * protect e
     *   (fun x ->
     *      let l =
     *        List.map
     *          (fun (lab,t) ->
     *             let lab = (Ocaml_common.Ident.name lab) in
     *             let e =
     *               to_ml <:expr< Value.get_field $x$ $label_ascii lab$ >> t in
     *             <:rec_binding< $id: consId (p^lab)$ = $e$ >>) l in
     *      <:expr< {$list:l$} >>) *)
    ML.(
      protect
        (fun e ->
           let l =
             List.map
               (fun (lab, t) ->
                  let lab = Ocaml_common.Ident.name lab in
                  let e =
                    to_ml (sapply "Value.get_field" [ e; CD.label_ascii lab ]) t
                  in
                  (lid (p ^ lab), e))
               l
           in
           record l)
        e)
  | Builtin ("list", [ t ]) ->
    (* List.rev_map fun_t (Value.get_sequence_rev <...> *)
    ML.sapply "Stdlib.List.rev_map"
      [ ML.var (to_ml_fun t); ML.sapply "Value.get_sequence_rev" [ e ] ]
  | Builtin ("array", [ t ]) ->
    ML.sapply "Stdlib.Array.of_list"
      [
        ML.sapply "Stdlib.List.rev_map"
          [ ML.var (to_ml_fun t); ML.sapply "Value.get_sequence_rev" [ e ] ];
      ]
  | Builtin ("Stdlib.ref", [ t ]) ->
    let f = ML.sapply "Value.get_field" [ e; CD.label_ascii "get" ] in
    let e = ML.sapply "Eval.eval_apply" [ f; ML.var "Value.nil" ] in
    ML.sapply "Stdlib.ref" [ to_ml e t ]
  | Builtin ("Value.t", []) -> e
  | Builtin ("unit", []) -> ML.(let_in (pany ()) e unit)
  | Builtin ("option", [ t ]) ->
    ML.sapply "Value.cduce2ocaml_option" [ ML.var (to_ml_fun t); e ]
  | Builtin ("Stdlib.Seq.t", [ t ]) ->
    ML.sapply "Value.cduce2ocaml_seq" [ ML.var (to_ml_fun t); e ]
  | Var _ -> e
  (* Other cases handled by to_ml_fun *)
  | Abstract _s -> ML.sapply (to_ml_fun t) [ e ]
  | Builtin (_, []) -> ML.sapply (to_ml_fun t) [ e ]
  | _ -> assert false

and tuple_to_ml tl vars =
  ML.tuple (List.map2 (fun t id -> to_ml (ML.var id) t) tl vars)

let global_transl () =
  let defs = ref [] in
  let gen_binding tbl to_descr =
    let l = tbl |> HashType.to_seq |> List.of_seq in
    HashType.clear tbl;
    List.iter
      (fun (t, fun_name) ->
         let p = ML.pat_var fun_name in
         let e = ML.(fun_ "x" (to_descr (var "x") t)) in
         defs := ML.bind p e :: !defs)
      l
  in
  while HashType.length to_cd_hash != 0 || HashType.length to_ml_hash != 0 do
    gen_binding to_cd_hash to_cd_descr;
    gen_binding to_ml_hash to_ml_descr
  done;
  !defs

(*
  let defs = ref [] in
  let rec aux hd tl gen don fun_name to_descr =
    gen := tl;
    if not (HashType.mem don hd) then (
        HashType.add don hd ();
      let p = ML.pat_var (fun_name hd) in
      let e = ML.(fun_ "x" (to_descr (var "x") hd)) in
      defs := ML.bind p e :: !defs);
    loop ()
  and loop () =
    match (!to_cd_gen, !to_ml_gen) with
    | hd :: tl, _ -> aux hd tl to_cd_gen to_cd_done to_cd_fun_name to_cd_descr
    | _, hd :: tl -> aux hd tl to_ml_gen to_ml_done to_ml_fun_name to_ml_descr
    | [], []      -> ()
  in
  loop ();
  !defs
*)
(* Check type constraints and generate stub code *)

let err_ppf = Format.err_formatter
let global_exts = ref []

let check_value ty_env c_env (s, caml_t, t) =
  (* Find the type for the value in the CDuce module *)
  let id = (Ns.empty, U.mk s) in
  let vt =
    try Cduce_core.Typer.find_value id ty_env with
    | Not_found ->
      Format.fprintf err_ppf
        "The interface exports a value %s which is not available in the \
         module@."
        s;
      exit 1
  in
  (* Compute expected CDuce type *)
  let et = Types.descr (typ t) in

  (* Check subtyping *)
  if not (Types.subtype vt et) then (
    Format.fprintf err_ppf
      "The type for the value %s is invalid@\n\
       Expected Caml type:@[%a@]@\n\
       Expected CDuce type:@[%a@]@\n\
       Inferred type:@[%a@]@." s print_ocaml caml_t Types.Print.print et
      Types.Print.print vt;
    exit 1);

  (* Generate stub code *)
  let x = mk_var () in
  let slot = Cduce_core.Compile.find_slot id c_env in
  let e = to_ml ML.(sapply "Stdlib.Array.get" [ var "slots"; int slot ]) t in
  ML.(pat_var s, var ("C." ^ x), bind (pat_var x) e)

(*
module Cleaner = Camlp4.Struct.CleanAst.Make(Ast)

let cleaner = object
  inherit Cleaner.clean_ast as super
  method str_item st =
    match super#str_item st with
      | <:str_item< value $rec:_$ $ <:binding< >> $ >> ->
        <:str_item< >>
      | x -> x
end
*)

let stub binary name ty_env c_env exts values mk prolog =
  gen_types := false;
  let items = List.map (check_value ty_env c_env) values in

  let exts = List.rev_map (fun (s, t) -> to_cd ML.(var s) t) exts in
  let g = global_transl () in

  let types = get_registered_types () in
  let raw = mk types in

  let items_def = List.map (fun (_, _, d) -> d) items in
  let items_expr = List.map (fun (_, e, _) -> e) items in
  let items_pat = List.map (fun (p, _, _) -> p) items in

  let str_items =
    let open Ocaml_common.Ast_helper in
    Str.value ML.non_rec
      [
        ML.(
          bind (pat_tuple items_pat)
            Exp.(
              letmodule
                (Ocaml_common.Location.mknoloc (Mlcompat.Mlstub.noloc "C"))
                (Mod.structure
                   [
                     Mlcompat.Mlstub.str_open (lid "Cduce_lib");
                     Str.eval (sapply "Cduce_config.init_all" [ unit ]);
                     Str.value non_rec
                       [
                         bind
                           (pat_tuple
                              [
                                pat_var "types";
                                pat_var "set_externals";
                                pat_var "slots";
                                pat_var "run";
                              ])
                           (sapply "Librarian.ocaml_stub" [ str raw ]);
                       ];
                     Str.value rec_ g;
                     Str.eval (sapply "set_externals" [ Exp.array exts ]);
                     Str.eval (sapply "run" [ unit ]);
                     Str.value non_rec items_def;
                   ])
                (tuple items_expr)));
      ]
  in
  let str_prolog =
    (* The prolog is only type declarations as returned
       by read_cmi *)
    Parse.implementation (Lexing.from_string prolog)
  in
  try
    let structure = str_prolog @ [ str_items ] in
    if binary then begin
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout name;
      output_value stdout structure;
      flush stdout
    end
    else Format.printf "%!%a\n%!" Ocaml_common.Pprintast.structure structure
  (* Printers.OCaml.print_implem (cleaner # str_item str_items) *)
  with
  | exn ->
    Format.printf "@.";
    raise exn

(* let exe = Filename.concat (Filename.dirname Sys.argv.(0)) "cdo2ml" in
   let oc = Unix.open_process_out exe in
   Marshal.to_channel oc str_items [];
   flush oc;
   ignore (Unix.close_process_out oc) *)

let stub_ml binary filename name ty_env c_env exts mk =
  let name = String.capitalize_ascii name in
  let exts =
    match (Obj.magic exts : (string * string * int * Mltypes.t) list option) with
    | None -> []
    | Some exts ->
      List.map (fun (_, s, _, t) -> Mltypes.reg_uid t; (s,t)) exts
  in
  (* First, read the description of ML types for externals.
     Don't forget to call reg_uid to avoid uid clashes...
     Do that before reading the cmi. *)
  let prolog, values =
    try Mltypes.read_cmi name with
    | Not_found -> ("", [])
  in
  stub binary filename ty_env c_env exts values mk prolog

let mk_poly_vars n =
  let rec loop i acc =
    if i = n then
      List.rev
        (Var.Map.fold
           (fun _ v acc -> v :: acc)
           (Var.full_renaming (Var.Set.from_list acc))
           [])
    else loop (i + 1) (Var.mk ("a" ^ string_of_int i) :: acc)
  in
  List.map (fun x -> Types.(cons (var x))) (loop 0 [])

let find_value s =
  try s, Mltypes.find_value s with
  | Not_found -> (
      match String.split_on_char '.' s with
      | prefix :: rest ->
        if Cduce_core.Librarian.has_virtual_prefix prefix then
          let s = String.concat "." rest in
          s, Mltypes.find_value s
        else raise Not_found
      | _ -> raise Not_found)

let register b vs args =
  try
    let s, (t, n) = find_value vs in
    let m = List.length args in
    let args = if m = 0 && n != 0 then mk_poly_vars n else args in
    let m = List.length args in
    if n <> m then
      Cduce_core.Cduce_error.(raise_err
                                Generic (Printf.sprintf
                                           "Wrong arity for external symbol %s (real arity = %i; given = %i)" s
                                           n m));
    let i =
        let i = List.length !global_exts  in
        global_exts := (vs, s, i, t) :: !global_exts;
        i
    in
    vars := Array.of_list args;
    let cdt = Types.descr (typ t) in
    vars := [||];
    (i, cdt)
  with
  | Not_found ->
    Cduce_core.Cduce_error.(raise_err
                              Generic (Printf.sprintf "Cannot resolve ocaml external %s" vs))

(* Generation of wrappers *)

let wrapper values =
  gen_types := false;
  let open Ocaml_common.Ast_helper in
  let exts =
    List.rev_map
      (fun ((prefix, s), t) ->
         let v = to_cd (ML.var s) t in
         ML.(
           sapply "Librarian.register_static_external" [ str_e (prefix ^ s); v ]))
      values
  in
  let load_paths =
    List.rev_map (fun s ->
        ML.(sapply "Cduce_loc.add_to_obj_path" [str_e s])
      ) (Cduce_core.Cduce_loc.get_obj_path ())
  in
  let g =
    match global_transl () with
    | [] -> ML.[ bind (pany ()) unit ]
    | g -> g
  in
  let g =
    ML.(bind (pat_var "___dummy") (fun_ "x" (sapply "___dummy" [ var "x" ])))
    :: g
  in
  ML.(
    fun_unit
      (seq
         (sapply "Cduce_config.init_all" [ unit ])
         (Exp.let_ rec_ g
            (List.fold_left
               (fun acc e -> seq e acc)
               unit (load_paths@exts)))))
(*   Mlcompat.Mlstub.str_open (lid "Cduce_lib"); *)

let gen_wrapper vals =
  let values =
    List.fold_left
      (fun accu (prefix, s) ->
         try ((prefix, s), fst (Mltypes.find_value s)) :: accu with
         | Not_found ->
           let vals =
             try
               List.map
                 (fun (s, v) -> ((prefix, s), v))
                 (Mltypes.load_module s)
             with
             | Not_found -> failwith ("Cannot resolve " ^ s)
           in
           vals @ accu)
      [] vals
  in
  wrapper values

let prefix_ok p =
  let l = String.length p in
  (l > 0 && p.[0] >= 'A' && p.[0] <= 'Z')
  &&
  (for i = 1 to l - 1 do
     match p.[i] with
     | 'A' .. 'Z'
     | 'a' .. 'z'
     | '0' .. '9'
     | '_' ->
       ()
     | _ -> failwith ("Error in primitive file: invalid prefix '" ^ p ^ "'")
   done;
   true)

let prefixes = Hashtbl.create 16

let make_wrapper binary fn =
  let ic = open_in fn in
  let v = ref [] in
  (try
     while true do
       let s = input_line ic in
       if s <> "" then
         match s.[0] with
         | 'A' .. 'Z' -> begin
             match String.split_on_char '!' s with
             | [ s ] -> v := ("", s) :: !v
             | [ prefix; s ] when prefix_ok prefix ->
               Hashtbl.replace prefixes prefix true;
               v := (prefix ^ "!", s) :: !v
             | _ ->
               failwith ("Error in primitive file: invalid name '" ^ s ^ "'")
           end
         | '#' -> ()
         | _ ->
           failwith
             "Error in primitive file: names must start with a capitalized \
              letter"
     done
   with
   | End_of_file -> ());
  let s = gen_wrapper !v in
  let epilogue =
    let open Ast_helper in
    [
      Mlcompat.Mlstub.str_open (ML.lid "Cduce_lib");
      Str.eval
        (Exp.setfield
           (Exp.ident (ML.lid "Cduce_lib.Run.external_init"))
           (ML.lid "contents") (ML.some s));
      Str.eval (ML.sapply "Cduce_lib.Run.main" [ ML.unit ]);
    ]
  in

  let structure = epilogue in
  if binary then begin
    output_string stdout Config.ast_impl_magic_number;
    output_value stdout fn;
    output_value stdout structure;
    flush stdout
  end
  else Format.printf "%!%a\n%!" Ocaml_common.Pprintast.structure structure

(* Dynamic coercions *)

(*
let to_cd_dyn = function
  | Link t -> to_cd_dyn e t
  | Arrow (l,t,s) ->
      let tt = Types.descr (typ t) in
      let ss = Types.descr (typ s) in
      let tf = to_ml_dyn t in
      let sf = to_cd_dyn t in
      (fun (f : Obj.repr) ->
  let f = (Obj.magic f : Obj.repr -> Obj.repr) in
  Value.Abstraction ([tt,ss],fun x -> sf (f (tf x))))
  | Tuple tl ->
      let fs = List.map to_cd_dyn tl in
      (fun (x : Obj.repr) ->
  let x = (Obj.magic x : Obj.repr array) in
  let rec aux i = function
    | [] -> assert false
    | [f] -> f x.(i)
    | f::tl -> Value.Pair (f x.(i), aux (succ i) tl) in
  aux 0 fs)
*)

let use () =
  let open Cduce_core in
  (Typer.has_ocaml_unit :=
     fun cu ->
       let s = U.get_str cu in
       Librarian.has_virtual_prefix s
       || ((not (Librarian.exists_with_prefix s)) && Mltypes.has_cmi s));
  Librarian.stub_ml := stub_ml;
  Externals.register := register;
  (Externals.ext_info := fun () -> Obj.magic !global_exts);
  Librarian.make_wrapper := make_wrapper

let () = Cduce_core.Cduce_config.register "ocaml" "OCaml interface" use
