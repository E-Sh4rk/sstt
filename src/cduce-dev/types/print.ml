module U = Encodings.Utf8

let any_node = Types.(cons any)

module Print = struct
  open Ident
  open Types

  let rec print_const ppf = function
    | Integer i -> Intervals.V.print ppf i
    | Atom a -> AtomSet.V.print_quote ppf a
    | Char c -> CharSet.V.print ppf c
    | Pair (x, y) -> Format.fprintf ppf "(%a,%a)" print_const x print_const y
    | Xml (x, y) -> Format.fprintf ppf "XML(%a,%a)" print_const x print_const y
    | Record r ->
        Format.fprintf ppf "Record{";
        LabelMap.iteri
          (fun l c ->
            Format.fprintf ppf "%a : %a; " Label.print_attr l print_const c)
          r;
        Format.fprintf ppf "}"
    | String (i, j, s, c) ->
        Format.fprintf ppf "\"%a\" %a" U.print
          (U.mk (U.get_substr s i j))
          print_const c

  let nil_atom = AtomSet.V.mk_ascii "nil"
  let nil_type = atom (AtomSet.atom nil_atom)

  let _seqs_node, seqs_descr =
    let n = make () in
    let d = cup nil_type (times any_node n) in
    define n d;
    (n, d)

  type gname = string * Ns.QName.t

  and nd = {
    id : int;
    mutable def : d list;
    mutable state :
      [ `Expand
      | `None
      | `Marked
      | `GlobalName of gname * Types.t list
      | `Named of U.t
      ];
  }

  and d =
    | Name of gname * nd list
    | Var of Var.t
    | Display of string
    | Regexp of nd Pretty.regexp
    | Atomic of (Format.formatter -> unit)
    | Interval of Intervals.t
    | Pair of nd * nd
    | Char of CharSet.V.t
    | Xml of [ `Tag of Format.formatter -> unit | `Type of nd ] * nd * nd
    | Record of (bool * nd) label_map * bool * bool
    | Arrows of (nd * nd) list * (nd * nd) list
    | Diff of nd * nd
    | Intersection of nd list
    | Neg of nd
    | Abs of nd

  let[@ocaml.warning "-32"] compare x y = x.id - y.id

  module S = struct
    type t = nd

    let compare x y = x.id - y.id
    let hash x = x.id
    let equal x y = x.id = y.id
  end

  module DescrHash = Hashtbl.Make (Types)
  module DescrMap = Map.Make (Types)
  module DescrPairMap = Map.Make (Custom.Pair (Types) (Types))
  module Decompile = Pretty.Decompile (DescrHash) (S)

  (*
  let memo = ref Cache.emp

  let uniq t =
    let c', r = Cache.find (fun t -> t) t !memo in
    memo := c';
    r

  let lookup t =
    match Cache.lookup t !memo with
    | Some t -> t
    | None -> t
  *)
  type memo = {
    fast : t DescrHash.t;
    mutable slow : t list;
  }

  let memo = { fast = DescrHash.create 16; slow = [] }

  let uniq t =
    let rec loop l =
      match l with
      | [] ->
          memo.slow <- t :: memo.slow;
          t
      | r :: ll -> if Types.equiv r t then r else loop ll
    in
    try DescrHash.find memo.fast t with
    | Not_found ->
        let r = loop memo.slow in
        DescrHash.replace memo.fast t r;
        r

  let lookup t = uniq t
  let named = ref DescrMap.empty
  let named_xml = ref DescrPairMap.empty

  let register_global cu (name : Ns.QName.t) ?(params = []) d =
    let d = uniq d in
    let params = List.map uniq params in
    (if equal Xml.(update d Dnf.empty) empty then
     let l = (*Product.merge_same_2*) Product.get ~kind:`XML d in
     match l with
     | [ (t1, t2) ] ->
         if DescrPairMap.mem (t1, t2) !named_xml then ()
         else
           named_xml :=
             DescrPairMap.add (t1, t2) ((cu, name), params) !named_xml
     | _ -> ());
    if DescrMap.mem d !named then ()
    else named := DescrMap.add d ((cu, name), params) !named

  let unregister_global d =
    let d = uniq d in
    (if equal Xml.(update d Dnf.empty) empty then
     let l = Product.get ~kind:`XML d in
     match l with
     | [ (t1, t2) ] -> named_xml := DescrPairMap.remove (t1, t2) !named_xml
     | _ -> ());
    named := DescrMap.remove d !named

  let memo = DescrHash.create 63
  let counter = ref 0

  let alloc def =
    {
      id =
        (incr counter;
         !counter);
      def;
      state = `None;
    }

  let count_name = ref 0

  let name () =
    incr count_name;
    U.mk ("X" ^ string_of_int !count_name)

  let to_print = ref []

  let all_kinds : (module Kind) list =
    [
      (module Atom);
      (module Char);
      (module Int);
      (module Times);
      (module Xml);
      (module Function);
      (module Rec);
      (module Abstract);
    ]

  let trivial (module K : Kind) b =
    let b = K.mk (K.get_vars b) in
    is_empty b || equiv b K.any

  let worth_abbrev d =
    not
      (trivial (module Times) d
      && trivial (module Xml) d
      && trivial (module Function) d
      && trivial (module Rec) d)

  let worth_complement d =
    let aux (module K : Kind) x =
      if equiv (K.mk (K.get_vars x)) K.any then 1 else 0
    in
    let n = List.fold_left (fun acc m -> acc + aux m d) 0 all_kinds in
    n >= 5

  let bool_type =
    atom AtomSet.(cup (atom (V.mk_ascii "true")) (atom (V.mk_ascii "false")))

  (* Pretty printers for each kind with special cases for sequences and some
     important types *)

  exception Variable_in_regexp

  let print_seq finite_atoms decompile acc tt =
    let tt_times = Times.(mk (get_vars tt)) in
    if subtype tt_times seqs_descr then
      let seq = cap tt seqs_descr in
      let seq_times = Times.(mk (get_vars seq)) in
      if is_empty seq || (is_empty seq_times && not finite_atoms) then (acc, tt)
      else
        let ntt =
          let d = diff tt seqs_descr in
          if finite_atoms then d else cup d (cap tt nil_type)
        in
        try (Regexp (decompile seq) :: acc, ntt) with
        | Variable_in_regexp -> ([], tt)
    else ([], tt)

  let print_chars acc tt =
    List.fold_right
      (fun (a, b) acc ->
        let d =
          if CharSet.V.equal a b then Char a
          else if CharSet.V.(to_int a == 0 && to_int b = 0x10ffff) then
            Display "Char"
          else
            Atomic
              (fun ppf ->
                Format.fprintf ppf "%a--%a" CharSet.V.print a CharSet.V.print b)
        in
        d :: acc)
      (CharSet.extract (Char.get tt))
      acc

  let print_ints acc tt =
    if Intervals.is_empty (Int.get tt) then acc
    else Interval (Int.get tt) :: acc

  let print_atoms acc tt =
    (* We need this complex bit because Atoms.print does not know about
       the precedence of outer operators *)
    let pr_atoms acc l =
      List.fold_left
        (fun acc (ns, atm) ->
          (match atm with
          | `Finite l ->
              List.map
                (fun a -> Atomic (fun ppf -> AtomSet.V.print_quote ppf a))
                l
          | `Cofinite [] ->
              [
                Atomic
                  (fun ppf ->
                    Format.fprintf ppf "`%a" Ns.InternalPrinter.print_any_ns ns);
              ]
          | `Cofinite l ->
              [
                Diff
                  ( alloc
                      [
                        Atomic
                          (fun ppf ->
                            Format.fprintf ppf "`%a"
                              Ns.InternalPrinter.print_any_ns ns);
                      ],
                    alloc
                      (List.map
                         (fun a ->
                           Atomic (fun ppf -> AtomSet.V.print_quote ppf a))
                         l) );
              ])
          @ acc)
        acc l
    in
    match AtomSet.extract (Atom.get tt) with
    | `Finite l -> pr_atoms acc l
    | `Cofinite [] -> Display "Atom" :: acc
    | `Cofinite l ->
        Diff (alloc [ Display "Atom" ], alloc (pr_atoms [] l)) :: acc

  (** [prepare d] massages a type and convert it to the syntactic form.
  Rough algorithm:
  - check whether [d] has been memoized (recursive types)
  - check whether [d] has a toplevel name
  - check whether [d] may be absent (as part of a record field)
  - check whether [d] needs to be expanded (i.e. isn't a trivially
  empty or full pair or record
  - for each kind (Atoms, Integers, Chars, Products, …) composing the type:
    - Check whether the type is worth complementing (that is write
      (Any \ Int) rather than (Arrow | Char | Atoms | ...)
      - special case for products and atoms:
        - products that are sequence types are written as regular expressions
        - if an atomic type is finite and contains the atoms `false and `true
          then write it has Bool.
*)

  module VarKey = struct
    include Custom.Pair (Var.Set) (Var.Set)

    let empty = (Var.Set.empty, Var.Set.empty)
  end

  module VarTable = Hashtbl.Make (VarKey)

  let inter_d l =
    match l with
    | [] -> Neg (alloc [])
    | [ p ] -> p
    | [ p; Neg { def = []; _ } ] -> p
    | _ -> Intersection (List.map (fun x -> alloc [ x ]) l)

  let inter_nd l =
    match l with
    | [] -> Neg (alloc [])
    | [ { def = [ p ]; _ } ] -> p
    | _ -> Intersection l

  let _diff_nd n1 n2 =
    match (n1, n2) with
    | { def = []; _ }, { def = []; _ } -> []
    | _, { def = []; _ } -> n1.def
    | { def = []; _ }, _ -> [ Neg n2 ]
    | _ -> [ Diff (n1, n2) ]

  let rec prepare d =
    let d = lookup d in
    try DescrHash.find memo d with
    | Not_found -> (
        try
          let gname, params = DescrMap.find d !named in
          named := DescrMap.remove d !named;
          (* break a cycle for named types occuring in their parameters*)
          let s = alloc [] in
          s.state <- `GlobalName (gname, params);
          s.def <- [ Name (gname, List.map prepare params) ];
          DescrHash.add memo d s;
          named := DescrMap.add d (gname, params) !named;
          s
        with
        | Not_found ->
            if Absent.get d then alloc [ Abs (prepare Absent.(update d false)) ]
            else
              let slot = alloc [] in
              if not (worth_abbrev d) then slot.state <- `Expand;
              DescrHash.add memo d slot;

              let fill_line (module K : Kind) tbl t =
                List.iter
                  (fun ((pv, nv), m) ->
                    match Var.merge pv nv with
                    | None -> ()
                    | Some vkey ->
                        let tkind = K.(mk (Dnf.mono m)) in
                        let old_t =
                          try VarTable.find tbl vkey with
                          | Not_found -> empty
                        in
                        let new_t = cup tkind old_t in
                        if non_empty new_t then VarTable.replace tbl vkey new_t
                        else VarTable.remove tbl vkey)
                  K.(Dnf.get_partial (get_vars t))
              in
              let vtable = VarTable.create 17 in
              let () = List.iter (fun m -> fill_line m vtable d) all_kinds in
              let found_any =
                try subtype any (VarTable.find vtable VarKey.empty) with
                | Not_found -> false
              in
              if found_any then begin
                slot.def <- [ Neg (alloc []) ];
                slot
              end
              else
                let print_vars s =
                  Var.Set.fold (fun acc v -> Var v :: acc) [] s
                in
                let acc =
                  VarTable.fold
                    (fun (pv, nv) tt acc ->
                      let vars =
                        match (print_vars pv, print_vars nv) with
                        | [], [] -> None
                        | pl, [] -> Some (inter_d pl)
                        | [], nl -> Some (Neg (alloc nl))
                        | pl, nl -> Some (Diff (alloc [ inter_d pl ], alloc nl))
                      in
                      if subtype any tt then
                        match vars with
                        | None ->
                            assert false
                            (* We would have found it with found_any above *)
                        | Some v -> v :: acc
                      else
                        (* sequence type. We do not want to split types such as
                           Any into Any \ [ Any *] | Any, and likewise, write
                           Atom \ [] | []. *)
                        (* check whether to display directly *)
                        let tt, positive =
                          if worth_complement tt then (diff any tt, false)
                          else (tt, true)
                        in
                        let finite_atoms = AtomSet.is_finite (Atom.get tt) in
                        let u_acc, tt =
                          print_seq finite_atoms decompile [] tt
                        in

                        (* basic types *)
                        let u_acc = print_chars u_acc tt in
                        let u_acc = print_ints u_acc tt in
                        (* display the Bool type explicitely if present *)
                        let u_acc, tt =
                          if finite_atoms && subtype bool_type tt then
                            (Display "Bool" :: u_acc, diff tt bool_type)
                          else (u_acc, tt)
                        in
                        let u_acc = print_atoms u_acc tt in

                        (* products *)
                        let u_acc =
                          List.fold_left
                            (fun acc (tt1, tt2) ->
                              Pair (prepare tt1, prepare tt2) :: acc)
                            u_acc
                            (Product.partition any (Times.get_vars tt))
                        in
                        (* xml products *)
                        let u_acc =
                          List.flatten
                            (List.map
                               (fun (t1, t2) ->
                                 try
                                   let n, params =
                                     DescrPairMap.find (t1, t2) !named_xml
                                   in
                                   [ Name (n, List.map prepare params) ]
                                 with
                                 | Not_found ->
                                     let tag =
                                       match
                                         AtomSet.print_tag (Atom.get t1)
                                       with
                                       | Some a
                                         when is_empty
                                                Atom.(update t1 Dnf.empty) ->
                                           `Tag a
                                       | _ -> `Type (prepare t1)
                                     in
                                     assert (
                                       is_empty Times.(update t2 Dnf.empty));
                                     List.rev_map
                                       (fun (ta, tb) ->
                                         Xml (tag, prepare ta, prepare tb))
                                       (Product.get t2))
                               (Product.partition Times.any (Xml.get_vars tt)))
                          @ u_acc
                        in
                        (* arrows *)
                        let u_acc =
                          List.map
                            (fun (p, n) ->
                              let p =
                                List.fold_left
                                  (fun acc (t, s) ->
                                    if is_empty (descr t) then acc
                                    else
                                      (prepare (descr t), prepare (descr s))
                                      :: acc)
                                  [] p
                              in
                              let n =
                                List.rev_map
                                  (fun (t, s) ->
                                    (prepare (descr t), prepare (descr s)))
                                  n
                              in
                              Arrows (p, n))
                            (Function.Dnf.get (Function.get_vars tt))
                          @ u_acc
                        in
                        (* records *)
                        let u_acc =
                          List.map
                            (fun (r, some, none) ->
                              let r =
                                LabelMap.map (fun (o, t) -> (o, prepare t)) r
                              in
                              Record (r, some, none))
                            (Record.get tt)
                          @ u_acc
                        in
                        let u_acc =
                          List.map
                            (fun x -> Atomic x)
                            (AbstractSet.print (Abstract.get tt))
                          @ u_acc
                        in
                        let u_acc = List.rev u_acc in
                        match (vars, positive) with
                        | None, true -> u_acc @ acc
                        | None, false -> [ Neg (alloc u_acc) ] @ acc
                        | Some v, true ->
                            [ inter_nd [ alloc [ v ]; alloc u_acc ] ] @ acc
                        | Some v, false ->
                            [ Diff (alloc [ v ], alloc u_acc) ] @ acc)
                    vtable []
                in
                slot.def <- acc;
                slot)

  and decompile d =
    Decompile.decompile
      (fun t ->
        let vdnf = Times.(Dnf.get_partial (get_vars t)) in
        if
          not
            (List.for_all
               (function
                 | ([], []), _ -> true
                 | _ -> false)
               vdnf)
        then raise Variable_in_regexp
        else
          let tr = Product.get t in
          let tr = Product.merge_same_first tr in
          let tr = Product.clean_normal tr in

          let eps = AtomSet.contains nil_atom (Atom.get t) in
          let tr_cons = List.map (fun (li, ti) -> (cons li, cons ti)) tr in

          try
            let l0, t0 =
              List.find
                (fun ((_l0, t0) as tr0) ->
                  let t'' =
                    List.fold_left
                      (fun accu ((li, ti) as tri) ->
                        if tr0 == tri then accu else cup accu (times li ti))
                      (if eps then nil_type else empty)
                      tr_cons
                  in
                  equiv (descr t0) t'')
                tr_cons
            in
            `Eps (prepare (descr l0), descr t0)
          with
          | Not_found ->
              let tr = List.map (fun (l, t) -> (prepare l, t)) tr in
              `T (tr, eps))
      d

  let gen = ref 0

  let rec assign_name s =
    incr gen;
    match s.state with
    | `None ->
        let g = !gen in
        s.state <- `Marked;
        List.iter assign_name_rec s.def;
        if s.state == `Marked && !gen <= g + 2 then s.state <- `None
    | `Marked ->
        s.state <- `Named (name ());
        to_print := s :: !to_print
    | _ -> ()

  and assign_name_rec = function
    | Neg t -> assign_name t
    | Abs t -> assign_name t
    | Char _
    | Atomic _
    | Interval _
    | Display _
    | Var _ ->
        ()
    | Name (_, params) -> List.iter assign_name params
    | Intersection l -> List.iter assign_name l
    | Regexp r -> assign_name_regexp r
    | Diff (t1, t2) ->
        assign_name t1;
        assign_name t2
    | Pair (t1, t2) ->
        assign_name t1;
        assign_name t2
    | Xml (tag, t2, t3) ->
        (match tag with
        | `Type t -> assign_name t
        | _ -> ());
        assign_name t2;
        assign_name t3
    | Record (r, _, _) ->
        List.iter (fun (_, (_, t)) -> assign_name t) (LabelMap.get r)
    | Arrows (p, n) ->
        List.iter
          (fun (t1, t2) ->
            assign_name t1;
            assign_name t2)
          p;
        List.iter
          (fun (t1, t2) ->
            assign_name t1;
            assign_name t2)
          n

  and assign_name_regexp = function
    | Pretty.Epsilon
    | Pretty.Empty ->
        ()
    | Pretty.Alt (r1, r2)
    | Pretty.Seq (r1, r2) ->
        assign_name_regexp r1;
        assign_name_regexp r2
    | Pretty.Star r
    | Pretty.Plus r ->
        assign_name_regexp r
    | Pretty.Trans t -> assign_name t

  let print_gname ppf (cu, n) = Format.fprintf ppf "%s%a" cu Ns.QName.print n

  (* operator precedences:
     20 names, constants, ...
     10 : <t1 >
     9 : star plus ?
     8 : seq
     7 : \ left of \
     6 : \
     5 : &
     4 : | alt
     3 : should be xml but for compatibility xml is stronger than & | \
     2 : arrow left of arrow
     1 : arrow
     0
     We use a private type to force the use of a symbolic name
  *)
  module Level : sig
    type t = private int

    val make : int -> t
  end = struct
    type t = int

    let make x = x
  end

  let lv_min = Level.make 0
  let lv_arrow = Level.make 1
  let lv_larrow = Level.make 2
  let lv_pair = Level.make 3
  let lv_alt = Level.make 4
  let lv_and = Level.make 5
  let lv_diff = Level.make 6
  let lv_ldiff = Level.make 7
  let lv_app = Level.make 8
  let lv_seq = Level.make 9
  let lv_post = Level.make 10
  let lv_xml = Level.make 11
  let lv_comma = Level.make 12
  let _lv_max = Level.make 20

  let opar ppf ~level (pri : Level.t) =
    if Stdlib.(level < pri) then Format.fprintf ppf "@[("

  let cpar ppf ~level (pri : Level.t) =
    if Stdlib.(level < pri) then Format.fprintf ppf ")@]"

  let get_name = function
    | { state = `Named n; _ } -> n
    | _ -> assert false

  let do_print_list empty pri op pri_op pr_e ppf l =
    let rec loop l =
      match l with
      | [] -> ()
      | [ h ] -> (pr_e pri_op) ppf h
      | h :: t ->
          Format.fprintf ppf "%a @{<prettify>%s@}@ " (pr_e pri_op) h op;
          loop t
    in
    match l with
    | [] -> Format.fprintf ppf "@{<prettify>%s@}" empty
    | [ h ] -> (pr_e pri) ppf h
    | _ ->
        opar ppf ~level:pri_op pri;
        loop (List.rev l);
        cpar ppf ~level:pri_op pri

  let rec do_print_slot (pri : Level.t) ppf s =
    match s.state with
    | `Named n -> U.print ppf n
    | `GlobalName (gname, []) -> print_gname ppf gname
    | _ -> do_print_slot_real pri ppf s.def

  and do_print_slot_real pri ppf def =
    do_print_list "Empty" pri "|" lv_alt do_print ppf def

  and do_print pri ppf = function
    | Neg { def = []; _ } -> Format.fprintf ppf "@{<prettify>Any@}"
    | Neg t ->
        Format.fprintf ppf "@{<prettify>Any@} \\ @[%a@]" (do_print_slot lv_diff)
          t
    | Abs t -> Format.fprintf ppf "?(@[%a@])" (do_print_slot lv_min) t
    | Var v -> Format.fprintf ppf "%a" Var.print v
    | Name (n, []) -> print_gname ppf n
    | Name (n, params) ->
        opar ppf ~level:lv_app pri;
        Format.fprintf ppf "@[%a@ (@[%a@])@]" print_gname n
          (do_print_list "#ERROR" pri "," lv_comma do_print_slot)
          params;
        cpar ppf ~level:lv_app pri
    | Display s -> Format.fprintf ppf "%s" s
    | Char c -> CharSet.V.print ppf c
    | Regexp r -> Format.fprintf ppf "@[[ %a ]@]" (do_print_regexp lv_min) r
    | Atomic a -> a ppf
    | Interval i -> (
        match List.rev_map (fun x -> Atomic x) (Intervals.print i) with
        | [] -> assert false
        | [ a ] ->
            if pri == lv_pair && not (fst (Intervals.is_bounded i)) then
              Format.fprintf ppf " ";
            do_print pri ppf a
        | lst ->
            opar ppf ~level:lv_alt pri;
            if
              Stdlib.(lv_alt < pri)
              || (pri = lv_pair && not (fst (Intervals.is_bounded i)))
            then Format.fprintf ppf " ";
            do_print_slot_real lv_alt ppf lst;
            cpar ppf ~level:lv_alt pri)
    | Diff (a, b) ->
        opar ppf ~level:lv_diff pri;
        Format.fprintf ppf "@[%a@] \\ @[%a@]" (do_print_slot lv_ldiff) a
          (do_print_slot lv_diff) b;
        cpar ppf ~level:lv_diff pri
    | Intersection [] -> ()
    | Intersection [ p ] -> do_print_slot pri ppf p
    | Intersection a -> do_print_list "Any" pri "&" lv_and do_print_slot ppf a
    | Pair (t1, t2) ->
        Format.fprintf ppf "@[(%a,%a)@]" (do_print_slot lv_pair) t1
          (do_print_slot lv_min) t2
    | Xml (tag, attr, t) ->
        opar ppf ~level:lv_xml pri;
        Format.fprintf ppf "<%a%a>%a" do_print_tag tag do_print_attr attr
          (do_print_slot lv_xml) t;
        cpar ppf ~level:lv_xml pri
    | Record (r, some, none) ->
        Format.fprintf ppf "@[{";
        do_print_record ppf (r, some, none);
        Format.fprintf ppf " }@]"
    | Arrows (p, []) ->
        do_print_list "Arrow" pri "&" lv_and do_print_arrow ppf p
    | Arrows (p, n) ->
        opar ppf ~level:lv_diff pri;
        do_print_list "Arrow" lv_diff "&" lv_and do_print_arrow ppf p;
        Format.fprintf ppf " \\@ ";
        do_print_list "##ERROR" lv_diff "|" lv_alt do_print_arrow ppf n;
        cpar ppf ~level:lv_diff pri

  and do_print_arrow pri ppf (t, s) =
    opar ppf ~level:lv_arrow pri;
    Format.fprintf ppf "%a @{<prettify>->@} %a" (do_print_slot lv_larrow) t
      (do_print_slot lv_arrow) s;
    cpar ppf ~level:lv_arrow pri

  and do_print_tag ppf = function
    | `Tag s -> s ppf
    | `Type t -> Format.fprintf ppf "(%a)" (do_print_slot lv_min) t

  and do_print_attr ppf = function
    | { state = _; def = []; _ } -> Format.fprintf ppf " .."
    | { state = `Marked | `Expand | `None; def = [ Record (r, some, none) ]; _ }
      ->
        do_print_record ppf (r, some, none)
    | t -> Format.fprintf ppf " (%a)" (do_print_slot lv_min) t

  and do_print_record ppf (r, some, none) =
    List.iter
      (fun (l, (o, t)) ->
        let opt = if o then "?" else "" in
        Format.fprintf ppf "@ @[%a=%s@]%a" Label.print_attr l opt
          (do_print_slot lv_min) t)
      (LabelMap.get r);
    if not none then Format.fprintf ppf "@ (+others)";
    if some then Format.fprintf ppf " .."

  and do_print_regexp pri ppf = function
    | Pretty.Empty -> Format.fprintf ppf "@{<prettify>Empty@}" (*assert false *)
    | Pretty.Epsilon -> ()
    | Pretty.Seq (Pretty.Trans { def = [ Char _ ]; _ }, _) as r -> (
        match extract_string [] r with
        | s, None ->
            Format.fprintf ppf "'";
            List.iter (CharSet.V.print_in_string ppf) s;
            Format.fprintf ppf "'"
        | s, Some r ->
            opar ppf ~level:lv_seq pri;
            Format.fprintf ppf "'";
            List.iter (CharSet.V.print_in_string ppf) s;
            Format.fprintf ppf "' %a" (do_print_regexp lv_seq) r;
            cpar ppf ~level:lv_seq pri)
    | Pretty.Seq (r1, r2) ->
        opar ppf ~level:lv_seq pri;
        Format.fprintf ppf "%a@ %a" (do_print_regexp lv_seq) r1
          (do_print_regexp lv_seq) r2;
        cpar ppf ~level:lv_seq pri
    | Pretty.Alt (r, Pretty.Epsilon)
    | Pretty.Alt (Pretty.Epsilon, r) ->
        Format.fprintf ppf "@[%a@]?" (do_print_regexp lv_post) r
    | Pretty.Alt (r1, r2) ->
        opar ppf ~level:lv_alt pri;
        Format.fprintf ppf "%a |@ %a" (do_print_regexp lv_alt) r1
          (do_print_regexp lv_alt) r2;
        cpar ppf ~level:lv_alt pri
    | Pretty.Star r -> Format.fprintf ppf "@[%a@]*" (do_print_regexp lv_post) r
    | Pretty.Plus r -> Format.fprintf ppf "@[%a@]+" (do_print_regexp lv_post) r
    | Pretty.Trans t -> do_print_slot pri ppf t

  and extract_string accu = function
    | Pretty.Seq (Pretty.Trans { def = [ Char c ]; _ }, r) ->
        extract_string (c :: accu) r
    | Pretty.Trans { def = [ Char c ]; _ } -> (List.rev (c :: accu), None)
    | r -> (List.rev accu, Some r)

  and pp_type ppf t =
    let t = uniq t in
    let t = prepare t in
    assign_name t;
    Format.fprintf ppf "@[@[%a@]" (do_print_slot lv_min) t;
    (match List.rev !to_print with
    | [] -> ()
    | s :: t ->
        Format.fprintf ppf " where@ @[<v>%a = @[%a@]" U.print (get_name s)
          (do_print_slot_real lv_min)
          s.def;
        List.iter
          (fun s ->
            Format.fprintf ppf " and@ %a = @[%a@]" U.print (get_name s)
              (do_print_slot_real lv_min)
              s.def)
          t;
        Format.fprintf ppf "@]");
    Format.fprintf ppf "@]";
    count_name := 0;
    to_print := [];
    DescrHash.clear memo

  and pp_noname ppf t =
    let old_named = !named in
    let old_named_xml = !named_xml in
    unregister_global t;
    pp_type ppf t;
    named := old_named;
    named_xml := old_named_xml

  let print_node ppf n = pp_type ppf (descr n)
  let print ppf t = pp_type ppf t
  let print_noname ppf t = pp_noname ppf t
  let () = Types.forward_print := print

  let print_to_string f x =
    let b = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer b in
    f ppf x;
    Format.pp_print_flush ppf ();
    Buffer.contents b

  let to_string t = print_to_string print t
end

type service_params =
  | TProd of service_params * service_params
  | TOption of service_params
  | TList of string * service_params
  | TSet of service_params
  | TSum of service_params * service_params
  | TString of string
  | TInt of string
  | TInt32 of string
  | TInt64 of string
  | TFloat of string
  | TBool of string
  | TFile of string
  (* | TUserType of string * (string -> 'a) * ('a -> string) *)
  | TCoord of string
  | TCoordv of service_params * string
  | TESuffix of string
  | TESuffixs of string
  (*  | TESuffixu of (string * (string -> 'a) * ('a -> string)) *)
  | TSuffix of (bool * service_params)
  | TUnit
  | TAny
  | TConst of string

module Service = struct
  open Ident

  type service_attributs = {
    mutable const : bool;
    mutable end_suffix : bool;
    mutable file : bool;
  }

  let prepare t =
    let t = Print.uniq t in
    let t = Print.prepare t in
    Print.assign_name t;
    t

  let trace _msg =
    (* output_string stderr (msg ^ "\n");
       flush stderr *)
    ()

  let print_to_string f =
    let b = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer b in
    f ppf;
    Format.pp_print_flush ppf ();
    Buffer.contents b

  let get_gname (_, n) = Ns.QName.to_string n
  let get_gtype t = get_gname t

  (* from ns:atom, returns :atom. *)
  let strip_namespace tagname =
    let len = String.length tagname in
    let cur = ref len in
    for i = 0 to len - 1 do
      let c = tagname.[i] in
      match c with
      | ':' -> cur := i
      | _ -> ()
    done;
    if !cur = len then tagname else String.sub tagname !cur (len - !cur)

  let convert_gtype t name =
    match t with
    | "Int" -> TInt name
    | "String" -> TString name
    | "Float" -> TFloat name
    | "Bool" -> TBool name
    | _ -> assert false

  let rec convert (s : Print.nd) name =
    trace ("debug:convert: " ^ name);
    match s.Print.state with
    | `Named n ->
        trace ("debug:convert " ^ U.to_string n);
        convert_real name s.Print.def
    | `GlobalName (n, _) -> (
        let t = get_gtype n in
        trace ("debug:convert:globalname: " ^ t);
        match t with
        | "Int"
        | "String"
        | "Float"
        | "Bool" ->
            convert_gtype t name
        | _ -> convert_real name s.Print.def)
    | _ -> convert_real name s.Print.def

  and convert_real name def =
    let aux = function
      | [] ->
          trace ("debug:convert_real:" ^ name);
          assert false
      | [ h ] -> convert_expr name h
      | _ :: _ -> assert false
    in
    aux def

  and convert_expr name = function
    | Print.Neg { Print.def = []; _ } -> assert false
    | Print.Neg t -> convert t name
    | Print.Abs t -> convert t name
    | Print.Name _ -> assert false
    | Print.Char _ -> assert false
    | Print.Regexp r -> convert_regexp name r
    | Print.Xml (tag, attr, t) -> (
        let flags = { const = false; end_suffix = false; file = false } in
        convert_attrs flags attr;
        let tagname = convert_tag tag in
        let tagname = strip_namespace tagname in
        match tagname with
        | ":suffix" -> TSuffix (true, convert t tagname)
        | ":any" -> TAny
        | _ ->
            if flags.const then TConst tagname
            else if flags.end_suffix then TESuffix tagname
            else if flags.file then TFile tagname
            else convert t tagname)
    | _ -> assert false

  and convert_regexp name = function
    | Pretty.Seq (r1, r2) ->
        TProd (convert_regexp name r1, convert_regexp name r2)
    | Pretty.Alt (r, Pretty.Epsilon)
    | Pretty.Alt (Pretty.Epsilon, r) ->
        TOption (convert_regexp name r)
    | Pretty.Alt (r1, r2) ->
        TSum (convert_regexp name r1, convert_regexp name r2)
    | Pretty.Star r -> TList (name, convert_regexp name r)
    | Pretty.Plus r -> TList (name, convert_regexp name r)
    | Pretty.Trans t -> convert t name
    | Pretty.Epsilon
    | Pretty.Empty ->
        TUnit

  and convert_tag = function
    | `Tag s -> print_to_string s
    | `Type _ -> assert false

  and convert_attrs flags =
    trace "convert_attrs";
    function
    | {
        Print.state = `Marked | `Expand | `None;
        def = [ Print.Record (r, some, none) ];
        _;
      } ->
        convert_record flags (r, some, none)
    | { Print.state = `Named n; def = [ Print.Record (r, some, none) ]; _ } ->
        trace ("debug:convert_attrs:Named " ^ U.to_string n);
        convert_record flags (r, some, none)
    (*	  convert_real name s.Print.def *)
    (*      | `GlobalName n -> get_gtype n name *)
    | _ ->
        trace "convert_attrs:_";
        ()

  and convert_record flags (r, some, none) =
    List.iter
      (fun (l, (_o, _t)) ->
        (*	let opt = if o then "?" else "" in *)
        let attr_label = Label.string_of_attr l in
        trace ("convert_record:" ^ attr_label);
        match attr_label with
        | "const" -> flags.const <- true
        | "end_suffix" -> flags.end_suffix <- true
        | "file" -> flags.file <- true
        | _ -> output_string stderr ("Bad attribute name:" ^ attr_label ^ "\n")
        (*	  Label.print_attr l opt (do_print_slot 0) t *))
      (LabelMap.get r);
    if not none then output_string stderr " (+others)";
    if some then output_string stderr " .."

  let clear () =
    Print.count_name := 0;
    Print.to_print := [];
    Print.DescrHash.clear Print.memo

  let to_service_params t =
    Print.unregister_global t;
    let s = prepare t in
    let ret = convert s "" in
    clear ();
    ret

  let to_string t =
    let bool_to_string = function
      | true -> "true"
      | false -> "false"
    in
    let rec aux = function
      | TInt n -> "TInt(" ^ n ^ ")"
      | TFloat n -> "TFloat(" ^ n ^ ")"
      | TBool n -> "TBool(" ^ n ^ ")"
      | TString n -> "TString(" ^ n ^ ")"
      | TConst n -> "TConst(" ^ n ^ ")"
      | TProd (e1, e2) -> "TProd(" ^ aux e1 ^ "," ^ aux e2 ^ ")"
      | TOption e -> "TOption(" ^ aux e ^ ")"
      | TSet e -> "TSet(" ^ aux e ^ ")"
      | TList (n, e) -> "TList(" ^ n ^ "," ^ aux e ^ ")"
      | TUnit -> "TUnit()"
      | TSum (e1, e2) -> "TSum(" ^ aux e1 ^ "," ^ aux e2 ^ ")"
      | TSuffix (b, e) -> "TSuffix(" ^ bool_to_string b ^ "," ^ aux e ^ ")"
      | TESuffix n -> "TESuffix(" ^ n ^ ")"
      | TFile n -> "TFile(" ^ n ^ ")"
      | TAny -> "TAny"
      | _ -> " unknown "
    in
    aux t
end

include Print
