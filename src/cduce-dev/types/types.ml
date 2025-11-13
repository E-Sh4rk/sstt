let std_compare = compare

open Ident
module U = Encodings.Utf8

let count = ref 0

let () =
  Stats.register Stats.Summary (fun ppf ->
      Format.fprintf ppf "Allocated type nodes:%i@\n" !count)

(*
To be sure not to use generic comparison ...
*)
let ( = ) : int -> int -> bool = ( == )
let ( < ) : int -> int -> bool = ( < )
let[@ocaml.warning "-32"] ( <= ) : int -> int -> bool = ( <= )
let[@ocaml.warning "-32"] ( <> ) : int -> int -> bool = ( <> )
let[@ocaml.warning "-32"] compare = 1

type const =
  | Integer of Intervals.V.t
  | Atom of AtomSet.V.t
  | Char of CharSet.V.t
  | Pair of const * const
  | Xml of const * const
  | Record of const label_map
  | String of U.uindex * U.uindex * U.t * const

module Const = struct
  type t = const

  let check _ = ()
  let dump ppf _ = Format.fprintf ppf "<Types.Const.t>"

  let rec compare c1 c2 =
    match (c1, c2) with
    | Integer x, Integer y -> Intervals.V.compare x y
    | Integer _, _ -> -1
    | _, Integer _ -> 1
    | Atom x, Atom y -> AtomSet.V.compare x y
    | Atom _, _ -> -1
    | _, Atom _ -> 1
    | Char x, Char y -> CharSet.V.compare x y
    | Char _, _ -> -1
    | _, Char _ -> 1
    | Pair (x1, x2), Pair (y1, y2) ->
      let c = compare x1 y1 in
      if c <> 0 then c else compare x2 y2
    | Pair (_, _), _ -> -1
    | _, Pair (_, _) -> 1
    | Xml (x1, x2), Xml (y1, y2) ->
      let c = compare x1 y1 in
      if c <> 0 then c else compare x2 y2
    | Xml (_, _), _ -> -1
    | _, Xml (_, _) -> 1
    | Record x, Record y -> LabelMap.compare compare x y
    | Record _, _ -> -1
    | _, Record _ -> 1
    | String (i1, j1, s1, r1), String (i2, j2, s2, r2) ->
      let c = std_compare i1 i2 in
      if c <> 0 then c
      else
        let c = std_compare j1 j2 in
        if c <> 0 then c
        else
          let c = U.compare s1 s2 in
          if c <> 0 then c
          (* Should compare
             only the substring *)
          else compare r1 r2

  let rec hash = function
    | Integer x -> 1 + (17 * Intervals.V.hash x)
    | Atom x -> 2 + (17 * AtomSet.V.hash x)
    | Char x -> 3 + (17 * CharSet.V.hash x)
    | Pair (x, y) -> 4 + (17 * hash x) + (257 * hash y)
    | Xml (x, y) -> 5 + (17 * hash x) + (257 * hash y)
    | Record x -> 6 + (17 * LabelMap.hash hash x)
    | String (_, _, s, r) -> 7 + (17 * U.hash s) + (257 * hash r)

  (* Note: improve hash for String *)

  let equal c1 c2 = compare c1 c2 = 0
end

type pair_kind =
  [ `Normal
  | `XML
  ]

type descr = {
  atoms : Bdd.VarAtomSet.t;
  ints : Bdd.VarIntervals.t;
  chars : Bdd.VarCharSet.t;
  times : (node * node) Bdd.var_bdd;
  xml : (node * node) Bdd.var_bdd;
  arrow : (node * node) Bdd.var_bdd;
  record : (bool * node label_map) Bdd.var_bdd;
  abstract : Bdd.VarAbstractSet.t;
  absent : bool;
  mutable hash : int;
}

and node = {
  id : int;
  cu : Compunit.t;
  mutable descr : descr;
}

let empty =
  {
    atoms = Bdd.empty;
    ints = Bdd.empty;
    chars = Bdd.empty;
    times = Bdd.empty;
    xml = Bdd.empty;
    arrow = Bdd.empty;
    record = Bdd.empty;
    abstract = Bdd.empty;
    absent = false;
    hash = -1;
  }

let todo_dump = Hashtbl.create 16
let forward_print = ref (fun _ _ -> assert false)

module Node = struct
  type t = node

  let check _ = ()

  let dump ppf n =
    if not (Hashtbl.mem todo_dump n.id) then
      Hashtbl.add todo_dump n.id (n, false);
    Format.fprintf ppf "X%i" n.id

  let hash x = x.id + Compunit.hash x.cu

  let compare x y =
    let c = x.id - y.id in
    if c = 0 then Compunit.compare x.cu y.cu else c

  let equal x y = x == y || (x.id == y.id && Compunit.equal x.cu y.cu)
  let mk id d = { id; cu = Compunit.current (); descr = d }
end

module BoolPair = Bdd.Make (Custom.Pair (Node) (Node))
module BoolRec = Bdd.Make (Custom.Pair (Custom.Bool) (LabelSet.MakeMap (Node)))

module Descr = struct
  type t = descr

  let _print_lst ppf =
    List.iter (fun f ->
        f ppf;
        Format.fprintf ppf " |")

  let hash a =
    if a.hash >= 0 then a.hash
    else
      let accu = Bdd.VarCharSet.hash a.chars in
      let accu = accu + (accu lsl 4) + Bdd.VarIntervals.hash a.ints in
      let accu = accu + (accu lsl 4) + Bdd.VarAtomSet.hash a.atoms in
      let accu = accu + (accu lsl 4) + BoolPair.hash a.times in
      let accu = accu + (accu lsl 4) + BoolPair.hash a.xml in
      let accu = accu + (accu lsl 4) + BoolPair.hash a.arrow in
      let accu = accu + (accu lsl 4) + BoolRec.hash a.record in
      let accu = accu + (accu lsl 4) + Bdd.VarAbstractSet.hash a.abstract in
      let accu = if a.absent then accu + 5 else accu in
      let accu = max_int land accu in
      let () = a.hash <- accu in
      accu

  let equal a b =
    a == b
    || hash a == hash b
       && Bdd.VarAtomSet.equal a.atoms b.atoms
       && Bdd.VarCharSet.equal a.chars b.chars
       && Bdd.VarIntervals.equal a.ints b.ints
       && BoolPair.equal a.times b.times
       && BoolPair.equal a.xml b.xml
       && BoolPair.equal a.arrow b.arrow
       && BoolRec.equal a.record b.record
       && Bdd.VarAbstractSet.equal a.abstract b.abstract
       && a.absent == b.absent

  let compare a b =
    if a == b then 0
    else
      let c = Bdd.VarAtomSet.compare a.atoms b.atoms in
      if c <> 0 then c
      else
        let c = Bdd.VarCharSet.compare a.chars b.chars in
        if c <> 0 then c
        else
          let c = Bdd.VarIntervals.compare a.ints b.ints in
          if c <> 0 then c
          else
            let c = BoolPair.compare a.times b.times in
            if c <> 0 then c
            else
              let c = BoolPair.compare a.xml b.xml in
              if c <> 0 then c
              else
                let c = BoolPair.compare a.arrow b.arrow in
                if c <> 0 then c
                else
                  let c = BoolRec.compare a.record b.record in
                  if c <> 0 then c
                  else
                    let c = Bdd.VarAbstractSet.compare a.abstract b.abstract in
                    if c <> 0 then c else Bdd.Bool.compare a.absent b.absent

  let check a =
    Bdd.VarCharSet.check a.chars;
    Bdd.VarIntervals.check a.ints;
    Bdd.VarAtomSet.check a.atoms;
    BoolPair.check a.times;
    BoolPair.check a.xml;
    BoolPair.check a.arrow;
    BoolRec.check a.record;
    Bdd.VarAbstractSet.check a.abstract;
    ()

  let forward_any = ref empty

  let dump_descr ppf d =
    if equal d empty then Format.fprintf ppf "EMPTY"
    else if equal d !forward_any then Format.fprintf ppf "ANY"
    else
      Format.fprintf ppf
        "<@[types = @[%a@]@\n\
        \  ints(@[%a@])@\n\
        \  atoms(@[%a@])@\n\
        \  chars(@[%a@])@\n\
        \  abstract(@[%a@])@\n\
        \  times(@[%a@])@\n\
        \  xml(@[%a@])@\n\
        \  record(@[%a@])@\n\
        \  arrow(@[%a@])@\n\
        \  absent=%b@]>" !forward_print d Bdd.VarIntervals.dump d.ints
        Bdd.VarAtomSet.dump d.atoms Bdd.VarCharSet.dump d.chars
        Bdd.VarAbstractSet.dump d.abstract BoolPair.dump d.times BoolPair.dump
        d.xml BoolRec.dump d.record BoolPair.dump d.arrow d.absent

  let dump ppf d =
    Hashtbl.clear todo_dump;
    dump_descr ppf d;
    let continue = ref true in
    let seen_list = ref [] in
    while !continue do
      continue := false;
      Hashtbl.iter
        (fun _ (n, seen) ->
           if not seen then begin
             seen_list := n :: !seen_list;
             continue := true
           end)
        todo_dump;
      List.iter
        (fun n ->
           Hashtbl.replace todo_dump n.id (n, true);
           Format.fprintf ppf "@\n%a=@[" Node.dump n;
           dump_descr ppf n.descr;
           Format.fprintf ppf "@]")
        !seen_list;
      seen_list := []
    done;
    Hashtbl.clear todo_dump
end

module DescrHash = Hashtbl.Make (Descr)
module DescrMap = Map.Make (Descr)
module DescrSet = Set.Make (Descr)
module DescrSList = SortedList.Make (Descr)

let dummy_descr =
  let () = incr count in
  let loop = Node.mk !count empty in
  let dummy =
    { empty with times = BoolPair.atom (loop, loop); absent = true; hash = -1 }
  in
  let () = loop.descr <- dummy in
  dummy

let make () =
  incr count;
  let res = Node.mk !count dummy_descr in
  res

let is_opened n = n.descr == dummy_descr

let define n d =
  assert (n.descr == dummy_descr);
  n.descr <- d

let cons d =
  incr count;
  Node.mk !count d

let any =
  {
    times = BoolPair.any;
    xml = BoolPair.any;
    arrow = BoolPair.any;
    record = BoolRec.any;
    ints = Bdd.VarIntervals.any;
    atoms = Bdd.VarAtomSet.any;
    chars = Bdd.VarCharSet.any;
    abstract = Bdd.VarAbstractSet.any;
    absent = false;
    hash = -1;
  }

let () = Descr.forward_any := any

module type Kind = sig
  module Dnf : Bdd.S

  val any : Descr.t
  val get : Descr.t -> Dnf.mono
  val get_vars : Descr.t -> Dnf.t
  val mk : Dnf.t -> Descr.t
  val update : Descr.t -> Dnf.t -> Descr.t
end

module Int = struct
  module Dnf = Bdd.VarIntervals

  let get_vars d = d.ints
  let get d = Dnf.get_mono d.ints
  let update t c = { t with ints = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Abstract = struct
  module Dnf = Bdd.VarAbstractSet

  let get_vars d = d.abstract
  let get d = Dnf.get_mono d.abstract
  let update t c = { t with abstract = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Atom = struct
  module Dnf = Bdd.VarAtomSet

  let get_vars d = d.atoms
  let get d = Dnf.get_mono d.atoms
  let update t c = { t with atoms = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Char = struct
  module Dnf = Bdd.VarCharSet

  let get_vars d = d.chars
  let get d = Dnf.get_mono d.chars
  let update t c = { t with chars = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Times = struct
  module Dnf = BoolPair

  let get d = Dnf.get_mono d.times
  let get_vars d = d.times
  let update t c = { t with times = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Xml = struct
  module Dnf = BoolPair

  let get_vars d = d.xml
  let get d = Dnf.get_mono (get_vars d)
  let update t c = { t with xml = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Function = struct
  module Dnf = BoolPair

  let get_vars d = d.arrow
  let get d = Dnf.get_mono (get_vars d)
  let update t c = { t with arrow = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Rec = struct
  module Dnf = BoolRec

  let get_vars d = d.record
  let get d = Dnf.get_mono (get_vars d)
  let update t c = { t with record = c; hash = -1 }
  let mk c = update empty c
  let any = mk (get_vars any)
end

module Absent = struct
  module Dnf = Bdd.Bool

  let get d = d.absent
  let update t c = { t with absent = c; hash = -1 }
  let mk c = update empty c
  let any = mk true
end

include Descr

let non_constructed =
  {
    any with
    times = empty.times;
    xml = empty.xml;
    record = empty.record;
    hash = -1;
  }

let interval i = Int.mk (Int.Dnf.mono i)
let times x y = Times.mk (Times.Dnf.atom (x, y))
let xml x y = Xml.mk (Xml.Dnf.atom (x, y))
let arrow x y = Function.mk (Function.Dnf.atom (x, y))
let record label t = Rec.mk (Rec.Dnf.atom (true, LabelMap.singleton label t))
let record_fields (x : bool * node Ident.label_map) = Rec.mk (Rec.Dnf.atom x)
let atom a = Atom.mk (Atom.Dnf.mono a)
let char c = Char.mk (Char.Dnf.mono c)
let abstract a = Abstract.mk (Abstract.Dnf.mono a)
let get_abstract = Abstract.get

let var x =
  {
    times = BoolPair.var x;
    xml = BoolPair.var x;
    arrow = BoolPair.var x;
    record = BoolRec.var x;
    ints = Bdd.VarIntervals.var x;
    atoms = Bdd.VarAtomSet.var x;
    chars = Bdd.VarCharSet.var x;
    abstract = Bdd.VarAbstractSet.var x;
    absent = false;
    hash = -1;
  }

let cup x y =
  if x == y then x
  else
    {
      times = Times.Dnf.cup x.times y.times;
      xml = Xml.Dnf.cup x.xml y.xml;
      arrow = Function.Dnf.cup x.arrow y.arrow;
      record = Rec.Dnf.cup x.record y.record;
      ints = Int.Dnf.cup x.ints y.ints;
      atoms = Atom.Dnf.cup x.atoms y.atoms;
      chars = Char.Dnf.cup x.chars y.chars;
      abstract = Abstract.Dnf.cup x.abstract y.abstract;
      absent = Absent.Dnf.cup x.absent y.absent;
      hash = -1;
    }

let cap x y =
  if x == y then x
  else
    {
      times = Times.Dnf.cap x.times y.times;
      xml = Xml.Dnf.cap x.xml y.xml;
      arrow = Function.Dnf.cap x.arrow y.arrow;
      record = Rec.Dnf.cap x.record y.record;
      ints = Int.Dnf.cap x.ints y.ints;
      atoms = Atom.Dnf.cap x.atoms y.atoms;
      chars = Char.Dnf.cap x.chars y.chars;
      abstract = Abstract.Dnf.cap x.abstract y.abstract;
      absent = Absent.Dnf.cap x.absent y.absent;
      hash = -1;
    }

let diff x y =
  if x == y then empty
  else
    {
      times = Times.Dnf.diff x.times y.times;
      xml = Xml.Dnf.diff x.xml y.xml;
      arrow = Function.Dnf.diff x.arrow y.arrow;
      record = Rec.Dnf.diff x.record y.record;
      ints = Int.Dnf.diff x.ints y.ints;
      atoms = Atom.Dnf.diff x.atoms y.atoms;
      chars = Char.Dnf.diff x.chars y.chars;
      abstract = Abstract.Dnf.diff x.abstract y.abstract;
      absent = Absent.Dnf.diff x.absent y.absent;
      hash = -1;
    }

let descr n =
  assert (n.descr != dummy_descr);
  n.descr

let internalize n = n
let id n = n.id

let rec constant = function
  | Integer i -> interval (Intervals.atom i)
  | Atom a -> atom (AtomSet.atom a)
  | Char c -> char (CharSet.atom c)
  | Pair (x, y) -> times (const_node x) (const_node y)
  | Xml (x, y) -> xml (const_node x) (const_node y)
  | Record x -> record_fields (false, LabelMap.map const_node x)
  | String (i, j, s, c) ->
    if U.equal_index i j then constant c
    else
      let ch, i' = U.next s i in
      constant (Pair (Char (CharSet.V.mk_int ch), String (i', j, s, c)))

and const_node c = cons (constant c)

let neg x = diff any x

module LabelS = Set.Make (Label)

let any_or_absent = { any with absent = true; hash = -1 }
let only_absent = { empty with absent = true; hash = -1 }

let get_single_record r =
  let labs accu (_, r) =
    List.fold_left (fun accu (l, _) -> LabelS.add l accu) accu (LabelMap.get r)
  in
  let extend descrs labs (o, r) =
    let rec aux i labs r =
      match labs with
      | [] -> ()
      | l1 :: labs -> (
          match r with
          | (l2, x) :: r when l1 == l2 ->
            descrs.(i) <- cap descrs.(i) (descr x);
            aux (i + 1) labs r
          | r ->
            if not o then descrs.(i) <- cap descrs.(i) only_absent;
            (* TODO:OPT *)
            aux (i + 1) labs r)
    in
    aux 0 labs (LabelMap.get r);
    o
  in
  let line (p, n) =
    let labels = List.fold_left labs (List.fold_left labs LabelS.empty p) n in
    let labels = LabelS.elements labels in
    let nlab = List.length labels in
    let mk () = Array.make nlab any_or_absent in

    let pos = mk () in
    let opos =
      List.fold_left (fun accu x -> extend pos labels x && accu) true p
    in
    let p = (opos, pos) in

    let n =
      List.map
        (fun x ->
           let neg = mk () in
           let o = extend neg labels x in
           (o, neg))
        n
    in
    (labels, p, n)
  in
  line r

let[@ocaml.warning "-32"] get_record r = List.map get_single_record (BoolRec.get r)
let get_record_full r =
  List.map (fun (vars, r) ->
      vars, get_single_record r)
    Rec.(r |> get_vars |> Dnf.get_full)


(* Subtyping algorithm *)

let diff_t d t = diff d (descr t)
let cap_t d t = cap d (descr t)

let cap_product any_left any_right l =
  List.fold_left
    (fun (d1, d2) (t1, t2) -> (cap_t d1 t1, cap_t d2 t2))
    (any_left, any_right) l

let any_pair = { empty with times = any.times; hash = -1 }
let rec exists max f = max > 0 && (f (max - 1) || exists (max - 1) f)

exception NotEmpty

module Witness = struct
  module NodeSet = Set.Make (Node)

  type witness =
    | WInt of Intervals.V.t
    | WAtom of AtomSet.sample
    | WChar of CharSet.V.t
    | WAbsent
    | WAbstract of AbstractSet.elem option
    | WPair of witness * witness * witness_slot
    | WXml of witness * witness * witness_slot
    | WRecord of witness label_map * bool * witness_slot
    (* Invariant: WAbsent cannot actually appear *)
    | WFun of (witness * witness option) list * witness_slot
    (* Poly *)
    | WPoly of Var.Set.t * Var.Set.t * witness

  and witness_slot = {
    mutable wnodes_in : NodeSet.t;
    mutable wnodes_out : NodeSet.t;
    mutable wuid : int;
  }

  module WHash = Hashtbl.Make (struct
      type t = witness

      let rec hash_small = function
        | WInt i -> 17 * Intervals.V.hash i
        | WChar c -> 1 + (17 * CharSet.V.hash c)
        | WAtom None -> 2
        | WAtom (Some (ns, None)) -> 3 + (17 * Ns.Uri.hash ns)
        | WAtom (Some (_, Some t)) -> 4 + (17 * Ns.Label.hash t)
        | WAbsent -> 5
        | WAbstract None -> 6
        | WAbstract (Some t) -> 7 + (17 * AbstractSet.T.hash t)
        | WPair (_, _, s)
        | WXml (_, _, s)
        | WRecord (_, _, s)
        | WFun (_, s) ->
          8 + (17 * s.wuid)
        | WPoly (pos, neg, w) ->
          9 + Var.Set.hash pos + 17 * Var.Set.hash neg + 257 * hash_small w

      let hash = function
        | WPair (p1, p2, _) -> (257 * hash_small p1) + (65537 * hash_small p2)
        | WXml (p1, p2, _) -> 1 + (257 * hash_small p1) + (65537 * hash_small p2)
        | WRecord (r, o, _) ->
          (if o then 2 else 3) + (257 * LabelMap.hash hash_small r)
        | WFun (f, _) ->
          4
          + 257
            * Hashtbl.hash
              (List.map
                 (function
                   | x, None -> 17 * hash_small x
                   | x, Some y ->
                     1 + (17 * hash_small x) + (257 * hash_small y))
                 f)
        | _ -> assert false

      let rec equal_small w1 w2 =
        match (w1, w2) with
        | WInt i1, WInt i2 -> Intervals.V.equal i1 i2
        | WChar c1, WChar c2 -> CharSet.V.equal c1 c2
        | WAtom None, WAtom None -> true
        | WAtom (Some (ns1, None)), WAtom (Some (ns2, None)) ->
          Ns.Uri.equal ns1 ns2
        | WAtom (Some (_, Some t1)), WAtom (Some (_, Some t2)) ->
          Ns.Label.equal t1 t2
        | WAbsent, WAbsent -> true
        | WAbstract None, WAbstract None -> false
        | WAbstract (Some t1), WAbstract (Some t2) -> AbstractSet.T.equal t1 t2
        | WPoly (p1, n1, w1), WPoly (p2, n2, w2) ->
          Var.Set.equal p1 p2 &&
          Var.Set.equal n1 n2 &&
          equal_small w1 w2
        | _ -> w1 == w2

      let equal w1 w2 =
        match (w1, w2) with
        | WPair (p1, q1, _), WPair (p2, q2, _)
        | WXml (p1, q1, _), WXml (p2, q2, _) ->
          equal_small p1 p2 && equal_small q1 q2
        | WRecord (r1, o1, _), WRecord (r2, o2, _) ->
          o1 == o2 && LabelMap.equal equal_small r1 r2
        | WFun (f1, _), WFun (f2, _) ->
          List.length f1 = List.length f2
          && List.for_all2
            (fun (x1, y1) (x2, y2) ->
               equal_small x1 x2
               &&
               match (y1, y2) with
               | Some y1, Some y2 -> equal_small y1 y2
               | None, None -> true
               | _ -> false)
            f1 f2
        | _ -> false
    end)

  let wmemo = WHash.create 1024
  let wuid = ref 0

  let wslot () =
    { wuid = !wuid; wnodes_in = NodeSet.empty; wnodes_out = NodeSet.empty }

  let () =
    Stats.register Stats.Summary (fun ppf ->
        Format.fprintf ppf "Allocated witnesses:%i@\n" !wuid)

  let rec print_witness ppf = function
    | WInt i -> Format.fprintf ppf "%a" Intervals.V.print i
    | WChar c -> Format.fprintf ppf "%a" CharSet.V.print c
    | WAtom None -> Format.fprintf ppf "`#:#"
    | WAtom (Some (ns, None)) ->
      Format.fprintf ppf "`%a" Ns.InternalPrinter.print_any_ns ns
    | WAtom (Some (_, Some t)) -> Format.fprintf ppf "`%a" Ns.Label.print_attr t
    | WPair (w1, w2, _) ->
      Format.fprintf ppf "(%a,%a)" print_witness w1 print_witness w2
    | WXml (w1, w2, _) ->
      Format.fprintf ppf "XML(%a,%a)" print_witness w1 print_witness w2
    | WRecord (ws, o, _) ->
      Format.fprintf ppf "{";
      LabelMap.iteri
        (fun l w ->
           Format.fprintf ppf " %a=%a" Label.print_attr l print_witness w)
        ws;
      if o then Format.fprintf ppf " ..";
      Format.fprintf ppf " }"
    | WFun (f, _) ->
      Format.fprintf ppf "FUN{";
      List.iter
        (fun (x, y) ->
           Format.fprintf ppf " %a->" print_witness x;
           match y with
           | None -> Format.fprintf ppf "#"
           | Some y -> print_witness ppf y)
        f;
      Format.fprintf ppf " }"
    | WAbstract None -> Format.fprintf ppf "Abstract(..)"
    | WAbstract (Some s) -> Format.fprintf ppf "Abstract(%s)" s
    | WAbsent -> Format.fprintf ppf "Absent"
    | WPoly (pos, neg, w) ->
      Format.fprintf ppf "Poly(%a, %a, %a)"
        Var.Set.print pos
        Var.Set.print neg
        print_witness w
  let wmk w =
    (* incr wuid; w *)
    (* hash-consing disabled *)
    try WHash.find wmemo w with
    | Not_found ->
      incr wuid;
      WHash.add wmemo w w;
      (* Format.fprintf Format.std_formatter "W:%a@."
         print_witness w; *)
      w

  let wpair p1 p2 = wmk (WPair (p1, p2, wslot ()))
  let wxml p1 p2 = wmk (WXml (p1, p2, wslot ()))
  let wrecord r o = wmk (WRecord (r, o, wslot ()))
  let wfun f = wmk (WFun (f, wslot ()))

  (* A witness with variables wpos wneg
     belongs to type with variables tpos tneg
     if :
       - wpos and tneg are disjoint (a witness 'a&0 cannot belong
        to a type T\'a)
       - likewise for wneg and tpos
       - wpos is a superset of tpos, that is 'a&'b&'c&42 is
         belongs to the type 'a&Int
       - wneg is a super set of tneg
  *)
  let subset_vars wpos wneg tpos tneg =
    let sneg = Var.Set.from_list tneg in
    Var.Set.disjoint wpos sneg &&
    Var.Set.subset sneg wneg &&
    let spos = Var.Set.from_list tpos in
    Var.Set.disjoint wneg spos &&
    Var.Set.subset spos wpos

  let basic_dnf (type mono) (module M : Kind with type Dnf.mono = mono)
      pos neg t f =
    M.get_vars t
    |> M.Dnf.get_partial
    |> List.exists (fun ((vp, vn), mono) ->
        subset_vars pos neg vp vn &&
        f mono)

  let full_dnf (type atom) (module M : Kind
                             with type Dnf.atom = atom
                              and type Dnf.line = atom list * atom list)
      pos neg t f =
    M.get_vars t
    |> M.Dnf.get_full
    |> List.exists (fun ((vp, vn), (ap, an)) ->
        subset_vars pos neg vp vn &&
        List.for_all f ap &&
        not (List.exists f an)
      )

  let rec node_has n = function
    | (WXml (_, _, s) | WPair (_, _, s) | WFun (_, s) | WRecord (_, _, s)) as w
      ->
      if NodeSet.mem n s.wnodes_in then true
      else if NodeSet.mem n s.wnodes_out then false
      else
        let r = type_has (descr n) w in
        if r then s.wnodes_in <- NodeSet.add n s.wnodes_in
        else s.wnodes_out <- NodeSet.add n s.wnodes_out;
        r
    | w -> type_has (descr n) w
  and type_has t w =
    let pos, neg, w =
      match w with
        WPoly (pos, neg, w) -> pos, neg, w
      | _ -> Var.Set.empty, Var.Set.empty, w
    in
    match w with
      WPoly _ -> assert false
    | WInt i ->
      basic_dnf (module Int) pos neg t (Intervals.contains i)
    | WChar c ->
      basic_dnf (module Char) pos neg t (CharSet.contains c)
    | WAtom a ->
      basic_dnf (module Atom) pos neg t (AtomSet.contains_sample a)
    | WAbsent -> t.absent
    | WAbstract a ->
      basic_dnf (module Abstract) pos neg t (AbstractSet.contains_sample a)
    | WPair (w1, w2, _) ->
      full_dnf (module Times) pos neg t
        (fun (n1, n2) -> node_has n1 w1 && node_has n2 w2)
    | WXml (w1, w2, _) ->
      full_dnf (module Xml) pos neg t
        (fun (n1, n2) -> node_has n1 w1 && node_has n2 w2)
    | WFun (f, _) ->
      full_dnf (module Function) pos neg t
        (fun (n1, n2) ->
           List.for_all
             (fun (x, y) ->
                (not (node_has n1 x))
                ||
                match y with
                | None -> false
                | Some y -> node_has n2 y)
             f)
    | WRecord (f, o, _) ->
      full_dnf (module Rec) pos neg t
        (fun (o', f') ->
           ((not o) || o')
           &&
           let checked = ref 0 in
           try
             LabelMap.iteri
               (fun l n ->
                  let w =
                    try
                      let w = LabelMap.assoc l f in
                      incr checked;
                      w
                    with
                    | Not_found -> WAbsent
                  in
                  if not (node_has n w) then raise Exit)
               f';
             o' || LabelMap.length f == !checked
           (* All the remaining fields cannot be WAbsent
              because of an invariant. Otherwise, we must
              check that all are WAbsent here. *)
           with
           | Exit -> false)

end

type slot = {
  mutable status : status;
  mutable notify : notify;
  mutable active : bool;
}

and status =
  | Empty
  | NEmpty of Witness.witness
  | Maybe

and notify =
  | Nothing
  | Do of slot * (Witness.witness -> unit) * notify

let slot_nempty w = { status = NEmpty w; active = false; notify = Nothing }

let rec notify w = function
  | Nothing -> ()
  | Do (n, f, rem) ->
    (if n.status == Maybe then
       try f w with
       | NotEmpty -> ());
    notify w rem

let mk_poly vars w =
  match vars with
    [], [] -> w
  | p, n -> Witness.WPoly (Var.Set.from_list p,
                           Var.Set.from_list n, w)
let rec iter_s s f = function
  | [] -> ()
  | (x,y) :: rem ->
    f x y s;
    iter_s s f rem

let set s w =
  s.status <- NEmpty w;
  notify w s.notify;
  s.notify <- Nothing;
  raise NotEmpty

let rec big_conj f l n w =
  match l with
  | [] -> set n w
  | [ arg ] -> f w arg n
  | arg :: rem -> (
      let s =
        {
          status = Maybe;
          active = false;
          notify = Do (n, big_conj f rem n, Nothing);
        }
      in
      try
        f w arg s;
        if s.active then n.active <- true
      with
      | NotEmpty when n.status == Empty || n.status == Maybe -> ())

let memo = DescrHash.create 8191
let marks = ref []
let count_subtype = Stats.Counter.create "Subtyping internal loop"

let rec find_map f = function
    [] -> None
  | e :: l ->
    match f e with
      None -> find_map f l
    | r -> r

let check_basic (type mono) (module M : Kind with type Dnf.mono = mono) d is_empty mk =
  M.get_vars d
  |> M.Dnf.get_partial
  |> find_map (fun ((pos, neg), m) ->
      if is_empty m then None
      else
        let w = mk m in
        let w = match pos, neg with
            [], [] -> w
          | _ -> Witness.WPoly (Var.Set.from_list pos,
                                Var.Set.from_list neg, w)
        in
        Some (slot_nempty w))

let (let*) o f =
  match o with
    Some e -> e
  | None ->  f ()

let rec slot d =
  Stats.Counter.incr count_subtype;
  if d == empty then { status = Empty; active = false; notify = Nothing }
  else if d.absent then slot_nempty Witness.WAbsent
  else
    let* () = check_basic (module Int) d Intervals.is_empty
        (fun i -> Witness.WInt (Intervals.sample i)) in
    let* () = check_basic (module Atom) d AtomSet.is_empty
        (fun a -> Witness.WAtom (AtomSet.sample a)) in
    let* () = check_basic (module Char) d CharSet.is_empty
        (fun c -> Witness.WChar (CharSet.sample c)) in
    let* () = check_basic (module Abstract) d AbstractSet.is_empty
        (fun a -> Witness.WAbstract (AbstractSet.sample a)) in
    try DescrHash.find memo d with
    | Not_found ->
      let s = { status = Maybe; active = false; notify = Nothing } in
      DescrHash.add memo d s;
      (try
         iter_s s check_times Times.(d |> get_vars |> Dnf.get_full);
         iter_s s check_xml Xml.(d |> get_vars |> Dnf.get_full);
         iter_s s check_arrow Function.(d |> get_vars |> Dnf.get_full);
         iter_s s check_record (get_record_full d);
         if s.active then marks := s :: !marks else s.status <- Empty
       with
       | NotEmpty -> ());
      s

and guard n t f =
  match slot t with
  | { status = Empty; _ } -> ()
  | { status = Maybe; _ } as s ->
    n.active <- true;
    s.notify <- Do (n, f, s.notify)
  | { status = NEmpty v; _ } -> f v

and check_product vars any_right mk_witness (left, right) s =
  let rec aux w1 w2 accu1 accu2 seen = function
    (* Find a product in right which contains (w1,w2) *)
    | [] ->
      (* no such product: the current witness is in the difference. *)
      set s (mk_poly vars (mk_witness w1 w2))
    | (n1, n2) :: rest when Witness.node_has n1 w1 && Witness.node_has n2 w2 ->
      let right = List.rev_append seen rest in
      let accu2' = diff accu2 (descr n2) in
      guard s accu2' (fun w2 -> aux w1 w2 accu1 accu2' [] right);
      let accu1' = diff accu1 (descr n1) in
      guard s accu1' (fun w1 -> aux w1 w2 accu1' accu2 [] right)
    | k :: rest -> aux w1 w2 accu1 accu2 (k :: seen) rest
  in
  let t1, t2 = cap_product any any_right left in
  guard s t1 (fun w1 -> guard s t2 (fun w2 -> aux w1 w2 t1 t2 [] right))

and check_times vars prod s = check_product vars any Witness.wpair prod s
and check_xml vars prod s = check_product vars any_pair Witness.wxml prod s

and check_arrow (vpos, vneg) (left, right) s =
  let single_right f (s1, s2) s =
    let rec aux w1 w2 accu1 accu2 left =
      match left with
      | (t1, t2) :: left ->
        let accu1' = diff_t accu1 t1 in
        guard s accu1' (fun w1 -> aux w1 w2 accu1' accu2 left);

        let accu2' = cap_t accu2 t2 in
        guard s accu2' (fun w2 -> aux w1 (Some w2) accu1 accu2' left)
      | [] ->
        let op, on, f =
          match f with
          | Witness.WFun (f, _) -> [], [], f
          | Witness.WPoly (op, on, Witness.WFun (f, _)) ->
            Var.Set.(get op, get on, f)
          | _ -> assert false
        in
        set s (mk_poly (op @ vpos, on@vneg) (Witness.wfun ((w1, w2) :: f)))
    in
    let accu1 = descr s1 in
    guard s accu1 (fun w1 -> aux w1 None accu1 (neg (descr s2)) left)
  in
  big_conj single_right right s (Witness.wfun [])

and check_record vars (labels, (oleft, left), rights) s =
  let rec aux ws accus seen = function
    | [] ->
      let rec aux w i = function
        | [] ->
          assert (i == Array.length ws);
          w
        | l :: labs ->
          let w =
            match ws.(i) with
            | Witness.WAbsent -> w
            | wl -> LabelMap.add l wl w
          in
          aux w (succ i) labs
      in
      set s (mk_poly vars (Witness.wrecord (aux LabelMap.empty 0 labels) oleft))
    | (false, _) :: rest when oleft -> aux ws accus seen rest
    | (_, f) :: rest
      when not
          (exists (Array.length left) (fun i ->
               not (Witness.type_has f.(i) ws.(i)))) ->
      (* TODO: a version f get_record which keeps nodes in neg records. *)
      let right = seen @ rest in
      for i = 0 to Array.length left - 1 do
        let di = diff accus.(i) f.(i) in
        guard s di (fun wi ->
            let accus' = Array.copy accus in
            accus'.(i) <- di;
            let ws' = Array.copy ws in
            ws'.(i) <- wi;
            aux ws' accus' [] right)
      done
    | k :: rest -> aux ws accus (k :: seen) rest
  in
  let rec start wl i =
    if i < 0 then aux (Array.of_list wl) left [] rights
    else guard s left.(i) (fun w -> start (w :: wl) (i - 1))
  in
  start [] (Array.length left - 1)

let timer_subtype = Stats.Timer.create "Types.is_empty"

let is_empty d =
  Stats.Timer.start timer_subtype;
  let s = slot d in
  List.iter
    (fun s' ->
       if s'.status == Maybe then s'.status <- Empty;
       s'.notify <- Nothing)
    !marks;
  marks := [];
  Stats.Timer.stop timer_subtype (s.status == Empty)

let getwit t =
  match (slot t).status with
  | NEmpty w -> w
  | _ -> assert false

(* Assumes that is_empty has been called on t before. *)

let witness t = if is_empty t then raise Not_found else getwit t
let print_witness ppf t = Witness.print_witness ppf (witness t)
let non_empty d = not (is_empty d)
let disjoint d1 d2 = is_empty (cap d1 d2)
let subtype d1 d2 = is_empty (diff d1 d2)
let equiv d1 d2 = subtype d1 d2 && subtype d2 d1

(* redefine operations to take subtyping into account and perform hash consing *)
let forward_pointers = DescrHash.create 16

module NL = SortedList.Make (Node)

let get_all n =
  if is_opened n then NL.singleton n
  else
    try DescrHash.find forward_pointers (descr n) with
    | Not_found -> NL.empty

let add t n =
  let lold =
    try DescrHash.find forward_pointers t with
    | Not_found -> NL.empty
  in
  DescrHash.replace forward_pointers t (NL.cup n lold)

let times n1 n2 =
  let f1 = get_all n1 in
  let f2 = get_all n2 in
  let t = times n1 n2 in
  add t f1;
  add t f2;
  t

module Cache = struct
  (*
  let type_has_witness t w =
    Format.fprintf Format.std_formatter
      "check wit:%a@." print_witness w;
    let r = type_has_witness t w in
    Format.fprintf Format.std_formatter "Done@.";
    r
*)

  type 'a cache =
    | Empty
    | Type of t * 'a
    | Split of Witness.witness * 'a cache * 'a cache

  let rec find f t = function
    | Empty ->
      let r = f t in
      (Type (t, r), r)
    | Split (w, yes, no) ->
      if Witness.type_has t w then
        let yes, r = find f t yes in
        (Split (w, yes, no), r)
      else
        let no, r = find f t no in
        (Split (w, yes, no), r)
    | Type (s, rs) as c -> (
        let f1 () =
          let w = witness (diff t s) in
          let rt = f t in
          (Split (w, Type (t, rt), c), rt)
        and f2 () =
          let w = witness (diff s t) in
          let rt = f t in
          (Split (w, c, Type (t, rt)), rt)
        in

        if Random.int 2 = 0 then
          try f1 () with
          | Not_found -> (
              try f2 () with
              | Not_found -> (c, rs))
        else
          try f2 () with
          | Not_found -> (
              try f1 () with
              | Not_found -> (c, rs)))

  let rec lookup t = function
    | Empty -> None
    | Split (w, yes, no) -> lookup t (if Witness.type_has t w then yes else no)
    | Type (s, rs) -> if equiv s t then Some rs else None

  let emp = Empty

  let[@ocaml.warning "-32"] rec dump_cache f ppf = function
    | Empty -> Format.fprintf ppf "Empty"
    | Type (_, s) -> Format.fprintf ppf "*%a" f s
    | Split (_, c1, c2) ->
      Format.fprintf ppf "?(%a,%a)"
        (*Witness.print_witness w *) (dump_cache f)
        c1 (dump_cache f) c2

  let memo f =
    let c = ref emp in
    fun t ->
      let c', r = find f t !c in
      c := c';
      r
end

module Product = struct
  type t = (descr * descr) list

  let _other ?(kind = `Normal) d =
    match kind with
    | `Normal -> { d with times = empty.times; hash = -1 }
    | `XML -> { d with xml = empty.xml; hash = -1 }

  let _is_product ?kind d = is_empty (_other ?kind d)

  let need_second = function
    | _ :: _ :: _ -> true
    | _ -> false

  let normal_aux = function
    | ([] | [ _ ]) as d -> d
    | d ->
      let res = ref [] in

      let add (t1, t2) =
        let rec loop t1 t2 = function
          | [] -> res := ref (t1, t2) :: !res
          | ({ contents = d1, d2 } as r) :: l ->
            (*OPT*)
            (*	    if equal_descr d1 t1 then r := (d1,cup d2 t2) else*)
            let i = cap t1 d1 in
            if is_empty i then loop t1 t2 l
            else (
              r := (i, cup t2 d2);
              let k = diff d1 t1 in
              if non_empty k then res := ref (k, d2) :: !res;

              let j = diff t1 d1 in
              if non_empty j then loop j t2 l)
        in
        loop t1 t2 !res
      in
      List.iter add d;
      List.map ( ! ) !res

  (* Partitioning:
     (t,s) - ((t1,s1) | (t2,s2) | ... | (tn,sn))
      =
      (t & t1, s - s1) | ... | (t & tn, s - sn) | (t - (t1|...|tn), s)
  *)
  let get_aux any_right d =
    let accu = ref [] in
    let line (left, right) =
      let d1, d2 = cap_product any any_right left in
      if non_empty d1 && non_empty d2 then
        let right = List.map (fun (t1, t2) -> (descr t1, descr t2)) right in
        let right = normal_aux right in
        let resid1 = ref d1 in
        let () =
          List.iter
            (fun (t1, t2) ->
               let t1 = cap d1 t1 in
               if non_empty t1 then
                 let () = resid1 := diff !resid1 t1 in
                 let t2 = diff d2 t2 in
                 if non_empty t2 then accu := (t1, t2) :: !accu)
            right
        in
        if non_empty !resid1 then accu := (!resid1, d2) :: !accu
    in
    List.iter line (BoolPair.get d);
    !accu

  let partition = get_aux
  (* Maybe, can improve this function with:
       (t,s) \ (t1,s1) = (t&t',s\s') | (t\t',s),
     don't call normal_aux *)

  let get ?(kind = `Normal) d =
    match kind with
    | `Normal -> get_aux any d.times
    | `XML -> get_aux any_pair d.xml

  let pi1 = List.fold_left (fun acc (t1, _) -> cup acc t1) empty
  let pi2 = List.fold_left (fun acc (_, t2) -> cup acc t2) empty

  let pi2_restricted restr =
    List.fold_left
      (fun acc (t1, t2) -> if disjoint t1 restr then acc else cup acc t2)
      empty

  let restrict_1 rects pi1 =
    let aux acc (t1, t2) =
      let t1 = cap t1 pi1 in
      if is_empty t1 then acc else (t1, t2) :: acc
    in
    List.fold_left aux [] rects

  type normal = t

  module Memo = Map.Make (BoolPair)

  (* TODO: try with an hashtable *)
  (* Also, avoid lookup for simple products (t1,t2) *)
  let memo = ref Memo.empty

  let normal_times d =
    try Memo.find d !memo with
    | Not_found ->
      let gd = get_aux any d in
      let n = normal_aux gd in
      (* Could optimize this call to normal_aux because one already
         know that each line is normalized ... *)
      memo := Memo.add d n !memo;
      n

  let memo_xml = ref Memo.empty

  let normal_xml d =
    try Memo.find d !memo_xml with
    | Not_found ->
      let gd = get_aux any_pair d in
      let n = normal_aux gd in
      memo_xml := Memo.add d n !memo_xml;
      n

  let normal ?(kind = `Normal) d =
    match kind with
    | `Normal -> normal_times d.times
    | `XML -> normal_xml d.xml

  (*
  let merge_same_2 r =
    let r =
      List.fold_left
	(fun accu (t1,t2) ->
	   let t = try DescrMap.find t2 accu with Not_found -> empty in
	   DescrMap.add t2 (cup t t1) accu
	) DescrMap.empty r in
    DescrMap.fold (fun t2 t1 accu -> (t1,t2)::accu) r []
*)

  let constraint_on_2 n t1 =
    List.fold_left
      (fun accu (d1, d2) -> if disjoint d1 t1 then accu else cap accu d2)
      any n

  let merge_same_first tr =
    let trs = ref [] in
    let _ =
      List.fold_left
        (fun memo (t1, t2) ->
           let memo', l =
             Cache.find
               (fun t1 ->
                  let l = ref empty in
                  trs := (t1, l) :: !trs;
                  l)
               t1 memo
           in
           l := cup t2 !l;
           memo')
        Cache.emp tr
    in
    List.map (fun (t1, l) -> (t1, !l)) !trs

  (* same on second component: use the same implem? *)
  let clean_normal l =
    let rec aux accu (t1, t2) =
      match accu with
      | [] -> [ (t1, t2) ]
      | (s1, s2) :: rem when equiv t2 s2 -> (cup s1 t1, s2) :: rem
      | (s1, s2) :: rem -> (s1, s2) :: aux rem (t1, t2)
    in
    List.fold_left aux [] l

  let is_empty d = d == []
end

module Record = struct
  let has_record d = not (is_empty { empty with record = d.record; hash = -1 })
  let or_absent d = { d with absent = true; hash = -1 }
  let absent = or_absent empty
  let any_or_absent = any_or_absent
  let has_absent d = d.absent
  let absent_node = cons absent

  module T = struct
    type t = descr

    let any = any_or_absent
    let cap = cap
    let cup = cup
    let diff = diff
    let is_empty = is_empty
    let empty = empty
  end

  module R = struct
    type t = descr

    let any = { empty with record = any.record; hash = -1 }
    let cap = cap
    let cup = cup
    let diff = diff
    let is_empty = is_empty
    let empty = empty
  end

  module TR = Normal.Make (T) (R)

  let any_record = { empty with record = BoolRec.any; hash = -1 }

  let atom o l =
    if o && LabelMap.is_empty l then any_record
    else { empty with record = BoolRec.atom (o, l); hash = -1 }

  type zor =
    | Pair of descr * descr
    | Any

  (* given a type t and a label l, this function computes the projection on
     l on each component of the DNF *)
  let aux_split d l =
    let f (o, r) =
      try
        (* separate a record type between the type of its label l, if it appears
           explicitely and the type of the reminder of the record *)
        let lt, rem = LabelMap.assoc_remove l r in
        Pair (descr lt, atom o rem)
      with
      | Not_found ->
        (* if the label l is not present explicitely *)
        if o then
          (* if the record is open *)
          if LabelMap.is_empty r then Any
          (* if there are no explicity fields return Any,
             the record type is not splited *)
          else
            (* otherwise returns the fact that the field may or may not be present
            *)
            Pair
              ( any_or_absent,
                { empty with record = BoolRec.atom (o, r); hash = -1 } )
        else
          (* for closed records, return the fact that the label was absent *)
          Pair (absent, { empty with record = BoolRec.atom (o, r); hash = -1 })
    in
    List.fold_left
      (fun b (p, n) ->
         (* for each positive/negative intersections*)
         let rec aux_p accu = function
           | x :: p -> (
               (*get the ucrrent positive record, and split according to l*)
               match f x with
               (* if something, add (typof l, rest) to the positive accumulator*)
               | Pair (t1, t2) -> aux_p ((t1, t2) :: accu) p
               | Any ->
                 aux_p accu p
                 (* if we have { ..} in this positive
                    intersection, we can ignore it. *))
           | [] -> aux_n accu [] n
         (* now follow up with negative*)
         and aux_n p accu = function
           | x :: n -> (
               match f x with
               (* if we have a pair add it to the current negative accmulator*)
               | Pair (t1, t2) -> aux_n p ((t1, t2) :: accu) n
               (* if { .. } is in a negative intersection, the whole branch (p,n)
                  can be discarded *)
               | Any -> b)
           | [] -> (p, accu) :: b
           (* add the current pair of line *)
         in
         aux_p [] p)
      [] (BoolRec.get d.record)

  (* We now have a DNF of pairs where the left component is the type of
     a label l and the right component the rest of the record types.
     split returns a simplified DNF where the intersection are pushed
     below the products.

      Given a type d and a label l, this function returns
      a list of pairs : the first component represents the disjoint union
      of types associated to l
      the second projection the remaining types (records \ l)
  *)
  let split (d : descr) l = TR.boolean (aux_split d l)

  (* same as above, but the types for .l are disjoint *)
  let split_normal d l = TR.boolean_normal (aux_split d l)

  (* returns the union of the first projections. If one of the record
     had an absent l, then absent will end up in the result. *)
  let pi l d = TR.pi1 (split d l)

  (* Same but check that the resulting type does not contain absent *)
  let project d l =
    let t = pi l d in
    if t.absent then raise Not_found;
    t

  (* Same but erase the status of absent : meaning return the type of l
     if it is present.*)
  let project_opt d l =
    let t = pi l d in
    { t with absent = false; hash = -1 }

  let _condition d l t = TR.pi2_restricted t (split d l)

  (* TODO: eliminate this cap ... (record l absent_node) when
     not necessary. eg. { ..... } \ l *)

  (* get the pi2 part of split, that is the union of all the record types where
     l has been removed explicitely. And cap it with an open record where l
     is absent, to eliminate l from open record types where it was implicitely present.
  *)
  let remove_field d l = cap (TR.pi2 (split d l)) (record l absent_node)

  let _all_labels d =
    let res = ref LabelSet.empty in
    let aux (_, r) =
      let ls = LabelMap.domain r in
      res := LabelSet.cup ls !res
    in
    BoolRec.iter aux d.record;
    !res

  let first_label d =
    let min = ref Label.dummy in
    let aux (_, r) =
      match LabelMap.get r with
      | (l, _) :: _ -> min := Label.min l !min
      | _ -> ()
    in
    Rec.Dnf.iter aux d.record;
    !min

  let empty_cases d =
    let x =
      BoolRec.compute ~empty:0 ~any:3
        ~cup:(fun x y -> x lor y)
        ~cap:(fun x y -> x land y)
        ~diff:(fun a b -> a land lnot b)
        ~atom:(function
            | o, r ->
              assert (LabelMap.get r == []);
              if o then 3 else 1)
        d.record
    in
    (x land 2 <> 0, x land 1 <> 0)

  let has_empty_record d =
    BoolRec.compute ~empty:false ~any:true ~cup:( || ) ~cap:( && )
      ~diff:(fun a b -> a && not b)
      ~atom:(function
          | _, r -> List.for_all (fun (_, t) -> (descr t).absent) (LabelMap.get r))
      d.record

  (*TODO: optimize merge
    - pre-compute the sequence of labels
    - remove empty or full { l = t }
  *)

  let merge d1 d2 =
    let res = ref empty in
    let rec aux accu d1 d2 =
      let l = Label.min (first_label d1) (first_label d2) in
      if l == Label.dummy then
        let some1, none1 = empty_cases d1
        and some2, none2 = empty_cases d2 in
        let _none = none1 && none2
        and some = some1 || some2 in
        let accu = LabelMap.from_list (fun _ _ -> assert false) accu in
        (* approx for the case (some && not none) ... *)
        res := cup !res (record_fields (some, accu))
      else
        let l1 = split d1 l
        and l2 = split d2 l in
        let loop (t1, d1) (t2, d2) =
          let t =
            if t2.absent then cup t1 { t2 with absent = false; hash = -1 }
            else t2
          in
          aux ((l, cons t) :: accu) d1 d2
        in
        List.iter (fun x -> List.iter (loop x) l2) l1
    in

    aux [] d1 d2;
    !res

  let get d =
    let rec aux r accu d =
      let l = first_label d in
      if l == Label.dummy then
        let o1, o2 = empty_cases d in
        if o1 || o2 then (LabelMap.from_list_disj r, o1, o2) :: accu else accu
      else
        List.fold_left
          (fun accu (t1, t2) ->
             let x = (t1.absent, { t1 with absent = false }) in
             aux ((l, x) :: r) accu t2)
          accu (split d l)
    in
    aux [] [] d

  type t = TR.t

  let focus = split_normal
  let get_this r = { (TR.pi1 r) with absent = false; hash = -1 }

  let need_others = function
    | _ :: _ :: _ -> true
    | _ -> false

  let constraint_on_others r t1 =
    List.fold_left
      (fun accu (d1, d2) -> if disjoint d1 t1 then accu else cap accu d2)
      any_record r
end

let memo_normalize = ref DescrMap.empty

let rec rec_normalize d =
  try DescrMap.find d !memo_normalize with
  | Not_found ->
    let n = make () in
    memo_normalize := DescrMap.add d n !memo_normalize;
    let times =
      List.fold_left
        (fun accu (d1, d2) ->
           BoolPair.cup accu
             (BoolPair.atom (rec_normalize d1, rec_normalize d2)))
        BoolPair.empty (Product.normal d)
    in
    let xml =
      List.fold_left
        (fun accu (d1, d2) ->
           BoolPair.cup accu
             (BoolPair.atom (rec_normalize d1, rec_normalize d2)))
        BoolPair.empty
        (Product.normal ~kind:`XML d)
    in
    let record = d.record in
    define n { d with times; xml; record; hash = -1 };
    n

let normalize n = descr (internalize (rec_normalize n))

module Arrow = struct
  let trivially_arrow t =
    if subtype Function.any t then `Arrow
    else if is_empty { empty with arrow = t.arrow; hash = -1 } then `NotArrow
    else `Other

  let check_simple left (s1, s2) =
    let rec aux accu1 accu2 = function
      | (t1, t2) :: left ->
        let accu1' = diff_t accu1 t1 in
        if non_empty accu1' then aux accu1 accu2 left;
        let accu2' = cap_t accu2 t2 in
        if non_empty accu2' then aux accu1 accu2 left
      | [] -> raise NotEmpty
    in
    let accu1 = descr s1 in
    is_empty accu1
    ||
    try
      aux accu1 (diff any (descr s2)) left;
      true
    with
    | NotEmpty -> false

  let check_line_non_empty (left, right) =
    not (List.exists (check_simple left) right)

  let sample t =
    let left, _right =
      List.find check_line_non_empty (Function.Dnf.get t.arrow)
    in
    List.fold_left
      (fun accu (t, s) -> cap accu (arrow t s))
      { empty with arrow = any.arrow; hash = -1 }
      left

  (* [check_strenghten t s]
     Assume that [t] is an intersection of arrow types
     representing the interface of an abstraction;
     check that this abstraction has type [s] (otherwise raise Not_found)
     and returns a refined type for this abstraction.
  *)

  let _check_strenghten t s =
    (*
    let left = match (BoolPair.get t.arrow) with [ (p,[]) ] -> p | _ -> assert false in
    let rec aux = function
      | [] -> raise Not_found
      | (p,n) :: rem ->
	  if (List.for_all (fun (a,b) -> check_simple left a b) p) &&
	    (List.for_all (fun (a,b) -> not (check_simple left a b)) n) then
	      { empty with arrow = Obj.magic [ (SortedList.cup left p, n) ] }  (* rework this ! *)
	  else aux rem
    in
    aux (BoolPair.get s.arrow)
*)
    if subtype t s then t else raise Not_found

  let check_simple_iface left s1 s2 =
    let rec aux accu1 accu2 = function
      | (t1, t2) :: left ->
        let accu1' = diff accu1 t1 in
        if non_empty accu1' then aux accu1 accu2 left;
        let accu2' = cap accu2 t2 in
        if non_empty accu2' then aux accu1 accu2 left
      | [] -> raise NotEmpty
    in
    let accu1 = descr s1 in
    is_empty accu1
    ||
    try
      aux accu1 (diff any (descr s2)) left;
      true
    with
    | NotEmpty -> false

  let check_iface iface s =
    let rec aux = function
      | [] -> false
      | (p, n) :: rem ->
        List.for_all (fun (a, b) -> check_simple_iface iface a b) p
        && List.for_all (fun (a, b) -> not (check_simple_iface iface a b)) n
        || aux rem
    in
    aux (Function.Dnf.get s.arrow)

  type t = descr * (descr * descr) list list

  let get t =
    List.fold_left
      (fun ((dom, arr) as accu) (left, right) ->
         if check_line_non_empty (left, right) then
           let left = List.map (fun (t, s) -> (descr t, descr s)) left in
           let d = List.fold_left (fun d (t, _) -> cup d t) empty left in
           (cap dom d, left :: arr)
         else accu)
      (any, []) (BoolPair.get t.arrow)

  let domain (dom, _) = dom

  let apply_simple t result left =
    let rec aux result accu1 accu2 = function
      | (t1, s1) :: left ->
        let result =
          let accu1 = diff accu1 t1 in
          if non_empty accu1 then aux result accu1 accu2 left else result
        in
        let result =
          let accu2 = cap accu2 s1 in
          aux result accu1 accu2 left
        in
        result
      | [] -> if subtype accu2 result then result else cup result accu2
    in
    aux result t any left

  let apply (_, arr) t =
    if is_empty t then empty else List.fold_left (apply_simple t) empty arr

  let need_arg (_dom, arr) =
    List.exists
      (function
        | [ _ ] -> false
        | _ -> true)
      arr

  let apply_noarg (_, arr) =
    List.fold_left
      (fun accu -> function
         | [ (_t, s) ] -> cup accu s
         | _ -> assert false)
      empty arr

  let is_empty (_, arr) = arr == []
end

let rec tuple = function
  | [ t1; t2 ] -> times t1 t2
  | t :: tl -> times t (cons (tuple tl))
  | _ -> failwith "tuple: invalid length"

let rec_of_list o l =
  let map =
    LabelMap.from_list
      (fun _ _ -> failwith "rec_of_list: duplicate fields")
      (List.map
         (fun (opt, qname, typ) ->
            (qname, cons (if opt then Record.or_absent typ else typ)))
         l)
  in
  record_fields (o, map)

let empty_closed_record = rec_of_list false []
let empty_open_record = Rec.any

let cond_partition univ qs =
  let add accu (t, s) =
    let t = if subtype t s then t else cap t s in
    if subtype s t || is_empty t then accu
    else
      let aux accu u =
        let c = cap u t in
        if is_empty c || subtype (cap u s) t then u :: accu
        else c :: diff u t :: accu
      in
      List.fold_left aux [] accu
  in
  List.fold_left add [ univ ] qs
