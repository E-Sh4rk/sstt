type _ constr =
  | Cup : v list constr
  | Cap : v list constr
  | Diff : (v * v) constr
  | Type : Types.t constr
  | Times : (v * v) constr
  | Xml : (v * v) constr
  | Arrow : (v * v) constr
  | Record : ((Ident.label * bool * v) list * bool) constr

and rhs = Rhs : ('a constr * 'a) -> rhs

and v = {
  id : int;
  mutable def : rhs;
  mutable node : Types.Node.t option;
}

module V : Custom.T with type t = v = struct
  type t = v

  let hash v = v.id
  let equal v1 v2 = v1 == v2
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let dump ppf v = Format.fprintf ppf "<%d>" v.id
  let check _ = ()
end

module DescrHash = Hashtbl.Make (struct
  type t = Types.t

  let equal = Types.equal
  let hash = Types.hash
end)

module VHash = Hashtbl.Make (V)

let op_l init op = function
  | [] -> init
  | e :: ll -> List.fold_left (fun acc e -> op acc e) e ll

let dump ppf v =
  let seen = VHash.create 16 in
  let open Format in
  let pp_sep ppf () = fprintf ppf ",@ " in
  let rec loop ppf v =
    if VHash.mem seen v then fprintf ppf "X_%d" v.id
    else (
      VHash.add seen v ();
      fprintf ppf "X_%d=" v.id;
      loop_def v)
  and loop_def v =
    match v.def with
    | Rhs (Type, d) -> fprintf ppf "@[Type(%a)@]" Print.print d
    | Rhs (Record, (fields, op)) ->
        fprintf ppf "@[Record(%s,[" (if op then "open" else "close");
        fprintf ppf "%a"
          (pp_print_list ~pp_sep (fun ppf (lab, opt, t) ->
               fprintf ppf "%a=%s%a" Ident.Label.print_attr lab
                 (if opt then "?" else "")
                 loop t))
          fields;
        fprintf ppf ")@]"
    | Rhs (Cup, vl) -> pp_lst "U" ppf (vl : v list)
    | Rhs (Cap, vl) -> pp_lst "I" ppf (vl : v list)
    | Rhs (Times, (v1, v2)) -> pp_binop ppf "Times" v1 v2
    | Rhs (Xml, (v1, v2)) -> pp_binop ppf "Xml" v1 v2
    | Rhs (Arrow, (v1, v2)) -> pp_binop ppf "Arrow" v1 v2
    | Rhs (Diff, (v1, v2)) -> pp_binop ppf "Diff" v1 v2
  and pp_binop ppf o v1 v2 = fprintf ppf "@[%s(%a,@ %a)@]" o loop v1 loop v2
  and pp_lst s ppf l =
    fprintf ppf "@[%s[" s;
    fprintf ppf "%a" (pp_print_list ~pp_sep loop) l;
    fprintf ppf "]@]"
  in

  loop ppf v

let forward () = { id = Oo.id (object end); def = Rhs (Cup, []); node = None }
let def v x = v.def <- x

let cons d =
  let v = forward () in
  def v d;
  v

module HashKeyList (X : Custom.T) : Custom.T with type t = X.t list = struct
  type t = X.t list

  let equal l1 l2 =
    let l1 = List.stable_sort X.compare l1 in
    let l2 = List.stable_sort X.compare l2 in
    try List.for_all2 X.equal l1 l2 with
    | _ -> false

  let hash l =
    let l = List.stable_sort X.compare l in
    List.fold_left (fun acc x -> acc + (17 * X.hash x)) 0 l

  let compare l1 l2 =
    let l1 = List.stable_sort X.compare l1 in
    let l2 = List.stable_sort X.compare l2 in
    let rec loop l1 l2 =
      if l1 == l2 then 0
      else
        match (l1, l2) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x1 :: ll1, x2 :: ll2 ->
            let c = X.compare x1 x2 in
            if c == 0 then loop ll1 ll2 else c
    in
    loop l1 l2

  let dump ppf x = Format.fprintf ppf "%a" (Custom.Print.pp_list X.dump) x
  let check _ = ()
end

module V2 = Custom.Pair (V) (V)
module VList = HashKeyList (V)

module Fields = HashKeyList (struct
  type t = Ident.label * bool * v

  let hash (i, b, v) = Ident.Label.hash i + (17 * Obj.magic b) + (253 * v.id)

  let equal (i1, b1, v1) (i2, b2, v2) =
    Ident.Label.equal i1 i2 && b1 == b2 && v1 == v2

  let compare (i1, b1, v1) (i2, b2, v2) =
    let c1 = Ident.Label.compare i1 i2 in
    if c1 != 0 then c1
    else
      let c2 = Stdlib.compare b1 b2 in
      if c2 != 0 then c2 else Stdlib.compare v1.id v2.id

  let dump ppf (a, b, c) =
    Format.fprintf ppf "%a=%s %a" Ident.Label.dump a
      (if b then "?" else "")
      V.dump c

  let check _ = ()
end)

module VRec = Custom.Pair (Fields) (Custom.Bool)
module V2Hash = Hashtbl.Make (V2)
module VListHash = Hashtbl.Make (VList)
module VRecHash = Hashtbl.Make (VRec)

let do_memo (type t) (module H : Hashtbl.S with type key = t) f =
  let memo = H.create 16 in
  fun x ->
    try H.find memo x with
    | Not_found ->
        let res = f x in
        H.add memo x res;
        res

let wrap_tuple app f =
  let res = app (fun (a, b) -> f a b) in
  fun a b -> res (a, b)

let memo_t f = do_memo (module DescrHash) f
let memo_vlist f = do_memo (module VListHash) f
let memo_v2 f = wrap_tuple (do_memo (module V2Hash)) f
let memo_vrec f = wrap_tuple (do_memo (module VRecHash)) f
let memo_v f = do_memo (module VHash) f
let ty = memo_t (fun d -> cons (Rhs (Type, d)))
let cup = memo_vlist (fun vl -> cons (Rhs (Cup, vl)))
let times = memo_v2 (fun d1 d2 -> cons (Rhs (Times, (d1, d2))))
let xml = memo_v2 (fun d1 d2 -> cons (Rhs (Xml, (d1, d2))))
let arrow = memo_v2 (fun d1 d2 -> cons (Rhs (Arrow, (d1, d2))))
let record = memo_vrec (fun fields op -> cons (Rhs (Record, (fields, op))))
let cap = memo_vlist (fun vl -> cons (Rhs (Cap, vl)))
let diff = memo_v2 (fun d1 d2 -> cons (Rhs (Diff, (d1, d2))))
let tany = ty Types.any

let diff_t d1 d2 =
  match (d1.def, d2.def) with
  | Rhs (Type, d1), Rhs (Type, d2) -> ty (Types.diff d1 d2)
  | _, Rhs (Cup, []) -> d1
  | _, Rhs (Cap, []) -> ty Types.empty
  | _ -> diff d1 d2

let[@ocaml.warning "-32"] neg = memo_v (fun d -> diff_t tany d)

let rec make_descr seen v =
  try VHash.find seen v with
  | Not_found ->
      let () = VHash.add seen v Types.empty in
      let res =
        match v.def with
        | Rhs (Type, d) -> (d : Types.t)
        | Rhs (Cup, vl) ->
            Types.(op_l empty Types.cup (List.map (make_descr seen) vl))
        | Rhs (Cap, vl) -> Types.(op_l any cap (List.map (make_descr seen) vl))
        | Rhs (Times, (v1, v2)) ->
            Types.times (make_node seen v1) (make_node seen v2)
        | Rhs (Xml, (v1, v2)) ->
            Types.xml (make_node seen v1) (make_node seen v2)
        | Rhs (Arrow, (v1, v2)) ->
            Types.arrow (make_node seen v1) (make_node seen v2)
        | Rhs (Record, (fields, op)) ->
            let tfields =
              List.map
                (fun (l, abs, v) ->
                  let v = if abs then cup [ v; ty Types.Absent.any ] else v in
                  let n = make_node seen v in
                  (l, n))
                fields
            in
            Types.record_fields
              (op, Ident.LabelMap.from_list (fun _ _ -> assert false) tfields)
        | Rhs (Diff, (v1, v2)) -> (
            match v2.def with
            | Rhs (Cup, []) -> make_descr seen v1
            | Rhs (Cap, []) -> Types.empty
            | _ -> Types.(diff (make_descr seen v1) (make_descr seen v2)))
      in
      VHash.replace seen v res;
      res

and make_node _seen v =
  match v.node with
  | Some n -> n
  | None ->
      let n = Types.make () in
      v.node <- Some n;
      let d = make_descr (VHash.create 16) v in
      Types.define n d;
      n

let make_node v = make_node (VHash.create 16) v
let define v1 v2 = def v1 (Rhs (Cup, [ v2 ]))
let solve v = Types.internalize (make_node v)

let cup_l = function
  | [] -> ty Types.empty
  | [ v ] -> v
  | l -> cup l

(* Circumvent weaker typing of GADTs in OCaml 4.07.1 *)
let int_of_tag (type a) (x : a constr) : int =
  match x with
  | Cup -> 0
  | Cap -> 1
  | Diff -> 2
  | Type -> 3
  | Times -> 4
  | Xml -> 5
  | Arrow -> 6
  | Record -> 7

let[@ocaml.warning "-32"] first_constr = int_of_tag Times

let do_line acc any_k cons (lpos, lneg) =
  let neg = cup_l (List.rev_map cons lneg) in
  let pos = cap (any_k :: List.map cons lpos) in
  diff_t pos neg :: acc

let do_line_cup any_k cons at = cup_l @@ do_line [] any_k cons at

module VarHash = Hashtbl.Make (Var)

let vmemo = VarHash.create 16

let ty_var v =
  try VarHash.find vmemo v with
  | Not_found ->
      let ty = Types.var v in
      VarHash.add vmemo v ty;
      ty

let decompose ?(stop = fun _ -> None) t =
  let memo = DescrHash.create 17 in
  let app_stop t f =
    match stop t with
    | Some v -> v
    | None -> f t
  in

  let do_vars acc do_atom dnf =
    List.fold_left
      (fun acc (vars, at) ->
        do_line acc (do_atom at) (fun v -> app_stop (ty_var v) ty) vars)
      acc dnf
  in
  let rec loop t =
    match stop t with
    | Some s -> s
    | None -> (
        try DescrHash.find memo t with
        | Not_found ->
            let node_t = forward () in
            let () = DescrHash.add memo t node_t in
            let rhs = loop_struct t in
            node_t.def <- rhs.def;
            node_t)
  and loop_struct t =
    cup_l
    @@ Iter.fold
         (fun acc pack t ->
           match pack with
           | Iter.Int m
           | Char m
           | Atom m
           | Abstract m ->
               let module K = (val m) in
               do_vars acc
                 (fun at -> app_stop K.(mk (Dnf.mono at)) ty)
                 K.(Dnf.get_partial (get_vars t))
           | Times m
           | Xml m
           | Function m ->
               let cons =
                 match pack with
                 | Times _ -> times
                 | Xml _ -> xml
                 | _ -> arrow
               in
               let module K = (val m) in
               do_vars acc
                 (do_line_cup (ty K.any) (fun (d1, d2) ->
                      let tt = K.(mk (Dnf.atom (d1, d2))) in
                      app_stop tt (fun _ ->
                          cons
                            (app_stop (Types.descr d1) loop)
                            (app_stop (Types.descr d2) loop))))
                 K.(Dnf.get_full (get_vars t))
           | Record _ ->
               do_vars acc
                 (do_line_cup (ty Types.Rec.any) (fun (op, fields) ->
                      let tt = Types.Rec.(mk (Dnf.atom (op, fields))) in
                      app_stop tt (fun _ ->
                          record
                            (Ident.LabelMap.mapi_to_list
                               (fun l t ->
                                 let t = Types.descr t in
                                 (l, Types.Record.has_absent t, app_stop t loop))
                               fields)
                            op)))
                 Types.Rec.(Dnf.get_full (get_vars t))
           | Absent ->
               if Types.Record.has_absent t then ty Types.Absent.any :: acc
               else acc)
         [] t
  in
  loop t
