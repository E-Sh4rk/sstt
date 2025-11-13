type t = Types.t Var.Map.map

let from_list l =
  Var.Map.from_list (fun _ _ -> failwith "Subst.from_list : invalid_mapping") l

module MemHash = Hashtbl.Make (struct
  type t = Types.t

  let equal = ( == )
  let hash = Types.hash
end)

let print_gen ppf pr_e vmap = Custom.Print.pp_set pr_e ppf vmap

let print ppf vmap =
  let open Format in
  print_gen ppf
    (fun ppf (v, t) -> fprintf ppf "%a:=%a" Var.print v Print.print t)
    (Var.Map.get vmap)

let print_list ppf vmap = Custom.Print.pp_list print ppf vmap
let update_polarity dir x = if x = dir then dir else `Both

let has_flag memo t v =
  match (MemHash.find memo t, v) with
  | (`False | `Both), false
  | (`True | `Both), true ->
      true
  | _ -> false
  | exception Not_found -> false

let add_flag memo t v =
  let nflag =
    match (MemHash.find memo t, v) with
    | `True, true -> `True
    | `False, false -> `False
    | _ -> `Both
    | exception Not_found -> if v then `True else `False
  in
  MemHash.replace memo t nflag

module VarHMap = Hashtbl.Make (Var)

(**  'a -> Int  ⇒ Any -> Int
       Any\'a -> Int =>   Any -> Int

       Int -> 'a  ⇒ Int -> Empty
       Int -> Int\'a ⇒

  *)

let _p_string = function
  | `Neg -> "`Neg"
  | `Pos -> "`Pos"
  | `Both -> "`Both"

let pr_pol fmt pol =
  Custom.Print.pp_list
    (fun ppf (v, pol) -> Format.fprintf ppf "%a:%s" Var.print v (_p_string pol))
    fmt (Var.Map.get pol)

let _p_string = function
  | `Neg -> "`Neg"
  | `Pos -> "`Pos"
  | `Both -> "`Both"

let _pr_pol fmt pol =
  Custom.Print.pp_list
    (fun ppf (v, pol) -> Format.fprintf ppf "%a:%s" Var.print v (_p_string pol))
    fmt (Var.Map.get pol)

let vars_gen switch_neg recurse t =
  let neg = if switch_neg then not else fun x -> x in
  let memo = MemHash.create 16 in
  let vset = ref Var.Map.empty in
  let add_vars pol (v1, v2) =
    let v1 = Var.Set.unsafe_cast v1 in
    let v2 = Var.Set.unsafe_cast v2 in
    let v1, v2 = if pol then (v1, v2) else (v2, v1) in
    if not (Var.Set.is_empty v1) then
      vset := Var.Map.merge_set update_polarity !vset v1 `Pos;
    if not (Var.Set.is_empty v2) then
      vset := Var.Map.merge_set update_polarity !vset v2 `Neg
  in

  let do_dnf =
    if recurse then fun pol do_mono dnf ->
      List.iter
        (fun (vars, mono) ->
          add_vars pol vars;
          do_mono pol mono)
        dnf
    else fun pol _ dnf -> List.iter (fun (vars, _) -> add_vars pol vars) dnf
  in
  let rec loop pol t =
    if not (has_flag memo t pol) then begin
      add_flag memo t pol;
      loop_descr pol t
    end
  and loop_descr pol t =
    Iter.iter
      (fun pack t ->
        match pack with
        | Int m
        | Atom m
        | Char m
        | Abstract m ->
            let module K = (val m) in
            do_dnf pol (fun _ _ -> ()) K.(Dnf.get_partial (get_vars t))
        | Times m
        | Xml m ->
            let module K = (val m) in
            do_dnf pol do_pair K.(Dnf.get_full (get_vars t))
        | Function m ->
            let module K = (val m) in
            do_dnf pol do_fun K.(Dnf.get_full (get_vars t))
        | Record m ->
            let module K = (val m) in
            do_dnf pol do_record K.(Dnf.get_full (get_vars t))
        | Absent -> ())
      t
  and do_pair pol (pa, na) =
    List.iter (loop_pair pol) pa;
    List.iter (loop_pair (neg pol)) na
  and do_fun pol (pa, na) =
    List.iter (loop_fun pol) pa;
    List.iter (loop_fun (neg pol)) na
  and do_record pol (pa, na) =
    List.iter (fun (_, map) -> Ident.LabelMap.iter (loop_node pol) map) pa;
    List.iter (fun (_, map) -> Ident.LabelMap.iter (loop_node (neg pol)) map) na
  and loop_node pol n = loop pol (Types.descr n)
  and loop_pair pol (n1, n2) =
    loop_node pol n1;
    loop_node pol n2
  and loop_fun pol (n1, n2) =
    loop_node (not pol) n1;
    loop_node pol n2
  in
  loop true t;
  !vset

let vars t = Var.Map.domain (vars_gen false true t)
let top_vars t = Var.Map.domain (vars_gen false false t)

let all_kinds : (module Types.Kind) list =
  [
    (module Types.Atom);
    (module Types.Char);
    (module Types.Int);
    (module Types.Times);
    (module Types.Xml);
    (module Types.Function);
    (module Types.Rec);
    (module Types.Abstract);
  ]

let dummy_var = Var.mk ~kind:`generated ":DUMMY:"

let extract_var ?(v = dummy_var) t =
  try
    let v, p, i, n =
      List.fold_left
        (fun acc (module M : Types.Kind) ->
          let v, ap, ai, an = acc in
          match M.(Dnf.extract_var (get_vars t)) with
          | None -> raise_notrace Exit
          | Some (w, bp, bi, bn) ->
              if v == dummy_var || Var.equal v w then
                ( w,
                  Types.cup ap (M.mk bp),
                  Types.cup ai (M.mk bi),
                  Types.cup an (M.mk bn) )
              else raise_notrace Exit)
        Types.(v, empty, empty, empty)
        all_kinds
    in
    if Types.is_empty i then
      if Types.(is_empty (neg p)) && Types.is_empty n then `Pos v
      else if Types.is_empty p && Types.(is_empty (neg n)) then `Neg v
      else `Not_var
    else `Not_var
  with
  | Exit -> `Not_var

let _v_string = function
  | `Pos v -> Format.asprintf "`Pos %a" Var.print v
  | `Neg v -> Format.asprintf "`Neg %a" Var.print v
  | _ -> "`Not_var"

let check_var_aux (vrs : Var.Set.t) t =
  let res =
    match (vrs :> Var.t list) with
    | []
    | _ :: _ :: _ ->
        `Not_var
    | [ v ] -> extract_var ~v t
  in
  res

let _vtype map v = Var.Map.assoc_present v map

(** To prevent spurious operations such as cup e empty or cap e any*)

let apply subst t =
  let dom = Var.Map.domain subst in
  let hmap = VarHMap.create 16 in
  let () = Var.Map.iteri (fun v t -> VarHMap.add hmap v t) subst in
  let vtype v = VarHMap.find hmap v in
  let v =
    Positive.decompose
      ~stop:(fun t ->
        let vrs = vars t in
        if Var.Set.disjoint vrs dom then Some (Positive.ty t)
        else
          match check_var_aux vrs t with
          | `Pos v -> Some (Positive.ty (vtype v))
          | `Neg v ->
              Some
                (Positive.diff (Positive.ty Types.any) (Positive.ty (vtype v)))
          | _ -> None)
      t
  in
  Types.descr (Positive.solve v)

let apply_full subst t =
  let subst = Var.Map.map (fun t -> (t, vars t)) subst in
  let rec loop subst acc =
    if Var.Map.is_empty subst then acc
    else
      let dom = Var.Map.domain subst in
      let sok, rem =
        Var.Map.split (fun _ (_, vt) -> Var.Set.disjoint vt dom) subst
      in
      if Var.Map.is_empty sok then
        failwith "Types.Subst.apply_full: cyclic substitutions";
      loop rem (sok :: acc)
  in

  List.fold_left (fun t s -> apply (Var.Map.map fst s) t) t (loop subst [])

let check_var t = check_var_aux (vars t) t

let _is_var_aux t =
  let var = ref None in
  let update s v =
    match !var with
    | None -> var := Some (s, v)
    | Some (ss, vv) ->
        if ss = s && Var.equal vv v then () else raise_notrace Not_found
  in
  let update_vars (module M : Types.Kind) t =
    let dnf = M.Dnf.get_partial (M.get_vars t) in
    let no_vars =
      M.(
        mk
        @@ List.fold_left
             (fun acc ((_, _), m) -> Dnf.cup acc (Dnf.mono m))
             Dnf.empty dnf)
    in
    let is_kind_any = Types.equal M.any no_vars in
    match dnf with
    | [ (([ vv ], []), _) ] when is_kind_any -> update true vv
    | [ (([], [ vv ]), _) ] when is_kind_any -> update false vv
    | _ -> raise_notrace Not_found
  in
  try
    Iter.iter
      (fun pack t ->
        match pack with
        | Iter.Abstract m
        | Iter.Int m
        | Iter.Atom m
        | Iter.Char m ->
            update_vars m t
        | Iter.Function m
        | Iter.Xml m
        | Iter.Times m ->
            update_vars (module (val m) : Types.Kind) t
        | Iter.Record m -> update_vars (module (val m) : Types.Kind) t
        | Iter.Absent -> ()
        (* can a variable have the absent flag ? *))
      t;
    !var
  with
  | Not_found -> None

let is_var t =
  match extract_var t with
  | `Not_var -> false
  | _ -> true

let extract t =
  match extract_var t with
  | `Pos v -> (v, true)
  | `Neg v -> (v, false)
  | _ -> assert false

let refresh pvars t =
  let all_vars = Var.Set.filter (fun v -> Var.kind v <> `weak) (vars t) in
  let all_vars = Var.Set.diff all_vars pvars in
  if Var.Set.is_empty all_vars then t
  else
    let subst =
      Var.Map.map_from_slist
        (fun v -> Types.var Var.(mk ~kind:(kind v) (name v)))
        all_vars
    in
    apply subst t

let solve_rectype t alpha =
  let x = Positive.forward () in
  let v =
    Positive.decompose
      ~stop:(fun t ->
        match check_var t with
        | `Pos v when Var.equal v alpha -> Some x
        | `Neg v when Var.equal v alpha ->
            (* this will most likely yielf an empty type,
                one should not perform recursion below diff.
            *)
            Some Positive.(diff (ty Types.any) x)
        | _ -> None)
      t
  in
  Positive.define x v;
  Types.descr (Positive.solve x)

let clean_type ?(pos = Types.empty) ?(neg = Types.any) delta t =
  let polarities = vars_gen false true t in
  let clean_subst =
    try
      Var.Map.fold
        (fun v pol acc ->
          match pol with
          | `Both -> acc
          | `Pos -> if Var.Set.mem delta v then acc else Var.Map.add v pos acc
          | `Neg -> if Var.Set.mem delta v then acc else Var.Map.add v neg acc)
        polarities Var.Map.empty
    with
    | _ -> failwith (Format.asprintf "%a" pr_pol polarities)
  in
  apply_full clean_subst t

let replace_vars any pos vars vpos vneg =
  let rec loop pol vlist acc =
    match vlist with
    | v :: vvlist ->
        if Var.Set.mem vars v then
          (* monomorphic variable, don't touch*)
          let t = if pol then Types.var v else Types.(neg (Types.var v)) in
          loop pol vvlist (Types.cap t acc)
        else if pol == pos then
          (* positive variable in positive position or
             negative var in negative position are replaced by any *)
          loop pol vvlist acc
        else Types.empty
    | [] -> acc
  in
  let tpos = loop true vpos any in
  if Types.is_empty tpos then Types.empty else loop false vneg tpos

let do_vars acc dnf any pos vars k mk =
  let res =
    List.fold_left
      (fun acc ((vpos, vneg), mono) ->
        let tvars = replace_vars any pos vars vpos vneg in
        if Types.is_empty tvars then acc else k acc mono tvars)
      acc dnf
  in
  mk res

let id_pol x = x
let neg_pol x = not x

let rec min_max_type pos_memo neg_memo pos vars t =
  let memo = if pos then pos_memo else neg_memo in
  try MemHash.find memo t with
  | Not_found ->
      let v = Positive.forward () in
      MemHash.add memo t v;
      let def =
        Positive.cup
        @@ Iter.fold
             (fun acc pack t ->
               let d =
                 match pack with
                 | Iter.Int m
                 | Iter.Char m
                 | Iter.Atom m
                 | Iter.Abstract m ->
                     let module M = (val m) in
                     let dnf = M.Dnf.get_partial (M.get_vars t) in
                     do_vars Types.empty dnf M.any pos vars
                       (fun acc mono tvars ->
                         Types.cup acc
                           (Types.cap tvars (M.mk (M.Dnf.mono mono))))
                       Positive.ty
                 | Iter.Times m
                 | Iter.Xml m
                 | Iter.Function m ->
                     let module M = (val m) in
                     let fix_pos, make =
                       if M.any == Types.Xml.any then (id_pol, Positive.xml)
                       else if M.any == Types.Times.any then
                         (id_pol, Positive.times)
                       else (neg_pol, Positive.arrow)
                     in
                     let dnf = M.Dnf.get_full (M.get_vars t) in
                     do_vars [] dnf M.any pos vars
                       (fun acc (lpos, lneg) tvars ->
                         Positive.diff
                           (Positive.cap
                           @@ Positive.ty tvars
                              :: do_prod pos_memo neg_memo make vars pos fix_pos
                                   id_pol lpos)
                           (Positive.cup
                              (do_prod pos_memo neg_memo make vars pos fix_pos
                                 neg_pol lneg))
                         :: acc)
                       Positive.cup
                 | Iter.Record m ->
                     let module M = (val m) in
                     let dnf = M.Dnf.get_full (M.get_vars t) in
                     do_vars [] dnf M.any pos vars
                       (fun acc (lpos, lneg) tvars ->
                         Positive.diff
                           (Positive.cap
                           @@ Positive.ty tvars
                              :: do_record pos_memo neg_memo vars pos id_pol
                                   lpos)
                           (Positive.cup
                              (do_record pos_memo neg_memo vars pos neg_pol lneg))
                         :: acc)
                       Positive.cup
                 | Absent -> Positive.ty (Types.Record.or_absent Types.empty)
               in
               d :: acc)
             [] t
      in
      Positive.define v def;
      def

and do_prod pos_memo neg_memo make vars pos fix_pos switch_pol line =
  List.map
    (fun (n1, n2) ->
      let v1 =
        min_max_type pos_memo neg_memo
          (switch_pol (fix_pos pos))
          vars (Types.descr n1)
      in
      let v2 =
        min_max_type pos_memo neg_memo (switch_pol pos) vars (Types.descr n2)
      in
      make v1 v2)
    line

and do_record pos_memo neg_memo vars pos switch_pol line =
  List.map
    (fun (b, lm) ->
      Positive.record
        (Ident.LabelMap.mapi_to_list
           (fun l n ->
             ( l,
               Types.Record.has_absent (Types.descr n),
               min_max_type pos_memo neg_memo (switch_pol pos) vars
                 (Types.descr n) ))
           lm)
        b)
    line

let min_max pos delta t =
  if Var.Set.is_empty (vars t) then t
  else
    let hp = MemHash.create 16 in
    let hn = MemHash.create 16 in
    let v = min_max_type hp hn pos delta t in
    let res = Types.descr @@ Positive.solve v in
    let _debug () =
      let debug_table h =
        MemHash.iter
          (fun t v ->
            let s = Types.descr @@ Positive.solve v in
            Format.eprintf "@[@[%a@] → @[%a@]@]@\n" Print.print t Print.print s)
          h
      in
      Format.eprintf "@[DEBUG %s (%a) = %a:@[@\n"
        (if pos then "max" else "min")
        Print.print t Print.print res;
      Format.eprintf "@[Descr:@\n";
      Format.eprintf "@[%a@]\n" Positive.dump v;
      Format.eprintf "@]\n";
      Format.eprintf "@[Positive:@\n";
      debug_table hp;
      Format.eprintf "@]@\n";
      Format.eprintf "@[Negative:@\n";
      debug_table hn;
      Format.eprintf "@]@\n";
      Format.eprintf "@]@]@\n"
    in
    res

let min_type vars t = min_max false vars t
let max_type vars t = min_max true vars t
let var_polarities = vars_gen true true
