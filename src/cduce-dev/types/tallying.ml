(* Implementation of
   Polymorphic Functions with Set-Theoretic TypesPart 2: Local Type Inference and
   Type Reconstruction, POPL2015.
*)

open Types

let cap_t d t = cap d (descr t)

let cap_product any_left any_right l =
  List.fold_left
    (fun (d1, d2) (t1, t2) -> (cap_t d1 t1, cap_t d2 t2))
    (any_left, any_right) l

type constr = Types.t * Types.t
(* lower and
   upper bounds *)

(* A comparison function between types that
   is compatible with subtyping. If types are
   not in a subtyping relation, use implementation
   defined order
*)
let compare_type t1 t2 =
  let inf12 = Types.subtype t1 t2 in
  let inf21 = Types.subtype t2 t1 in
  if inf12 && inf21 then 0
  else if inf12 then -1
  else if inf21 then 1
  else
    let c = Types.compare t1 t2 in
    assert (c <> 0);
    c

exception Step1Fail
exception Step2Fail

(* All algorithms are parametherized with a custom order on variables *)
module Make (V : sig
  include Custom.T with type t = Var.t

  val delta : Var.Set.t
end) =
struct
  (* A line is a conjunction of constraints. This correspond to a constraint set
     of the paper.
  *)
  module Line : sig
    type t

    val empty : t
    val singleton : Var.t -> constr -> t
    val is_empty : t -> bool
    val length : t -> int [@@ocaml.warning "-32"]
    val subsumes : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int [@@ocaml.warning "-32"]
    val add : Var.t -> constr -> t -> t [@@ocaml.warning "-32"]
    val join : t -> t -> t
    val fold : (Var.t -> constr -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (Var.t -> constr -> bool) -> t -> bool [@@ocaml.warning "-32"]
  end = struct
    module VSet = SortedList.Make (V)

    type t = constr VSet.Map.map

    let is_empty = VSet.Map.is_empty
    let length = VSet.Map.length

    (* a set of constraints m1 subsumes a set of constraints m2,
       that is the solutions for m1 contains all the solutions for
       m2 if:
       forall i1 <= v <= s1 in m1,
       there exists i2 <= v <= s2 in m2 such that i1 <= i2 <= v <= s2 <= s1
    *)
    let subsumes (map1 : t) (map2 : t) =
      let rec loop l1 l2 =
        match (l1, l2) with
        | [], _ -> true
        | _, [] -> false
        | (v1, (i1, s1)) :: ll1, (v2, (i2, s2)) :: ll2 ->
            let c = V.compare v1 v2 in
            (c > 0 && loop l1 ll2)
            || (c == 0 && subtype i1 i2 && subtype s2 s1 && loop ll1 ll2)
      in
      loop (VSet.Map.get map1) (VSet.Map.get map2)

    let print ppf map =
      let open Format in
      fprintf ppf "@[{";
      fprintf ppf "%a"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (fun ppf (v, (i, s)) ->
             fprintf ppf "@[%a <= %a <= %a@]" Print.print i Var.print v
               Print.print s))
        (VSet.Map.get map);
      fprintf ppf "}@]"

    let compare map1 map2 =
      VSet.Map.compare
        (fun (i1, s1) (i2, s2) ->
          let c = compare_type i1 i2 in
          if c == 0 then compare_type s1 s2 else c)
        map1 map2

    let add_both v (inf, sup) map =
      let new_i, new_s =
        try
          let old_i, old_s = VSet.Map.assoc v map in
          (cup old_i inf, cap old_s sup)
        with
        | Not_found -> (inf, sup)
      in
      VSet.Map.replace v (new_i, new_s) map

    let add_inf v tinf map =
      (* tinf < v *)
      if Subst.is_var tinf then
        let v2, pos = Subst.extract tinf in
        if V.compare v v2 <= 0 || Var.Set.mem V.delta v2 then
          (* v is smaller var, ok to use as key *)
          add_both v (tinf, any) map
        else if pos then add_both v2 (empty, var v) map
        else add_both v2 (neg (var v), any) map
      else add_both v (tinf, any) map

    let add_sup v tsup map (* v < tsup *) =
      if Subst.is_var tsup then
        let v2, pos = Subst.extract tsup in
        if V.compare v v2 <= 0 || Var.Set.mem V.delta v2 then
          (* v is smaller var than v2 ok to use as key *)
          add_both v (empty, tsup) map
        else if pos then add_both v2 (var v, any) map
        else add_both v2 (empty, neg (var v)) map
      else add_both v (empty, tsup) map

    let add v (inf, sup) map = add_inf v inf (add_sup v sup map)
    let empty = VSet.Map.empty
    let singleton v c = add v c empty

    let join map1 map2 =
      let rec loop l1 l2 =
        match (l1, l2) with
        | [], _ -> l2
        | _, [] -> l1
        | ((v1, (i1, s1)) as m1) :: ll1, ((v2, (i2, s2)) as m2) :: ll2 ->
            let c = V.compare v1 v2 in
            if c < 0 then m1 :: loop ll1 l2
            else if c > 0 then m2 :: loop l1 ll2
            else (v1, (Types.cup i1 i2, Types.cap s1 s2)) :: loop ll1 ll2
      in
      VSet.Map.unsafe_cast (loop (VSet.Map.get map1) (VSet.Map.get map2))

    let fold = VSet.Map.fold
    let for_all f m = List.for_all (fun (k, v) -> f k v) (VSet.Map.get m)
  end

  (** A set of lines, that is a set of sets of constraints from the paper. *)
  module ConstrSet : sig
    type t

    val singleton : Line.t -> t
    val single_var : Var.t -> constr -> t
    val elements : t -> Line.t list [@@ocaml.warning "-32"]
    val unsat : t
    val sat : t
    val is_unsat : t -> bool
    val is_sat : t -> bool [@@ocaml.warning "-32"]
    val print : Format.formatter -> t -> unit [@@ocaml.warning "-32"]
    val fold : (Line.t -> 'a -> 'a) -> t -> 'a -> 'a
    val union : t -> t -> t
    val inter : t -> t -> t
    val add : Line.t -> t -> t [@@ocaml.warning "-32"]
    val filter : (Line.t -> bool) -> t -> t [@@ocaml.warning "-32"]
  end = struct
    (* A set of constraint-sets is just a list of Lines,
       that are pairwise "non-subsumable"
    *)
    type t = Line.t list

    let elements t = t
    let empty = []

    let add m l =
      let rec loop m l acc =
        match l with
        | [] -> m :: acc
        | mm :: ll ->
            if Line.subsumes m mm then loop m ll acc
            else if Line.subsumes mm m then List.rev_append ll (mm :: acc)
            else loop m ll (mm :: acc)
      in
      loop m l []

    let unsat = empty
    let sat = [ Line.empty ]
    let is_empty l = l == []
    let is_unsat m = is_empty m

    let is_sat m =
      match m with
      | [ l ] when Line.is_empty l -> true
      | _ -> false

    let print ppf s =
      let open Format in
      fprintf ppf "@[[";
      fprintf ppf "%a"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@\n") Line.print)
        s;
      fprintf ppf "]@]"

    let fold f l a = List.fold_left (fun e a -> f a e) a l

    (* Square union : *)
    let union s1 s2 =
      match (s1, s2) with
      | [], _ -> s2
      | _, [] -> s1
      | _ ->
          (* Invariant: all elements of s1 (resp s2) are pairwise
             incomparable (they don't subsume one another)
             let e1 be an element of s1:
             - if e1 subsumes no element of s2, add e1 to the result
             - if e1 subsumes an element e2 of s2, add e1 to the
             result and remove e2 from s2
             - if an element e2 of s2 subsumes e1, add e2 to the
             result and remove e2 from s2 (and discard e1)

             once we are done for all e1, add the remaining elements from
             s2 to the result.
          *)
          let append e1 s2 result =
            let rec loop s2 accs2 =
              match s2 with
              | [] -> (accs2, e1 :: result)
              | e2 :: ss2 ->
                  if Line.subsumes e1 e2 then
                    (List.rev_append ss2 accs2, e1 :: result)
                  else if Line.subsumes e2 e1 then
                    (List.rev_append ss2 accs2, e2 :: result)
                  else loop ss2 (e2 :: accs2)
            in
            loop s2 []
          in
          let rec loop s1 s2 result =
            match s1 with
            | [] -> List.rev_append s2 result
            | e1 :: ss1 ->
                let new_s2, new_result = append e1 s2 result in
                loop ss1 new_s2 new_result
          in
          loop s1 s2 []

    (* Square intersection *)
    let inter s1 s2 =
      match (s1, s2) with
      | [], _
      | _, [] ->
          []
      | _ ->
          (* Perform the cartesian product. For each constraint m1 in s1,
             m2 in s2, we add Line.join m1 m2 to the result.
             Optimisations:
             - we use add to ensure that we do not add something that subsumes
             a constraint set that is already in the result
             - if m1 subsumes m2, it means that whenever m2 holds, so does m1, so
             we only add m2 (note that the condition is reversed w.r.t. union).
          *)
          fold
            (fun m1 acc1 -> fold (fun m2 -> add (Line.join m1 m2)) s2 acc1)
            s1 []

    let filter = List.filter
    let single_var v cs = [ Line.singleton v cs ]
    let singleton e = [ e ]
  end

  (** Generation of constraint [norm(t,M)] function of the paper. 
*)

  module GlobalMemo = Hashtbl.Make (Var.Set)
  module LocalMemo = Hashtbl.Make (Types)

  (* The global cache is indexed by a  set of monomorphic variables.
     The only situation where it is re-used is when the same type with the
     exact same variables (internally, not variables with the same name) is
     used several times.

     This situation often occurs in practice when applying the same polymorphic
     function in the body of another one (e.g. List.map f aplied to several times
     to lists with similar types).
  *)
  let global_memo = GlobalMemo.create 17

  let cons_to_type
      (type atom)
      (module K : Kind with type Dnf.atom = atom)
      (lpos, lneg) =
    let open K.Dnf in
    let p = List.fold_left (fun acc a -> cap acc (atom a)) any lpos in
    let n = List.fold_left (fun acc a -> cup acc (atom a)) empty lneg in
    K.mk (diff p n)

  let norm_basic cast _delta _mem t =
    let t = cast t in
    if is_empty t then ConstrSet.sat else ConstrSet.unsat

  let single b vpos vneg t =
    let accp = List.fold_left (fun acc v -> cap acc (var v)) t vpos in
    let accn = List.fold_left (fun acc v -> cup acc (var v)) empty vneg in
    let s = diff accp accn in
    if b then neg s else s

  let toplevel (type atom) delta mem (vpos, vneg) (t : atom) to_type norm_atom =
    let split_vars = List.partition (Var.Set.mem delta) in
    let vpos_mono, vpos_poly = split_vars vpos in
    let vneg_mono, vneg_poly = split_vars vneg in
    let vpos_poly = List.sort V.compare vpos_poly in
    let vneg_poly = List.sort V.compare vneg_poly in
    match (vpos_poly, vneg_poly) with
    | [], [] -> norm_atom delta mem t
    | x :: rem, [] ->
        let s = single true (rem @ vpos_mono) vneg (to_type t) in
        ConstrSet.single_var x (empty, s)
    | [], x :: rem ->
        let s = single false vpos (rem @ vneg_mono) (to_type t) in
        ConstrSet.single_var x (s, any)
    | x :: rem_pos, y :: rem_neg ->
        if V.compare x y < 0 then
          let s = single true (rem_pos @ vpos_mono) vneg (to_type t) in
          ConstrSet.single_var x (empty, s)
        else
          let s = single false vpos (rem_neg @ vneg_mono) (to_type t) in
          ConstrSet.single_var y (s, any)

  let fold_union acc delta mem to_type norm_atom dnf =
    try
      List.fold_left
        (fun acc ((vpos, vneg), atom) ->
          if ConstrSet.is_unsat acc then raise Exit
          else
            let top = toplevel delta mem (vpos, vneg) atom to_type norm_atom in
            ConstrSet.inter acc top)
        acc dnf
    with
    | Exit -> ConstrSet.unsat

  (** norm function that generates constraints. *)
  let rec norm delta mem t =
    try
      let finished, cst = LocalMemo.find mem t in
      if finished then cst else ConstrSet.sat
    with
    | Not_found ->
        if is_empty t then ConstrSet.sat
        else
          let vars = Subst.vars t in
          if Var.Set.subset vars delta then ConstrSet.unsat
          else if Subst.is_var t then
            let v, p = Subst.extract t in
            if Var.Set.mem delta v then ConstrSet.unsat
            else
              ConstrSet.single_var v (if p then (empty, empty) else (any, any))
          else begin
            LocalMemo.add mem t (false, ConstrSet.sat);
            let res =
              Iter.fold
                (fun acc pack t ->
                  if ConstrSet.is_unsat acc then acc
                  else
                    match pack with
                    | Iter.Int m
                    | Char m
                    | Atom m
                    | Abstract m ->
                        let module K = (val m) in
                        let to_type at = K.(mk (Dnf.mono at)) in
                        let dnf = K.(Dnf.get_partial (get_vars t)) in
                        fold_union acc delta mem to_type (norm_basic to_type)
                          dnf
                    | Times m
                    | Xml m ->
                        let module K = (val m) in
                        let to_type = cons_to_type (module K) in
                        let dnf = K.(Dnf.get_full (get_vars t)) in
                        fold_union acc delta mem to_type norm_prod dnf
                    | Function m ->
                        let module K = (val m) in
                        let to_type = cons_to_type (module K) in
                        let dnf = K.(Dnf.get_full (get_vars t)) in
                        fold_union acc delta mem to_type norm_arrow dnf
                    | Record m ->
                        let module K = (val m) in
                        let to_type = cons_to_type (module K) in
                        let dnf = K.(Dnf.get_full (get_vars t)) in
                        fold_union acc delta mem to_type (norm_record to_type)
                          dnf
                    | Absent -> acc)
                ConstrSet.sat t
            in
            LocalMemo.replace mem t (true, res);
            res
          end

  and norm_prod delta mem (lpos, lneg) =
    let rec neg_part t1 t2 = function
      | [] -> ConstrSet.unsat
      | (s1, s2) :: rest ->
          let z1 = diff t1 (descr s1) in
          let z2 = diff t2 (descr s2) in
          let con1 = norm delta mem z1 in
          let con10 = neg_part z1 t2 rest in
          let con11 = ConstrSet.union con1 con10 in
          if ConstrSet.is_unsat con11 then ConstrSet.unsat
          else
            let con2 = norm delta mem z2 in
            let con20 = neg_part t1 z2 rest in
            let con22 = ConstrSet.union con2 con20 in
            ConstrSet.inter con11 con22
    in
    (* cap_product return the intersection of all (fst pos,snd pos) *)
    let t1, t2 = cap_product any any lpos in
    let con1 = norm delta mem t1 in
    let con2 = norm delta mem t2 in
    let con0 = neg_part t1 t2 lneg in
    ConstrSet.(union (union con1 con2) con0)

  and norm_arrow delta mem (lpos, lneg) =
    match lneg with
    | [] -> ConstrSet.unsat
    | (t1, t2) :: n ->
        if is_empty (descr t1) then ConstrSet.sat
        else
          let t1 = descr t1
          and t2 = descr t2 in
          let con1 = norm delta mem t1 in
          (* [t1] *)
          let con2 = aux_arrow delta mem t1 (diff any t2) lpos in
          let con0 = norm_arrow delta mem (lpos, n) in
          ConstrSet.union (ConstrSet.union con1 con2) con0

  and aux_arrow delta mem t1 acc l =
    match l with
    | [] -> ConstrSet.unsat
    | (s1, s2) :: p ->
        let t1s1 = diff t1 (descr s1) in
        let acc1 = cap acc (descr s2) in
        let con1 = norm delta mem t1s1 in
        (* [t1 \ s1] *)
        let con10 = aux_arrow delta mem t1s1 acc p in
        let con11 = ConstrSet.union con1 con10 in
        if ConstrSet.is_unsat con11 then ConstrSet.unsat
        else
          let con2 = norm delta mem acc1 in
          (* [(Any \ t2) ^ s2] *)
          let con20 = aux_arrow delta mem t1 acc1 p in
          let con22 = ConstrSet.union con2 con20 in
          ConstrSet.inter con11 con22

  and norm_record to_type delta mem line =
    (* We normalize the record *)
    let tline = to_type line in
    if is_empty tline then ConstrSet.sat
    else
      List.fold_left
        (fun acc (fields, _, _) ->
          ConstrSet.inter acc
            (Ident.LabelMap.fold
               (fun _ (_, t) acc -> ConstrSet.union acc (norm delta mem t))
               fields ConstrSet.unsat))
        ConstrSet.sat (Record.get tline)

  let get_local delta =
    try GlobalMemo.find global_memo delta with
    | Not_found ->
        let mem = LocalMemo.create 17 in
        GlobalMemo.add global_memo delta mem;
        mem

  let norm delta t =
    let mem = get_local delta in
    norm delta mem t

  (* Merging of constraints. *)
  module TypeCache = struct
    let empty = []
    let mem e l = List.exists (Types.equiv e) l
    let add e l = if mem e l then l else e :: l
  end

  exception Found of descr

  let rec merge delta cache m =
    let saturate x =
      let cache = TypeCache.add x cache in
      let n = norm delta x in
      let c1 = ConstrSet.inter n (ConstrSet.singleton m) in
      let c2 =
        ConstrSet.fold
          (fun m1 acc -> ConstrSet.union acc (merge delta cache m1))
          c1 ConstrSet.unsat
      in
      c2
    in
    try
      Line.fold
        (fun _v (inf, sup) () ->
          if not (subtype inf sup) then
            let x = diff inf sup in
            if not (TypeCache.mem x cache) then raise_notrace (Found x))
        m ();
      ConstrSet.singleton m
    with
    | Found x -> saturate x

  let merge delta m =
    let res = merge delta TypeCache.empty m in
    res

  (** Constraint solving *)

  let solve delta s =
    let add_eq alpha s t acc =
      let beta =
        let a = Var.name alpha in
        var Var.(mk ~kind:`generated (a ^ a))
      in
      Var.Map.replace alpha (cap (cup s beta) t) acc
    in
    let extra_var t acc =
      if Subst.is_var t then
        let v, _ = Subst.extract t in
        if Var.Set.mem delta v then acc else add_eq v empty any acc
      else acc
    in
    let to_eq_set m =
      Line.fold
        (fun alpha (s, t) acc ->
          let acc = extra_var t acc in
          let acc = extra_var s acc in
          add_eq alpha s t acc)
        m Var.Map.empty
    in
    ConstrSet.fold (fun m acc -> to_eq_set m :: acc) s []

  let unify (eq_set : t Var.Map.map) =
    let rec loop eq_set accu =
      if Var.Map.is_empty eq_set then accu
      else
        let (alpha, t), eq_set' = Var.Map.remove_min eq_set in
        let x = Subst.solve_rectype t alpha in
        let subst_x = Var.Map.singleton alpha x in
        let eq_set' = Var.Map.map (Subst.apply_full subst_x) eq_set' in
        let sigma = loop eq_set' (Var.Map.replace alpha x accu) in
        let t_alpha = Subst.apply_full sigma x in
        Var.Map.replace alpha t_alpha sigma
    in
    loop eq_set Var.Map.empty

  let no_var d = Var.Set.is_empty (Subst.vars d)

  let tallying_gen gen_all delta l =
    let n =
      try
        List.fold_left
          (fun acc (s, t) ->
            if ConstrSet.is_unsat acc then raise Exit
            else
              let d = diff s t in
              if is_empty d then acc
              else if no_var d then raise Exit
              else ConstrSet.inter acc (norm delta d))
          ConstrSet.sat l
      with
      | Exit -> ConstrSet.unsat
    in
    if ConstrSet.is_unsat n then raise Step1Fail
    else
      let m =
        ConstrSet.fold
          (fun c acc ->
            let mc = merge delta c in
            match solve delta mc with
            _ ::_ when not gen_all -> raise Exit
            | l -> List.rev_append l acc)
          n []
      in
      if m == [] then raise Step2Fail;
      let res = List.map unify m in
      res
end

module type S = sig
  val tallying_gen : bool -> Var.Set.t -> (t * t) list -> t Var.Map.map list
end

let tallying gen_all var_order delta l =
  let amod =
    match var_order with
    | [] ->
        (module Make (struct
          include Var

          let delta = delta
        end) : S)
    | _ ->
        let module VHash = Hashtbl.Make (Var) in
        let hash = VHash.create 16 in
        let () = List.iteri (fun i v -> VHash.add hash v i) var_order in
        let vmax = VHash.length hash in
        let comp v1 v2 =
          let n1 =
            try VHash.find hash v1 with
            | Not_found -> vmax
          in
          let n2 =
            try VHash.find hash v2 with
            | Not_found -> vmax
          in
          let c = Stdlib.compare n1 n2 in
          if c = 0 then Var.compare v1 v2 else c
        in
        (module Make (struct
          include Var

          let delta = delta
          let compare = comp
        end))
  in
  let module A = (val amod) in
  A.tallying_gen gen_all delta l

let set a i v =
  let len = Array.length !a in
  if i < len then !a.(i) <- v
  else
    let b = Array.make ((2 * len) + 1) empty in
    Array.blit !a 0 b 0 len;
    b.(i) <- v;
    a := b

let get a i = if i < 0 then any else !a.(i)

exception UnsatConstr of string

exception FoundApply of t * int * int * Types.t Var.Map.map list

let apply_raw delta s t =
  (*GlobalMemo.clear global_memo;*)
  (* cell i of ai contains /\k<=i s_k, cell j of aj contains /\k<=j t_k *)
  let ai = ref [||]
  and aj = ref [||] in
  let tallying i j =
    try
      let s = get ai i in
      let targ = get aj j in
      let s = Subst.refresh delta s in
      let targ = Subst.refresh delta targ in
      let vgamma = Var.mk "Gamma" in
      let gamma = var vgamma in
      let cgamma = cons gamma in
      let t = arrow (cons targ) cgamma in
      let sl = tallying true [] delta [ (s, t) ] in
      let new_res =
        List.fold_left
          (fun tacc si ->
            let tres = Subst.apply_full si gamma in
            let tres = Subst.refresh delta tres in
            let tres = Subst.clean_type delta tres in
            cap tacc tres)
          any sl
      in

      (*      let new_res = Subst.clean_type delta new_res in*)
      raise (FoundApply (new_res, i, j, sl))
    with
    | Step1Fail ->
        assert (i == 0 && j == 0);
        raise (UnsatConstr "apply_raw step1")
    | Step2Fail -> ()
    (* continue *)
  in
  let rec loop i =
    try
      (* Format.eprintf "Starting expansion %i @\n@." i; *)
      let ss, tt =
        if i = 0 then (s, t)
        else
          ( cap (Subst.refresh delta s) (get ai (i - 1)),
            cap (Subst.refresh delta t) (get aj (i - 1)) )
      in
      set ai i ss;
      set aj i tt;
      for j = 0 to i - 1 do
        tallying j i;
        tallying i j
      done;
      tallying i i;
      loop (i + 1)
    with
    | FoundApply (res, i, j, sl) ->
        ( sl,
          get ai i,
          get aj j,
          let vars = Subst.vars res in
          let vars = Var.Set.diff vars delta in
          let mapping = Var.full_renaming vars in
          Subst.apply_full (Var.Map.map Types.var mapping) res )
  in
  loop 0

let apply_full delta s t =
  try
    let _, _, _, res = apply_raw delta s t in
    Some res
  with
  | UnsatConstr _ -> None

let squareapply delta s t =
  try
    let s, _, _, res = apply_raw delta s t in
    Some (s, res)
  with
  | UnsatConstr _ -> None

let apply_raw delta s t =
  try Some (apply_raw delta s t) with
  | UnsatConstr _ -> None

  let test_tallying ?(var_order = []) delta types =
    match tallying  false var_order delta types with
    [] -> false
    | _ :: _ -> true
    | exception Step1Fail -> false
    | exception Step2Fail -> false
    | exception Exit -> true


  let tallying ?(var_order = []) delta types =
  try tallying true var_order delta types with
  | Step1Fail
  | Step2Fail ->
      []

