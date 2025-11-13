type 'a line = 'a list * 'a list
type 'a dnf = 'a line list

type ('elem, 'leaf) bdd =
  | False
  | True
  | Leaf of 'leaf
  | Split of
      int * 'elem * ('elem, 'leaf) bdd * ('elem, 'leaf) bdd * ('elem, 'leaf) bdd

let empty = False

type 'atom var_bdd = (Var.t, ('atom, bool) bdd) bdd

(* Ternary decision diagrams:
   False → 𝟘
   True → 𝟙
   Leaf (v) → ⟦ v ⟧
   Split (h, x, p, i, n) →
        (⟦ x ⟧ ∩ ⟦ p ⟧) ∪ ⟦ i ⟧ ∪ (⟦ n ⟧ \ ⟦ x ⟧)

   Invariants maintained (see simplify split and leaf smart constructors and simplify functions:)
   - p ≠ n
   - ∀ i st. t ≡ Split (h, x, p, Split (…, Split (_, _, i, _) …), n), p ≠ i
     Indeed, given the equation above, if some bdd is in ⟦ i ⟧, it can be removed,
     from ⟦ p ⟧ and ⟦ n ⟧ safely. A corollary is that if ⟦ i ⟧ contains True, both p and
      n are useless, and therefore the whole bdd is replaced by true.

   - if t ≡ Leaf (v), ⟦ v ⟧ ≠ 𝟘 and ⟦ v ⟧ ≠ 𝟙
    (if v is recursively a bdd of atoms (while t is a bdd of variables) then
     it means that v is not trivially False or True, but it could be a complex
     bdd that is in fine equal to True, but we would need subtyping to decide this).
*)

module MK
    (E : Custom.T) (Leaf : sig
      include Tset.S

      val iter : (elem -> unit) -> t -> unit
    end) =
struct
  type elem = E.t
  type t = (E.t, Leaf.t) bdd

  let test x =
    match x with
    | Leaf l -> Leaf.test l
    | _ -> Tset.Unknown

  let rec equal a b =
    a == b
    ||
    match (a, b) with
    | Split (h1, x1, p1, i1, n1), Split (h2, x2, p2, i2, n2) ->
        h1 == h2 && E.equal x1 x2 && equal p1 p2 && equal i1 i2 && equal n1 n2
    | Leaf l1, Leaf l2 -> Leaf.equal l1 l2
    | _ -> false

  let rec compare a b =
    if a == b then 0
    else
      match (a, b) with
      | Split (h1, x1, p1, i1, n1), Split (h2, x2, p2, i2, n2) ->
          if h1 < h2 then -1
          else if h1 > h2 then 1
          else
            let c = E.compare x1 x2 in
            if c <> 0 then c
            else
              let c = compare p1 p2 in
              if c <> 0 then c
              else
                let c = compare i1 i2 in
                if c <> 0 then c else compare n1 n2
      | Leaf l1, Leaf l2 -> Leaf.compare l1 l2
      | True, _ -> -1
      | _, True -> 1
      | False, _ -> -1
      | _, False -> 1
      | Leaf _, Split _ -> 1
      | Split _, Leaf _ -> -1

  let hash = function
    | False -> 0
    | True -> 1
    | Leaf l -> 17 * Leaf.hash l
    | Split (h, _, _, _, _) -> h

  let compute_hash x p i n =
    let h = E.hash x in
    let h = h lxor (257 * hash p) in
    let h = h lxor (8191 * hash i) in
    let h = h lxor (16637 * hash n) in
    h

  let rec check = function
    | True
    | False ->
        ()
    | Leaf l -> Leaf.check l
    | Split (h, x, p, i, n) ->
        assert (h = compute_hash x p i n);
        (match p with
        | Split (_, y, _, _, _) -> assert (E.compare x y < 0)
        | _ -> ());
        (match i with
        | Split (_, y, _, _, _) -> assert (E.compare x y < 0)
        | _ -> ());
        (match n with
        | Split (_, y, _, _, _) -> assert (E.compare x y < 0)
        | _ -> ());
        E.check x;
        check p;
        check i;
        check n

  let any = True
  let empty = empty

  let leaf l =
    match Leaf.test l with
    | Tset.Empty -> empty
    | Tset.Full -> any
    | _ -> Leaf l

  let rec iter_partial fe fleaf = function
    | Split (_, x, p, i, n) ->
        fe x;
        iter_partial fe fleaf p;
        iter_partial fe fleaf i;
        iter_partial fe fleaf n
    | Leaf leaf -> fleaf leaf
    | _ -> ()

  let iter_full fe fl = iter_partial fe (Leaf.iter fl)

  let rec dump s ppf = function
    | False -> Format.fprintf ppf "F"
    | True -> Format.fprintf ppf "T"
    | Leaf l -> Format.fprintf ppf "%s%a" s Leaf.dump l
    | Split (_, x, p, i, n) ->
        Format.fprintf ppf "@[%s%a@[(@[%a@],@,@[%a@],@,@[%a@])@]@]" s E.dump x
          (dump "+") p (dump "=") i (dump "-") n

  let dump ppf a = (dump "") ppf a

  let rec get accu pos neg = function
    | False -> accu
    | True -> ((pos, neg), Leaf.any) :: accu
    | Leaf leaf -> ((pos, neg), leaf) :: accu
    | Split (_, x, p, i, n) ->
        (*OPT: can avoid creating this list cell when pos or neg =False *)
        let accu = get accu (x :: pos) neg p in
        let accu = get accu pos (x :: neg) n in
        let accu = get accu pos neg i in
        accu

  let get (l : t) = get [] [] [] l

  let compute_full ~empty ~any ~cup ~cap ~diff ~atom ~leaf b =
    let rec aux t =
      match t with
      | False -> empty
      | True -> any
      | Leaf l -> leaf l
      | Split (_, x, p, i, n) ->
          let atx = atom x in
          let p = cap atx (aux p)
          and i = aux i
          and n = diff (aux n) atx in
          cup (cup p i) n
    in
    aux b

  let split0 x pos ign neg = Split (compute_hash x pos ign neg, x, pos, ign, neg)
  let atom x = split0 x any empty empty

  (* Smart constructor for split, ensures that the envariants are preserved
     with the use of simplify *)
  let rec split x p i n =
    if i == True then any
    else if equal p n then if equal p i then p else p ++ i
    else
      let p = simplify p [ i ]
      and n = simplify n [ i ] in
      if equal p n then if equal p i then p else p ++ i else split0 x p i n

  (* ensures that a does not contain any of the bdds in l,
     if it does, replace it does, remove it. *)
  and simplify a l =
    match a with
    | False -> empty
    | Split (_, x, p, i, n) -> restrict_from_list a x p i n [] [] [] False l
    | _ -> restrict_true_or_same a l

  and restrict_true_or_same a = function
    | [] -> a
    | True :: _ -> empty
    | b :: l -> if equal a b then empty else restrict_true_or_same a l

  (* traverses a list of bdds b and
     accumulate in ap ai an those that might be present in each
     component p, i, n of a (a   ≡ Split (h, x, p, i, n)) the arguments
     are passed to avoid reconstructing it.
     The _dummy argument keeps the arguments of both functions
     in the same order which prevent ocaml from spilling too much on the stack.
  *)
  and restrict_from_list a x p i n ap ai an _dummy = function
    | [] ->
        let p = simplify p ap
        and n = simplify n an
        and i = simplify i ai in
        if equal p n then p ++ i else split0 x p i n
    | b :: l -> restrict_elem a x p i n ap ai an b l

  and restrict_elem a x p i n ap ai an b l =
    match b with
    | False -> restrict_from_list a x p i n ap ai an b l
    | True -> empty
    | Leaf _ as b ->
        (* inline then next case, knowing that a = Split ...
           and b is Leaf b is greater*)
        restrict_from_list a x p i n (b :: ap) (b :: ai) (b :: an) b l
    | Split (_, x2, p2, i2, n2) as b ->
        let c = E.compare x2 x in
        if c < 0 then restrict_elem a x p i n ap ai an i2 l
        else if c > 0 then
          restrict_from_list a x p i n (b :: ap) (b :: ai) (b :: an) b l
        else if equal a b then empty
        else
          restrict_from_list a x p i n (p2 :: i2 :: ap) (i2 :: ai)
            (n2 :: i2 :: an) b l

  and ( ++ ) a b =
    if a == b then a
    else
      match (a, b) with
      | True, _
      | _, True ->
          any
      | False, _ -> b
      | _, False -> a
      | Leaf l1, Leaf l2 -> leaf (Leaf.cup l1 l2)
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then split x1 (p1 ++ p2) (i1 ++ i2) (n1 ++ n2)
          else if c < 0 then split x1 p1 (i1 ++ b) n1
          else split x2 p2 (i2 ++ a) n2
      | Split (_, x1, p1, i1, n1), Leaf _ -> split x1 p1 (i1 ++ b) n1
      | Leaf _, Split (_, x2, p2, i2, n2) -> split x2 p2 (i2 ++ a) n2

  let rec ( ** ) a b =
    if a == b then a
    else
      match (a, b) with
      | False, _
      | _, False ->
          empty
      | True, _ -> b
      | _, True -> a
      | Leaf l1, Leaf l2 -> leaf (Leaf.cap l1 l2)
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then
            split x1
              ((p1 ** (p2 ++ i2)) ++ (p2 ** i1))
              (i1 ** i2)
              ((n1 ** (n2 ++ i2)) ++ (n2 ** i1))
          else if c < 0 then split x1 (p1 ** b) (i1 ** b) (n1 ** b)
          else split x2 (p2 ** a) (i2 ** a) (n2 ** a)
      | Split (_, x1, p1, i1, n1), Leaf _ ->
          split x1 (p1 ** b) (i1 ** b) (n1 ** b)
      | Leaf _, Split (_, x2, p2, i2, n2) ->
          split x2 (p2 ** a) (i2 ** a) (n2 ** a)

  let rec neg = function
    | False -> any
    | True -> empty
    | Leaf l -> leaf (Leaf.neg l)
    | Split (_, x, p, i, False) -> split x empty (neg (i ++ p)) (neg i)
    | Split (_, x, False, i, n) -> split x (neg i) (neg (i ++ n)) empty
    | Split (_, x, p, False, n) -> split x (neg p) (neg (p ++ n)) (neg n)
    | Split (_, x, p, i, n) -> neg i ** split x (neg p) (neg (p ++ n)) (neg n)

  let rec ( // ) a b =
    if a == b then empty
    else
      match (a, b) with
      | False, _
      | _, True ->
          empty
      | _, False -> a
      | True, _ -> neg b
      | Leaf l1, Leaf l2 -> leaf (Leaf.diff l1 l2)
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then
            if i2 == empty && n2 == empty then
              split x1 (p1 // p2) (i1 // p2) (n1 ++ i1)
            else
              split x1
                ((p1 ++ i1) // (p2 ++ i2))
                empty
                ((n1 ++ i1) // (n2 ++ i2))
          else if c < 0 then split x1 (p1 // b) (i1 // b) (n1 // b)
          else split x2 (a // (i2 ++ p2)) empty (a // (i2 ++ n2))
      | _ -> a ** neg b

  let ( ~~ ) = neg
  let cup = ( ++ )
  let cap = ( ** )
  let diff = ( // )

  let extract_var = function
    | Split
        ( _,
          v,
          ((False | True | Leaf _) as p),
          ((False | True | Leaf _) as i),
          ((False | True | Leaf _) as n) ) ->
        Some (v, p, i, n)
    | _ -> None
end

module Bool : sig
  include Tset.S with type t = bool and type elem = bool

  val iter : (elem -> unit) -> t -> unit
end = struct
  include Custom.Bool

  type elem = bool

  let empty = false
  let any = true
  let test (x : bool) : Tset.cardinal = Obj.magic x
  let atom b = b
  let cup a b = a || b
  let cap a b = a && b
  let neg a = not a
  let diff a b = a && not b
  let iter _ _ = ()
end

module type S = sig
  type atom
  (** The type of atoms in the Boolean combinations *)

  type mono
  (** The type of Boolean combinations of atoms. *)

  include Custom.T with type t = (Var.t, mono) bdd

  type line
  (** An explicit representation of conjunctions of atoms. *)

  type dnf
  (** An explicit representation fo the DNF. *)

  val atom : atom -> t
  val mono : mono -> t
  val mono_dnf : mono -> dnf
  val any : t
  val empty : t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val get : t -> dnf
  val get_mono : t -> mono
  val iter : (atom -> unit) -> t -> unit

  val compute :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(atom -> 'b) ->
    t ->
    'b

  val var : Var.t -> t
  val extract_var : t -> (Var.t * t * t * t) option

  (** {2 Polymorphic interface. }*)

  val get_partial : t -> ((Var.t list * Var.t list) * mono) list
  val get_full : t -> ((Var.t list * Var.t list) * line) list
  val iter_partial : (Var.t -> unit) -> (mono -> unit) -> t -> unit
  val iter_full : (Var.t -> unit) -> (atom -> unit) -> t -> unit

  val compute_partial :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    mono:(mono -> 'b) ->
    var:(Var.t -> 'b) ->
    t ->
    'b

  val compute_full :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(atom -> 'b) ->
    var:(Var.t -> 'b) ->
    t ->
    'b

  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( // ) : t -> t -> t
  val ( ~~ ) : t -> t
end

module Make (E : Custom.T) :
  S
    with type atom = E.t
     and type line = E.t list * E.t list
     and type dnf = (E.t list * E.t list) list
     and type mono = (E.t, Bool.t) bdd = struct
  module Atom = MK (E) (Bool)

  type atom = E.t
  type mono = (E.t, Bool.t) bdd

  include
    MK
      (Var)
      (struct
        include Atom

        let iter f b = iter_full f ignore b
      end)

  type line = E.t list * E.t list
  type dnf = (E.t list * E.t list) list

  let var x = atom x
  let atom x = leaf (Atom.atom x)
  let mono x = leaf x

  let get_aux combine acc b =
    List.fold_left
      (fun acc ((p, n), dnf) ->
        let p = List.rev p in
        let n = List.rev n in
        let dnf = Atom.get dnf in
        List.fold_left
          (fun acc ((p2, n2), b) -> if b then combine p n p2 n2 acc else acc)
          acc dnf)
      acc (get b)

  let get_full = get_aux (fun p n p2 n2 acc -> ((p, n), (p2, n2)) :: acc) []

  let mono_dnf mono =
    let l = Atom.get mono in
    List.map fst l

  let get_partial b : ((Var.t list * Var.t list) * mono) list =
    List.rev_map (fun ((p, n), m) -> ((List.rev p, List.rev n), m)) (get b)

  let get b = get_aux (fun _ _ p2 n2 acc -> (p2, n2) :: acc) [] b

  let get_mono b : mono =
    List.fold_left
      (fun acc (_, dnf) -> Atom.cup acc dnf)
      Atom.empty (get_partial b)

  (* Temporary List.rev to order elements as before introduction of
     polymorphic variables.
  *)
  let iter = iter_full (fun _ -> ())

  let compute_partial ~empty ~any ~cup ~cap ~diff ~mono ~(var : Var.t -> 'a) b =
    compute_full ~empty ~any ~cup ~cap ~diff ~atom:var ~leaf:mono b

  let compute_full ~empty ~any ~cup ~cap ~diff ~atom ~(var : Var.t -> 'a) b =
    compute_full ~empty ~any ~cup ~cap ~diff ~atom:var
      ~leaf:
        (Atom.compute_full ~empty ~any ~cup ~cap ~diff ~atom ~leaf:(function
          | false -> empty
          | true -> any))
      b

  let compute ~empty ~any ~cup ~cap ~diff ~atom b =
    compute_full ~empty ~any ~cup ~cap ~diff ~atom ~var:(fun _ -> any) b
end

module MKBasic (T : Tset.S) :
  S with type atom = T.elem and type mono = T.t and type dnf = T.t = struct
  type atom = T.elem
  type mono = T.t

  include
    MK
      (Var)
      (struct
        include T

        let iter _ _ = assert false
      end)

  type line
  type dnf = T.t

  let var v = atom v
  let mono x = leaf x
  let atom x = leaf (T.atom x)

  let get_partial t =
    List.map (fun ((p, n), m) -> ((List.rev p, List.rev n), m)) (get t)

  let mono_dnf x = x

  let get_mono t =
    List.fold_left (fun acc ((_, _), dnf) -> T.cup acc dnf) T.empty (get t)

  let get _ = assert false
  let get_full _ = assert false

  let compute_partial ~empty ~any ~cup ~cap ~diff ~mono ~var b =
    compute_full ~empty ~any ~cup ~cap ~diff ~atom:var ~leaf:mono b

  let[@ocaml.warning "-27"] compute_full
      ~empty
      ~any
      ~cup
      ~cap
      ~diff
      ~atom
      ~var
      b =
    assert false

  let[@ocaml.warning "-27"] compute ~empty ~any ~cup ~cap ~diff ~atom b =
    assert false

  let iter _ _ = assert false
  let iter_full _ _ _ = assert false
end

module VarIntervals = MKBasic (Intervals)
module VarCharSet = MKBasic (CharSet)
module VarAtomSet = MKBasic (AtomSet)
module VarAbstractSet = MKBasic (AbstractSet)
