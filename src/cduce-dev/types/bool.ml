let ( < ) : int -> int -> bool = ( < )
let ( > ) : int -> int -> bool = ( > )
let ( = ) : int -> int -> bool = ( = )

module type S = sig
  include Tset.S

  val get : t -> (elem list * elem list) list

  (*
    val get' : t -> (elem list * elem list list) list
  *)
  val iter : (elem -> unit) -> t -> unit

  val compute :
    empty:'b ->
    any:'b ->
    cup:('b -> 'b -> 'b) ->
    cap:('b -> 'b -> 'b) ->
    diff:('b -> 'b -> 'b) ->
    atom:(elem -> 'b) ->
    t ->
    'b

  (*  val trivially_disjoint : t -> t -> bool *)
end

module Make (E : Custom.T) = struct
  type elem = E.t

  type t =
    | True
    | False
    | Split of int * elem * t * t * t

  let rec equal a b =
    a == b
    ||
    match (a, b) with
    | Split (h1, x1, p1, i1, n1), Split (h2, x2, p2, i2, n2) ->
        h1 == h2 && equal p1 p2 && equal i1 i2 && equal n1 n2 && E.equal x1 x2
    | _ -> false

  (* Idea: add a mutable "unique" identifier and set it to
     the minimum of the two when egality ... *)

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
      | True, _ -> -1
      | _, True -> 1
      | False, _ -> -1
      | _, False -> 1

  let hash = function
    | True -> 1
    | False -> 0
    | Split (h, _, _, _, _) -> h

  let compute_hash x p i n =
    E.hash x + (17 * hash p) + (257 * hash i) + (16637 * hash n)

  let rec check = function
    | True
    | False ->
        ()
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

  let atom x =
    let h = E.hash x + 17 in
    (* partial evaluation of compute_hash... *)
    Split (h, x, True, False, False)

  let rec iter f = function
    | Split (_, x, p, i, n) ->
        f x;
        iter f p;
        iter f i;
        iter f n
    | _ -> ()

  let rec dump ppf = function
    | True -> Format.fprintf ppf "+"
    | False -> Format.fprintf ppf "-"
    | Split (_, x, p, i, n) ->
        Format.fprintf ppf "%i(@[%a,%a,%a@])" (* E.dump x *) (E.hash x) dump p
          dump i dump n

  let rec print f ppf = function
    | True -> Format.fprintf ppf "Any"
    | False -> Format.fprintf ppf "Empty"
    | Split (_, x, p, i, n) -> (
        let flag = ref false in
        let b () = if !flag then Format.fprintf ppf " | " else flag := true in
        (match p with
        | True ->
            b ();
            Format.fprintf ppf "%a" f x
        | False -> ()
        | _ ->
            b ();
            Format.fprintf ppf "%a & @[(%a)@]" f x (print f) p);
        (match i with
        | True -> assert false
        | False -> ()
        | _ ->
            b ();
            print f ppf i);
        match n with
        | True ->
            b ();
            Format.fprintf ppf "@[~%a@]" f x
        | False -> ()
        | _ ->
            b ();
            Format.fprintf ppf "@[~%a@] & @[(%a)@]" f x (print f) n)

  let[@ocaml.warning "-32"] print a f = function
    | True -> [ (fun ppf -> Format.fprintf ppf "%s" a) ]
    | False -> []
    | c -> [ (fun ppf -> print f ppf c) ]

  let rec get accu pos neg = function
    | True -> (pos, neg) :: accu
    | False -> accu
    | Split (_, x, p, i, n) ->
        (*OPT: can avoid creating this list cell when pos or neg =False *)
        let accu = get accu (x :: pos) neg p in
        let accu = get accu pos (x :: neg) n in
        let accu = get accu pos neg i in
        accu

  let get x = get [] [] [] x

  let rec get' accu pos neg = function
    | True -> (pos, neg) :: accu
    | False -> accu
    | Split (_, x, p, i, n) ->
        let accu = get' accu (x :: pos) neg p in
        let rec aux l = function
          | Split (_, x, False, i, n') when equal n n' -> aux (x :: l) i
          | i ->
              (*	      if (List.length l > 1) then (print_int (List.length l); flush stdout); *)
              let accu = get' accu pos (l :: neg) n in
              get' accu pos neg i
        in
        aux [ x ] i

  let _get' x = get' [] [] [] x

  let compute ~empty ~any ~cup ~cap ~diff ~atom b =
    let rec aux = function
      | True -> any
      | False -> empty
      | Split (_, x, p, i, n) ->
          let p = cap (atom x) (aux p)
          and i = aux i
          and n = diff (aux n) (atom x) in
          cup (cup p i) n
    in
    aux b

  (* Invariant: correct hash value *)

  let split0 x pos ign neg = Split (compute_hash x pos ign neg, x, pos, ign, neg)
  let empty = False
  let any = True

  let test = function
    | False -> Tset.Empty
    | True -> Tset.Full
    | _ -> Tset.Unknown
  (* Invariants:
     Split (x, pos,ign,neg) ==>  (ign <> True), (pos <> neg)
  *)

  let rec has_true = function
    | [] -> false
    | True :: _ -> true
    | _ :: l -> has_true l

  let rec has_same a = function
    | [] -> false
    | b :: l -> equal a b || has_same a l

  let rec split x p i n =
    if i == True then True
    else if equal p n then p ++ i
    else
      let p = simplify p [ i ]
      and n = simplify n [ i ] in
      if equal p n then p ++ i else split0 x p i n

  and simplify a l =
    match a with
    | False -> False
    | True -> if has_true l then False else True
    | Split (_, x, p, i, n) ->
        if has_true l || has_same a l then False
        else s_aux2 a x p i n [] [] [] l

  and s_aux2 a x p i n ap ai an = function
    | [] ->
        let p = simplify p ap
        and n = simplify n an
        and i = simplify i ai in
        if equal p n then p ++ i else split0 x p i n
    | b :: l -> s_aux3 a x p i n ap ai an l b

  and s_aux3 a x p i n ap ai an l = function
    | False -> s_aux2 a x p i n ap ai an l
    | True -> assert false
    | Split (_, x2, p2, i2, n2) as b ->
        if equal a b then False
        else
          let c = E.compare x2 x in
          if c < 0 then s_aux3 a x p i n ap ai an l i2
          else if c > 0 then s_aux2 a x p i n (b :: ap) (b :: ai) (b :: an) l
          else s_aux2 a x p i n (p2 :: i2 :: ap) (i2 :: ai) (n2 :: i2 :: an) l

  and ( ++ ) a b =
    if a == b then a
    else
      match (a, b) with
      | True, _
      | _, True ->
          True
      | False, a
      | a, False ->
          a
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then split x1 (p1 ++ p2) (i1 ++ i2) (n1 ++ n2)
          else if c < 0 then split x1 p1 (i1 ++ b) n1
          else split x2 p2 (i2 ++ a) n2

  (* seems better not to make ++ and this split mutually recursive;
     is the invariant still inforced ? *)

  let rec ( ** ) a b =
    if a == b then a
    else
      match (a, b) with
      | True, a
      | a, True ->
          a
      | False, _
      | _, False ->
          False
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then
            split x1
              ((p1 ** (p2 ++ i2)) ++ (p2 ** i1))
              (i1 ** i2)
              ((n1 ** (n2 ++ i2)) ++ (n2 ** i1))
            (* if (p2 == True) && (n2 == False)
               then split x1 (p1 ++ i1) (i1 ** i2) (n1 ** i2)
               else if (p2 == False) && (n2 == True)
               then split x1 (p1 ** i2) (i1 ** i2) (n1 ++ i1)
               else
                 split x1 ((p1++i1) ** (p2 ++ i2)) False ((n1 ++ i1) ** (n2 ++ i2))
            *)
          else if c < 0 then split x1 (p1 ** b) (i1 ** b) (n1 ** b)
          else split x2 (p2 ** a) (i2 ** a) (n2 ** a)

  (* let rec trivially_disjoint a b =
     if a == b then a == False
     else
       match (a, b) with
       | True, a | a, True -> a == False
       | False, _ | _, False -> true
       | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
           let c = E.compare x1 x2 in
           if c = 0 then
             (* try expanding -> p1 p2; p1 i2; i1 p2; i1 i2 ... *)
             trivially_disjoint (p1 ++ i1) (p2 ++ i2)
             && trivially_disjoint (n1 ++ i1) (n2 ++ i2)
           else if c < 0 then
             trivially_disjoint p1 b && trivially_disjoint i1 b
             && trivially_disjoint n1 b
           else
             trivially_disjoint p2 a && trivially_disjoint i2 a
             && trivially_disjoint n2 a
  *)
  let rec neg = function
    | True -> False
    | False -> True
    | Split (_, x, p, i, False) -> split x False (neg (i ++ p)) (neg i)
    | Split (_, x, False, i, n) -> split x (neg i) (neg (i ++ n)) False
    | Split (_, x, p, False, n) -> split x (neg p) (neg (p ++ n)) (neg n)
    (* | Split (_,x, p, False, False) ->
       split x False (neg p) True
          | Split (_,x, False, False, n) -> split x True (neg n) False *)
    | Split (_, x, p, i, n) -> split x (neg (i ++ p)) False (neg (i ++ n))

  let rec ( // ) a b =
    (*    if equal a b then False  *)
    if a == b then False
    else
      match (a, b) with
      | False, _
      | _, True ->
          False
      | a, False -> a
      | True, b -> neg b
      | Split (_, x1, p1, i1, n1), Split (_, x2, p2, i2, n2) ->
          let c = E.compare x1 x2 in
          if c = 0 then
            if i2 == False && n2 == False then
              split x1 (p1 // p2) (i1 // p2) (n1 ++ i1)
              (* else if (i2 == False) && (p2 == False)
                 then split x1 (p1 ++ i1) (i1 // n2) (n1 // n2) *)
            else
              split x1
                ((p1 ++ i1) // (p2 ++ i2))
                False
                ((n1 ++ i1) // (n2 ++ i2))
          else if c < 0 then split x1 (p1 // b) (i1 // b) (n1 // b)
            (*	    split x1 ((p1 ++ i1)// b) False ((n1 ++i1) // b)  *)
          else split x2 (a // (i2 ++ p2)) False (a // (i2 ++ n2))

  let cup = ( ++ )
  let cap = ( ** )
  let diff = ( // )
end
