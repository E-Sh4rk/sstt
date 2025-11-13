module type S = sig
  module Elem : Custom.T
  include Custom.T with type t = private Elem.t list

  external get : t -> Elem.t list = "%identity"
  external unsafe_cast : Elem.t list -> t = "%identity"
  val singleton : Elem.t -> t
  val iter : (Elem.t -> unit) -> t -> unit
  val filter : (Elem.t -> bool) -> t -> t
  val exists : (Elem.t -> bool) -> t -> bool
  val fold : ('a -> Elem.t -> 'a) -> 'a -> t -> 'a
  val pick : t -> Elem.t option
  val choose : t -> Elem.t
  val length : t -> int
  val empty : t
  val is_empty : t -> bool
  val from_list : Elem.t list -> t
  val add : Elem.t -> t -> t
  val remove : Elem.t -> t -> t
  val disjoint : t -> t -> bool
  val cup : t -> t -> t
  val split : t -> t -> t * t * t

  (* split l1 l2 = (l1 \ l2, l1 & l2, l2 \ l1) *)
  val cap : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val map : (Elem.t -> Elem.t) -> t -> t
  val mem : t -> Elem.t -> bool

  module Map : sig
    type 'a map

    external get : 'a map -> (Elem.t * 'a) list = "%identity"
    external unsafe_cast : (Elem.t * 'a) list -> 'a map = "%identity"
    val add : Elem.t -> 'a -> 'a map -> 'a map
    val length : 'a map -> int
    val domain : 'a map -> t
    val restrict : 'a map -> t -> 'a map
    val empty : 'a map
    val iter : ('a -> unit) -> 'a map -> unit
    val iteri : (Elem.t -> 'a -> unit) -> 'a map -> unit
    val filter : (Elem.t -> 'a -> bool) -> 'a map -> 'a map
    val split : (Elem.t -> 'a -> bool) -> 'a map -> 'a map * 'a map
    val is_empty : 'a map -> bool
    val singleton : Elem.t -> 'a -> 'a map
    val assoc_remove : Elem.t -> 'a map -> 'a * 'a map
    val remove : Elem.t -> 'a map -> 'a map
    val subset_keys : 'a map -> 'a map -> bool
    val merge : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val merge_set : ('a -> 'a -> 'a) -> 'a map -> t -> 'a -> 'a map

    val combine :
      ('a -> 'c) -> ('b -> 'c) -> ('a -> 'b -> 'c) -> 'a map -> 'b map -> 'c map

    val cap : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val sub : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val merge_elem : 'a -> 'a map -> 'a map -> 'a map
    val union_disj : 'a map -> 'a map -> 'a map
    val diff : 'a map -> t -> 'a map
    val from_list : ('a -> 'a -> 'a) -> (Elem.t * 'a) list -> 'a map
    val from_list_disj : (Elem.t * 'a) list -> 'a map
    val map_from_slist : (Elem.t -> 'a) -> t -> 'a map
    val collide : ('a -> 'b -> unit) -> 'a map -> 'b map -> unit
    val may_collide : ('a -> 'b -> unit) -> exn -> 'a map -> 'b map -> unit
    val map : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (Elem.t -> 'a -> 'b) -> 'a map -> 'b map
    val fold : (Elem.t -> 'a -> 'b -> 'b) -> 'a map -> 'b -> 'b
    val constant : 'a -> t -> 'a map
    val num : int -> t -> int map
    val map_to_list : ('a -> 'b) -> 'a map -> 'b list
    val mapi_to_list : (Elem.t -> 'a -> 'b) -> 'a map -> 'b list
    val assoc : Elem.t -> 'a map -> 'a
    val assoc_present : Elem.t -> 'a map -> 'a
    val replace : Elem.t -> 'a -> 'a map -> 'a map
    val update : ('a -> 'a -> 'a) -> Elem.t -> 'a -> 'a map -> 'a map
    val remove_min : 'a map -> (Elem.t * 'a) * 'a map
    val compare : ('a -> 'a -> int) -> 'a map -> 'a map -> int
    val hash : ('a -> int) -> 'a map -> int
    val equal : ('a -> 'a -> bool) -> 'a map -> 'a map -> bool
  end

  module MakeMap (Y : Custom.T) : sig
    include Custom.T with type t = Y.t Map.map
  end
end

module Make (X : Custom.T) = struct
  include Custom.List (X)

  let rec check = function
    | x :: (y :: _ as tl) ->
        X.check x;
        assert (X.compare x y < 0);
        check tl
    | [ x ] -> X.check x
    | _ -> ()

  type elem = X.t

  let rec equal l1 l2 =
    l1 == l2
    ||
    match (l1, l2) with
    | x1 :: l1, x2 :: l2 -> X.equal x1 x2 && equal l1 l2
    | _ -> false

  let rec hash accu = function
    | [] -> 1 + accu
    | x :: l -> hash ((17 * accu) + X.hash x) l

  let hash l = hash 1 l

  let rec compare l1 l2 =
    if l1 == l2 then 0
    else
      match (l1, l2) with
      | x1 :: l1, x2 :: l2 ->
          let c = X.compare x1 x2 in
          if c <> 0 then c else compare l1 l2
      | [], _ -> -1
      | _ -> 1

  let iter = List.iter
  let filter = List.filter
  let exists = List.exists
  let fold = List.fold_left

  external get : t -> elem list = "%identity"
  external unsafe_cast : elem list -> t = "%identity"

  let singleton x = [ x ]

  let pick = function
    | x :: _ -> Some x
    | _ -> None

  let choose = function
    | x :: _ -> x
    | _ -> raise Not_found

  let length = List.length
  let empty = []
  let is_empty l = l == []

  let rec disjoint l1 l2 =
    if l1 == l2 then l1 == []
    else
      match (l1, l2) with
      | t1 :: q1, t2 :: q2 ->
          let c = X.compare t1 t2 in
          if c < 0 then disjoint q1 l2
          else if c > 0 then disjoint l1 q2
          else false
      | _ -> true

  let rec cup l1 l2 =
    if l1 == l2 then l1
    else
      match (l1, l2) with
      | t1 :: q1, t2 :: q2 ->
          let c = X.compare t1 t2 in
          if c = 0 then t1 :: cup q1 q2
          else if c < 0 then t1 :: cup q1 l2
          else t2 :: cup l1 q2
      | [], l2 -> l2
      | l1, [] -> l1

  let add x l = cup [ x ] l

  let rec split l1 l2 =
    match (l1, l2) with
    | t1 :: q1, t2 :: q2 ->
        let c = X.compare t1 t2 in
        if c = 0 then
          let l1, i, l2 = split q1 q2 in
          (l1, t1 :: i, l2)
        else if c < 0 then
          let l1, i, l2 = split q1 l2 in
          (t1 :: l1, i, l2)
        else
          let l1, i, l2 = split l1 q2 in
          (l1, i, t2 :: l2)
    | _ -> (l1, [], l2)

  let rec diff l1 l2 =
    if l1 == l2 then []
    else
      match (l1, l2) with
      | t1 :: q1, t2 :: q2 ->
          let c = X.compare t1 t2 in
          if c = 0 then diff q1 q2
          else if c < 0 then t1 :: diff q1 l2
          else diff l1 q2
      | _ -> l1

  let remove x l = diff l [ x ]

  let rec cap l1 l2 =
    if l1 == l2 then l1
    else
      match (l1, l2) with
      | t1 :: q1, t2 :: q2 ->
          let c = X.compare t1 t2 in
          if c = 0 then t1 :: cap q1 q2
          else if c < 0 then cap q1 l2
          else cap l1 q2
      | _ -> []

  let rec subset_gen comp l1 l2 =
    l1 == l2
    ||
    match (l1, l2) with
    | t1 :: q1, t2 :: q2 ->
        let c = comp t1 t2 in
        if c = 0 then
          (* inlined: subset q1 q2 *)
          q1 == q2
          ||
          match (q1, q2) with
          | t1 :: qq1, t2 :: qq2 ->
              let c = comp t1 t2 in
              if c = 0 then subset_gen comp qq1 qq2
              else if c < 0 then false
              else subset_gen comp q1 qq2
          | [], _ -> true
          | _ -> false
        else if c < 0 then false
        else subset_gen comp l1 q2
    | [], _ -> true
    | _ -> false

  let subset = subset_gen X.compare

  let from_list l =
    let rec initlist = function
      | [] -> []
      | e :: rest -> [ e ] :: initlist rest
    in
    let rec merge2 = function
      | l1 :: l2 :: rest -> cup l1 l2 :: merge2 rest
      | x -> x
    in
    let rec mergeall = function
      | [] -> []
      | [ l ] -> l
      | llist -> mergeall (merge2 llist)
    in
    mergeall (initlist l)

  let map f l = from_list (List.map f l)

  let rec mem l x =
    match l with
    | [] -> false
    | t :: q ->
        let c = X.compare x t in
        c = 0 || (c > 0 && mem q x)

  module Map = struct
    type 'a map = (X.t * 'a) list

    external get : 'a map -> (elem * 'a) list = "%identity"
    external unsafe_cast : (elem * 'a) list -> 'a map = "%identity"

    let empty = []
    let is_empty l = l = []
    let singleton x y = [ (x, y) ]
    let length = List.length
    let domain l = List.map fst l

    let rec iter f = function
      | (_, y) :: l ->
          f y;
          iter f l
      | [] -> ()

    let rec iteri f = function
      | (x, y) :: l ->
          f x y;
          iteri f l
      | [] -> ()

    let rec filter f = function
      | ((x, y) as c) :: l -> if f x y then c :: filter f l else filter f l
      | [] -> []

    let rec split f = function
      | ((x, y) as c) :: l ->
          if f x y then
            let a, b = split f l in
            (c :: a, b)
          else
            let a, b = split f l in
            (a, c :: b)
      | [] -> ([], [])

    let rec assoc_remove_aux v r = function
      | ((x, y) as a) :: l ->
          let c = X.compare x v in
          if c = 0 then (
            r := Some y;
            l)
          else if c < 0 then a :: assoc_remove_aux v r l
          else raise Not_found
      | [] -> raise Not_found

    let assoc_remove v l =
      let r = ref None in
      let l = assoc_remove_aux v r l in
      match !r with
      | Some x -> (x, l)
      | _ -> assert false

    (* TODO: is is faster to raise exception Not_found and return
       original list ? *)
    let rec remove v = function
      | ((x, _) as a) :: rem as l ->
          let c = X.compare x v in
          if c = 0 then rem else if c < 0 then a :: remove v rem else l
      | [] -> []

    let remove_min = function
      | b :: rem -> (b, rem)
      | _ -> failwith "SortedList.Map.remove_min"

    let compare_keys (k1, _) (k2, _) = Elem.compare k1 k2
    let subset_keys l1 l2 = subset_gen compare_keys l1 l2

    let rec merge f l1 l2 =
      match (l1, l2) with
      | ((x1, y1) as t1) :: q1, ((x2, y2) as t2) :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then (x1, f y1 y2) :: merge f q1 q2
          else if c < 0 then t1 :: merge f q1 l2
          else t2 :: merge f l1 q2
      | [], l2 -> l2
      | l1, [] -> l1

    let rec combine f1 f2 f12 l1 l2 =
      match (l1, l2) with
      | (x1, y1) :: q1, (x2, y2) :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then (x1, f12 y1 y2) :: combine f1 f2 f12 q1 q2
          else if c < 0 then (x1, f1 y1) :: combine f1 f2 f12 q1 l2
          else (x2, f2 y2) :: combine f1 f2 f12 l1 q2
      | [], l2 -> List.map (fun (x2, y2) -> (x2, f2 y2)) l2
      | l1, [] -> List.map (fun (x1, y1) -> (x1, f1 y1)) l1

    let rec cap f l1 l2 =
      match (l1, l2) with
      | (x1, y1) :: q1, (x2, y2) :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then (x1, f y1 y2) :: cap f q1 q2
          else if c < 0 then cap f q1 l2
          else cap f l1 q2
      | _ -> []

    let rec sub f l1 l2 =
      match (l1, l2) with
      | ((x1, y1) as t1) :: q1, (x2, y2) :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then (x1, f y1 y2) :: sub f q1 q2
          else if c < 0 then t1 :: sub f q1 l2
          else sub f l1 q2
      | l1, _ -> l1

    let merge_elem x l1 l2 = merge (fun _ _ -> x) l1 l2

    let rec merge_set f map set def =
      match (map, set) with
      | _, [] -> map
      | [], _ -> List.map (fun x -> (x, def)) set
      | ((x1, y1) as t1) :: q1, x2 :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then (x1, f y1 def) :: merge_set f q1 q2 def
          else if c < 0 then t1 :: merge_set f q1 set def
          else (x2, def) :: merge_set f map q2 def
    (* TODO: optimize this ? *)

    let rec union_disj l1 l2 =
      match (l1, l2) with
      | ((x1, _) as t1) :: q1, ((x2, _) as t2) :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then failwith "SortedList.Map.union_disj"
          else if c < 0 then t1 :: union_disj q1 l2
          else t2 :: union_disj l1 q2
      | [], l2 -> l2
      | l1, [] -> l1

    let add x v = union_disj [ (x, v) ]

    let rec diff l1 l2 =
      match (l1, l2) with
      | ((x1, _) as t1) :: q1, x2 :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then diff q1 q2
          else if c < 0 then t1 :: diff q1 l2
          else diff l1 q2
      | _ -> l1

    let rec restrict l1 l2 =
      match (l1, l2) with
      | ((x1, _) as t1) :: q1, x2 :: q2 ->
          let c = X.compare x1 x2 in
          if c = 0 then t1 :: restrict q1 q2
          else if c < 0 then restrict q1 l2
          else restrict l1 q2
      | _ -> []

    let from_list f l =
      let rec initlist = function
        | [] -> []
        | e :: rest -> [ e ] :: initlist rest
      in
      let rec merge2 = function
        | l1 :: l2 :: rest -> merge f l1 l2 :: merge2 rest
        | x -> x
      in
      let rec mergeall = function
        | [] -> []
        | [ l ] -> l
        | llist -> mergeall (merge2 llist)
      in
      mergeall (initlist l)

    let from_list_disj l =
      let rec initlist = function
        | [] -> []
        | e :: rest -> [ e ] :: initlist rest
      in
      let rec merge2 = function
        | l1 :: l2 :: rest -> union_disj l1 l2 :: merge2 rest
        | x -> x
      in
      let rec mergeall = function
        | [] -> []
        | [ l ] -> l
        | llist -> mergeall (merge2 llist)
      in
      mergeall (initlist l)

    let rec map_from_slist f = function
      | x :: l ->
          let v = (x, f x) in
          v :: map_from_slist f l
      | [] -> []

    let rec collide f l1 l2 =
      match (l1, l2) with
      | (_, y1) :: l1, (_, y2) :: l2 ->
          f y1 y2;
          collide f l1 l2
      | [], [] -> ()
      | _ -> assert false

    let rec may_collide f exn l1 l2 =
      match (l1, l2) with
      | (x1, y1) :: l1, (x2, y2) :: l2 when X.compare x1 x2 = 0 ->
          f y1 y2;
          may_collide f exn l1 l2
      | [], [] -> ()
      | _ -> raise exn

    let rec map f = function
      | (x, y) :: l -> (x, f y) :: map f l
      | [] -> []

    let rec mapi f = function
      | (x, y) :: l -> (x, f x y) :: mapi f l
      | [] -> []

    let rec mapi_to_list f = function
      | (x, y) :: l -> f x y :: mapi_to_list f l
      | [] -> []

    let fold f l acc = List.fold_left (fun tacc (k, v) -> f k v tacc) acc l

    let rec constant y = function
      | x :: l -> (x, y) :: constant y l
      | [] -> []

    let rec num i = function
      | [] -> []
      | h :: t -> (h, i) :: num (i + 1) t

    let rec map_to_list f = function
      | (_, y) :: l -> f y :: map_to_list f l
      | [] -> []

    let rec assoc v = function
      | (x, y) :: l ->
          let c = X.compare x v in
          if c = 0 then y else if c < 0 then assoc v l else raise Not_found
      | [] -> raise Not_found

    let rec assoc_present v = function
      | [ (_, y) ] -> y
      | (x, y) :: l ->
          let c = X.compare x v in
          if c = 0 then y else assoc_present v l
      | [] -> assert false

    let rec update f v d m =
      match m with
      | ((x, y) as h) :: l ->
          let c = X.compare x v in
          if c = 0 then (x, f y d) :: l
          else if c < 0 then h :: update f v d l
          else (v, d) :: m
      | [] -> [ (v, d) ]

    let replace v d m = update (fun _ d -> d) v d m

    let rec compare f l1 l2 =
      if l1 == l2 then 0
      else
        match (l1, l2) with
        | (x1, y1) :: l1, (x2, y2) :: l2 ->
            let c = X.compare x1 x2 in
            if c <> 0 then c
            else
              let c = f y1 y2 in
              if c <> 0 then c else compare f l1 l2
        | [], _ -> -1
        | _, [] -> 1

    let rec hash f = function
      | [] -> 1
      | (x, y) :: l -> X.hash x + (17 * f y) + (257 * hash f l)

    let rec equal f l1 l2 =
      l1 == l2
      ||
      match (l1, l2) with
      | (x1, y1) :: l1, (x2, y2) :: l2 ->
          X.equal x1 x2 && f y1 y2 && equal f l1 l2
      | _ -> false

    let rec check f = function
      | (x, a) :: ((y, _) :: _ as tl) ->
          X.check x;
          f a;
          assert (X.compare x y < 0);
          check f tl
      | [ (x, a) ] ->
          X.check x;
          f a
      | _ -> ()
  end

  module MakeMap (Y : Custom.T) = struct
    type t = Y.t Map.map

    (* Note: need to eta expand these definitions, because
       of the compilation of the recursive module definitions
       in types.ml... *)
    let hash x = Map.hash Y.hash x
    let compare x y = Map.compare Y.compare x y
    let equal x y = Map.equal Y.equal x y
    let check l = Map.check Y.check l

    let dump ppf l =
      List.iter
        (fun (x, y) -> Format.fprintf ppf "(%a->%a)" X.dump x Y.dump y)
        l
  end
end

module type FiniteCofinite = sig
  type elem

  type s = private
    | Finite of elem list
    | Cofinite of elem list

  include Custom.T with type t = s

  val empty : t
  val any : t
  val atom : elem -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val contains : elem -> t -> bool
  val disjoint : t -> t -> bool
  val is_empty : t -> bool
  val sample : t -> elem option
end

module FiniteCofinite (X : Custom.T) = struct
  type elem = X.t

  module SList = Make (X)

  type s =
    | Finite of SList.t
    | Cofinite of SList.t

  type t = s

  let hash = function
    | Finite l -> SList.hash l
    | Cofinite l -> (17 * SList.hash l) + 1

  let compare l1 l2 =
    match (l1, l2) with
    | Finite l1, Finite l2
    | Cofinite l1, Cofinite l2 ->
        SList.compare l1 l2
    | Finite _, Cofinite _ -> -1
    | _ -> 1

  let equal l1 l2 = compare l1 l2 = 0

  let check = function
    | Finite s
    | Cofinite s ->
        SList.check s

  let dump ppf = function
    | Finite s -> Format.fprintf ppf "Finite[%a]" SList.dump s
    | Cofinite s -> Format.fprintf ppf "Cofinite[%a]" SList.dump s

  let sample = function
    | Finite (x :: _) -> Some x
    | Finite [] -> raise Not_found
    | Cofinite _ -> None

  let empty = Finite []
  let any = Cofinite []
  let atom x = Finite [ x ]

  let cup s t =
    match (s, t) with
    | Finite s, Finite t -> Finite (SList.cup s t)
    | Finite s, Cofinite t -> Cofinite (SList.diff t s)
    | Cofinite s, Finite t -> Cofinite (SList.diff s t)
    | Cofinite s, Cofinite t -> Cofinite (SList.cap s t)

  let cap s t =
    match (s, t) with
    | Finite s, Finite t -> Finite (SList.cap s t)
    | Finite s, Cofinite t -> Finite (SList.diff s t)
    | Cofinite s, Finite t -> Finite (SList.diff t s)
    | Cofinite s, Cofinite t -> Cofinite (SList.cup s t)

  let diff s t =
    match (s, t) with
    | Finite s, Cofinite t -> Finite (SList.cap s t)
    | Finite s, Finite t -> Finite (SList.diff s t)
    | Cofinite s, Cofinite t -> Finite (SList.diff t s)
    | Cofinite s, Finite t -> Cofinite (SList.cup s t)

  let neg = function
    | Finite s -> Cofinite s
    | Cofinite s -> Finite s

  let contains x = function
    | Finite s -> SList.mem s x
    | Cofinite s -> not (SList.mem s x)

  let disjoint s t =
    match (s, t) with
    | Finite s, Finite t -> SList.disjoint s t
    | Finite s, Cofinite t -> SList.subset s t
    | Cofinite s, Finite t -> SList.subset t s
    | Cofinite _, Cofinite _ -> false

  let is_empty = function
    | Finite [] -> true
    | _ -> false
end

module FiniteCofiniteMap (X : Custom.T) (SymbolSet : FiniteCofinite) = struct
  include Custom.Dummy
  module T0 = Make (X)
  module TMap = T0.MakeMap (SymbolSet)
  module T = T0.Map

  type t =
    | Finite of TMap.t
    | Cofinite of TMap.t

  let sample = function
    | Cofinite _ -> None
    | Finite l -> (
        match T.get l with
        | [] -> raise Not_found
        | (x, y) :: _ -> Some (x, SymbolSet.sample y))

  let get = function
    | Finite l -> `Finite (T.get l)
    | Cofinite l -> `Cofinite (T.get l)

  let check = function
    | Finite l
    | Cofinite l ->
        TMap.check l

  let dump ppf = function
    | Finite s -> Format.fprintf ppf "Finite[%a]" TMap.dump s
    | Cofinite s -> Format.fprintf ppf "Cofinite[%a]" TMap.dump s

  let empty = Finite T.empty
  let any = Cofinite T.empty
  let any_in_ns ns = Finite (T.singleton ns SymbolSet.any)

  let finite l =
    let l =
      T.filter
        (fun _ x ->
          match x with
          | SymbolSet.Finite [] -> false
          | _ -> true)
        l
    in
    Finite l

  let cofinite l =
    let l =
      T.filter
        (fun _ x ->
          match x with
          | SymbolSet.Finite [] -> false
          | _ -> true)
        l
    in
    Cofinite l

  let atom (ns, x) = Finite (T.singleton ns (SymbolSet.atom x))

  let cup s t =
    match (s, t) with
    | Finite s, Finite t -> finite (T.merge SymbolSet.cup s t)
    | Finite s, Cofinite t -> cofinite (T.sub SymbolSet.diff t s)
    | Cofinite s, Finite t -> cofinite (T.sub SymbolSet.diff s t)
    | Cofinite s, Cofinite t -> cofinite (T.cap SymbolSet.cap s t)

  let cap s t =
    match (s, t) with
    | Finite s, Finite t -> finite (T.cap SymbolSet.cap s t)
    | Finite s, Cofinite t -> finite (T.sub SymbolSet.diff s t)
    | Cofinite s, Finite t -> finite (T.sub SymbolSet.diff t s)
    | Cofinite s, Cofinite t -> cofinite (T.merge SymbolSet.cup s t)

  let diff s t =
    match (s, t) with
    | Finite s, Cofinite t -> finite (T.cap SymbolSet.cap s t)
    | Finite s, Finite t -> finite (T.sub SymbolSet.diff s t)
    | Cofinite s, Cofinite t -> finite (T.sub SymbolSet.diff t s)
    | Cofinite s, Finite t -> cofinite (T.merge SymbolSet.cup s t)

  let is_empty = function
    | Finite l -> T.is_empty l
    | _ -> false

  let hash = function
    | Finite l -> 1 + (17 * TMap.hash l)
    | Cofinite l -> 2 + (17 * TMap.hash l)

  let compare l1 l2 =
    match (l1, l2) with
    | Finite l1, Finite l2
    | Cofinite l1, Cofinite l2 ->
        TMap.compare l1 l2
    | Finite _, Cofinite _ -> -1
    | _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let symbol_set ns = function
    | Finite s -> (
        try T.assoc ns s with
        | Not_found -> SymbolSet.empty)
    | Cofinite s -> (
        try SymbolSet.neg (T.assoc ns s) with
        | Not_found -> SymbolSet.any)

  let contains (ns, x) = function
    | Finite s -> (
        try SymbolSet.contains x (T.assoc ns s) with
        | Not_found -> false)
    | Cofinite s -> (
        try not (SymbolSet.contains x (T.assoc ns s)) with
        | Not_found -> true)

  let disjoint s t = is_empty (cap t s)

  (* TODO: OPT *)
end
