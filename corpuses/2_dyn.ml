val (+) : (int, int) -> int
val (-) : (int, int) -> int
val ( * ) : (int, int) -> int
val (/) : (int, int) -> int
val (%) : (int, int) -> int
val (@) : (['a*], ['b*]) -> ['a* 'b*]

val (<) : (int, int) -> bool
val (<=) : (int, int) -> bool
val (>) : (int, int) -> bool
val (>=) : (int, int) -> bool

(* ========= SIGNATURES & ANNOTATIONS ========= *)

val valint : int
let valint = true
let valint = 42

val test_sig : 'a|bool -> 'a|int -> ('a|bool,'a|int)
let test_sig x y = (y,x)
let test_sig x y = (x,y)

val test_sig_overloaded : (int -> int) & (bool -> bool)
let test_sig_overloaded x = x

let test_annot (x:'a|bool) (y:'a|int) = (x,y)

(* ========= SIMPLE CONSTRUCTOR TESTS ======== *)

let proj_a (A(v)) = v
let proj_ab x =
  match x with
  | A(v) -> v
  | B(v) -> v
  end

let proj_a_test x = proj_a A(x)
let proj_ab_test x y = (proj_ab A(x), proj_ab B(y))

type clist('a) = Nil | Cons('a, clist('a))
let map_clist f (lst:clist('a)) =
  match lst with
  | Cons(v,tail) -> Cons(f v, map_clist f tail)
  | Nil -> Nil
  end

let record_ab_open ({ a ; b ..}) = { a ; b }
let record_ab ({ a ; b }) = { a ; b }

(* ========= OPAQUE TYPES & IMPERATIVE PROGRAMMING ======== *)

abstract type ref('a)
val ref : 'a -> ref('a)
val (<-) : (ref('a), 'a) -> ()
val (!) : ref('a) -> 'a

val ref_42 : ref(int)
let ref_42 = ref 42
let ref_42_unresolved = ref 42
let mutate_ref x =
  let y = ref x in
  y <- 42 ; !y

let is_ref x = if x is ref then true else false
let is_not_ref x = if x is ~ref then true else false

abstract type dict('k, 'v)
abstract type array('a)

val dict : () -> dict('a, 'b)
val array : () -> array('a)
val ([]<-) : ((dict('a, 'b), 'a, 'b) -> ()) & ((array('b), int, 'b) -> ())
val ([]) : ((dict('a, 'b), 'a) -> 'b) & ((array('b), int) -> 'b)
val push : array('a) -> 'a -> ()
val len : array('a) -> int

let test_dict x =
  let d = dict () in
  d[x]<- 42 ;
  d["key"]<- 0 ;
  d, d[false]

let filter_arr (f:('a -> any) & ('b -> ~true)) (arr:array('a|'b)) =
  let res = array () in
  let i = ref 0 in
  while !i < (len arr) do
    let e = arr[!i] in
    if f e do push res e end ;
    i <- (!i + 1)
  end ;
  res

(* val test_arr : 'a -> array('a | 'b) *)
let test_arr x =
  let arr = array () in
  push arr true ;
  push arr x ;
  push arr false ;
  filter_arr (fun x -> if x is int then true else false) arr

let test_double_array =
  let arr = array () in
  arr[0]<- (array ()) ;
  (arr[0])[0]<- 42 ;
  (arr[0])[0]

(* val arr_dict_assign: (array('a)|dict(int,'a) -> ()) *)
let arr_dict_assign x = x[0]<- x[1]

let nested x y =
  let d = dict () in
  d[x]<- (array ()) ;
  (d[x])[0]<- y ; (d[x])[0]

(* val swap : ('a -> 'a -> dict('a,'b) -> ())
         & (int -> int -> array('b) -> ()) *)
let swap i j x =
    let tmp = x[i] in
    x[i]<- x[j] ; x[j]<- tmp

(* ========= POLYMORPHISM ======== *)

val succ : int -> int
type falsy = false | "" | 0
type truthy = ~falsy

let test a = (fst a, fst a)

let and_js = fun x -> fun y ->
  if x is falsy then x else y

let not_js = fun x -> if x is falsy then 1 else 0

let or_js = fun x -> fun y ->
  if x is truthy then x else y

let identity_js = fun x -> or_js x x

let and_pair = fun x -> fun y ->
  if x is falsy then x else (y, succ x)

(* val test_pair : ((int \ 0, any) | (int, int) -> int) *)
let test_pair = fun x ->
  if fst x is falsy then (fst x) + (snd x) else succ (fst x)

type tt('a, 'b)  =  'a -> 'b -> 'a
type ff('a, 'b)  =  'a -> 'b -> 'b

val ifthenelse : (tt('c, 'd) -> 'c -> 'd -> 'c) & (ff('c, 'd) -> 'c -> 'd -> 'd)
let ifthenelse b x y = b x y

let test1_patterns (a,_) = a

let test2_patterns x =
  match x with (a,_)&(_,b) -> (a,b) end

let test3_patterns x y =
  let pack x y = (x,y) in
  let (y,x) = pack x y in
  pack x y

(* ========= RECURSIVE FUNCTIONS ========= *)

let fact (x:int) =
  if x is 0 then 1 else x * (fact (x-1))

let map f (lst:['a*]) =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map f lst)
  end

let map_noannot f lst =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map_noannot f lst)
  end

let foo x = bar x
and bar y = foo y

val filter : ('a->any) & ('b -> ~true) -> [('a|'b)*] -> [('a\'b)*]
let filter f l =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter f l)
    else filter f l
  end

let filter2 (f: ('a->any) & ('b -> ~true)) (l:[('a|'b)*]) =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter2 f l)
    else filter2 f l
  end

let filter_noannot f l =
  match l with
  | [] -> []
  | e::l ->
    let l = filter_noannot f l in
    if f e is true then e::l else l
  end

val filtermap :
    (('t -> ((true, 'u) | false), ['t*]) -> ['u*])
  & (('t -> ((true, 'u) | bool), ['t*]) -> [('t | 'u)*])
let filtermap (f, l) =
    match l with
    | [] -> []
    | x::xs ->
      match f x with
      | false -> filtermap (f, xs)
      | true -> x::(filtermap (f, xs))
      | (true, y) -> y::(filtermap (f, xs))
    end
  end

type objF('a) = { f :? 'a ; proto :? objF('a) ..}

let call_f (o:objF('a)) =
  if o is { f : any ..} then o.f
  else if o is { proto : any ..}
  then call_f o.proto
  else ()

type tree('a) = [ ('a\[any*] | tree('a))* ]
let deep_flatten (l : tree('a)) =
  match l with
  | [] -> []
  | (x & :list)::y -> (deep_flatten x) @ (deep_flatten y)
  | x::y -> x::(deep_flatten y)
  end

type expr = ("const", (0..)) | ("add", (expr, expr)) | ("uminus", expr)

let eval (e:expr) =
  match e with
  | (:"add", (e1, e2)) -> (eval e1) + (eval e2)
  | (:"uminus", e) -> 0 - (eval e)
  | (:"const", x) -> x
  end

let rec_and_imp arr k i n =
  if k < n do arr[k]<- (i+k) ; rec_and_imp arr (k+1) i n end

let interval i j =
  let arr = array () in
  rec_and_imp arr 0 i ((j-i)+1) ; arr

(* ========= GRADUAL TYPING ========= *)

val lnot : (true -> false) & (false -> true)
val reflect

let test_reflect x = reflect x

val test_reflect_ann : int -> int
let test_reflect_ann x = reflect x

let gradual1 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  end

let gradual2 x =
  match reflect x with
  | y & :int -> y
  | y & :bool -> y
  end

let gradual3 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> y
  end

let gradual4 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> 42::y
  end

val gradual4_ann : any -> int | bool
let gradual4_ann x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> 42::y
  end

(* ========= CONTROL FLOW ========= *)

let typeof_cf x =
  if x is string do return "string" end ;
  if x is bool do return "bool" end ;
  if x is int do return "int" end ;
  if x is char do return "char" end ;
  if x is () do return "unit" end ;
  "object"

let break_return_cf x =
  while x is ~false do
    if x is 0 do break end ;
    return x
  end

let weird_return x =
  42, (if x then return 69 else 69)

(* ========= BUILTIN MUTABLE VARS & IMPERATIVE PROGRAMMING ========= *)

let mut mx : int = 42
let mut_invalid =
  mx := true ; mx
let mut_valid =
  mx := 69 ; mx

val mut my
let mut my = 42
let read_mut = my
let read_mut_cast = (my :>> bool)
let write_mut = my := false

let mut_narrowing =
  let mut y = 42 in
  while y is ~Nil do
    y := succ y ;
    (* ... *)
    if y > 100 do y := Nil end
  end ;
  y

let mut_seq1 =
  let mut y = false in
  while <bool> do y := false end ;
  y := 42 ;
  while <bool> do y := 42 end ;
  y

let mut_seq2 =
  let mut y = false in
  while <bool> do y := false end ;
  y := 42 ;
  while <bool> do y := 42 end ;
  y := Nil ;
  while <bool> do y := Nil end ;
  y

let mut_and_return (_) =
    if mx is 0 do
        mx := 69
    else
        mx := 0 ; return false
    end ;
    mx

let neg_and_pos x =
  let mut x = x in
  if x is Nil do return x end ;
  if x < 0 do x := 0-x end ;
  x := (0-x,x) ;
  return x

let neg_and_pos_ann (x:int|Nil) =
  let mut x = x in
  if x is Nil do return x end ;
  if x < 0 do x := 0-x end ;
  x := (0-x,x) ;
  return x

let order (x:(int|Nil,int|Nil)|Nil) =
  let mut x = x in
  if x is Nil do return x end ;
  if fst x is Nil do return snd x end ;
  if snd x is Nil do return fst x end ;
  if (snd x) < (fst x) do x := (snd x, fst x) end ;
  return x

val rand : () -> any
val is_int : (int -> true) & (~int -> false)
let loop_tricky_narrowing y =
  let mut x in
  let mut y = y in
  while is_int (x := rand () ; x) do
    y := y + x
  end ;
  return (x,y)

let loop_invalid x =
  let mut x = x in
  while true do
    x := x + 1 ;
    x := false
  end ;
  x

let loop_valid x =
  let mut x = x in
  while true do
    if x is ~int do return x end ;
    x := x + 1 ;
    x := false
  end ;
  x

let filter_imp (f:('a -> bool) & ('b -> false)) (arr:array('a|'b)) =
  let res = array () in
  let mut i = 0 in
  while i < (len arr) do
    let e = arr[i] in
    if f e do push res e end ;
    i := i + 1
  end ;
  return res

val filter_imp_test : array(42|13|"abc")
(* we have to specify a type for filter_imp_test,
   otherwise the type inference will infer array(42|13|"abc"|'a) where 'a cannot be generalized,
   which will error as top-level definitions cannot have unquantified type variables *)
let filter_imp_test =
  let mut arr = array () in
  push arr 42 ;
  push arr false ;
  push arr 13 ;
  arr := filter_imp (fun x -> (x is int)) arr ;
  push arr "abc" ;
  arr

(* ========= ALTERNATIVES ========= *)

val f1 : int -> int
val f2 : bool -> bool
val f3 : string -> string
val f4 : char -> char
val f5 : () -> ()
val f6 : Nil -> Nil

let test_alt a = [ f1 a | f2 a | f3 a | f4 a | f5 a | f6 a ]
let fall = [ f1 | f2 | f3 | f4 | f5 | f6 ]
let test_noalt a = fall a
