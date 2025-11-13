
val (=) : (int,int) | (bool,bool) | (string,string) | (char,char) | (float,float) -> bool

#infer_overload = false
#type_narrowing = false

(* ===== Ad-hoc Arithmetic (non overloaded) ===== *)

let f' x =
  match x with
  | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4
  | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8 | 9 -> 9
  | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14
  | 15 -> 15 | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19
  | 20 -> 20 | 21 -> 21 | 22 -> 22 | 23 -> 23 | 24 -> 24
  | _ -> (-1)
  end

let test' (y:(10..15)) = f' y

let succ' x =
  match x with
  | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | 4 -> 5
  | 5 -> 6 | 6 -> 7 | 7 -> 8 | 8 -> 9 | 9 -> 10
  | 10 -> 11 | 11 -> 12 | 12 -> 13 | 13 -> 14 | 14 -> 15
  | 15 -> 16 | 16 -> 17 | 17 -> 18 | 18 -> 19 | 19 -> 20
  | 20 -> 21 | 21 -> 22 | 22 -> 23 | 23 -> 24 | 24 -> 25
  | _ -> 0
  end

let test2' (y:(5..15)) = succ' (succ' (succ' (succ' (succ' y))))

(* ===== Ad-hoc Arithmetic (overloaded) ===== *)

#infer_overload = true

let f x =
  match x with
  | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4
  | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8 | 9 -> 9
  | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14
  | 15 -> 15 | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19
  | 20 -> 20 | 21 -> 21 | 22 -> 22 | 23 -> 23 | 24 -> 24
  | _ -> (-1)
  end

let test (y:(10..15)) = f y

let succ x =
  match x with
  | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | 4 -> 5
  | 5 -> 6 | 6 -> 7 | 7 -> 8 | 8 -> 9 | 9 -> 10
  | 10 -> 11 | 11 -> 12 | 12 -> 13 | 13 -> 14 | 14 -> 15
  | 15 -> 16 | 16 -> 17 | 17 -> 18 | 18 -> 19 | 19 -> 20
  | 20 -> 21 | 21 -> 22 | 22 -> 23 | 23 -> 24 | 24 -> 25
  | _ -> 0
  end

let test2 (y:(5..15)) = succ (succ (succ (succ (succ y))))

(* ===== Boolean operations ===== *)

let to_bool x =
  match x with
  | 0 -> false
  | :int -> true
  | false -> false
  | true -> true
  | y & :float -> y = 0.0
  | "" -> false
  | :string -> true
  | '\000' -> false
  | :char -> true
  end

let and_ (x,y) =
  match x,y with
  | true,true -> true
  | _ -> false
  end

let (&&) (x,y) = and_ (to_bool x, to_bool y)

let and1 = 42 && 0
let and2 = "" && true
let and3 = 42 && "42"
let and4 = 17.0 && 42.0
let and5 = 'c' && "c"

let not_ x = if x is true then false else true

let or_ = fun (x,y) -> not_ (and_ (not_ x, not_ y))

(* ===== Type narrowing ===== *)

#type_narrowing = true

val strlen : string -> int
val add : int -> int -> int
val add1 : int -> int

let impossible_branch = fun x ->
  if x is int then add1 x else (42 3)

let impossible_branch2 = fun x -> fun y ->
  if y is int then add1 y else add1 x

let switch1 f s a b =
  if s then f a else f b

let switch2 s f a b =
  if s then f a else f b

let typeof x =
  if x is ()|[] then "[]"
  else if x is string then "String"
  else if x is char then "Character"
  else if x is int then "Number"
  else if x is bool then "Boolean"
  else "Object"

let typeof_patterns x =
  match x with
  | :() | :[] -> "[]"
  | :string -> "String"
  | :char -> "Character"
  | :int -> "Number"
  | :bool -> "Boolean"
  | :any -> "Object"
  end

let land x y = and_ (x,y)
let lor x y = or_ (x,y)
let lnot x = not_ x

let tautology = fun x -> fun y ->
  land (lor x (lnot x)) (lor (lnot y) y)

let tautology_ann = fun (x:any) -> fun (y:any) ->
  suggest x is true or ~true in
  suggest y is true or ~true in
  land (lor x (lnot x)) (lor (lnot y) y)

let test_many_params_ann1 (a:any) (b:any) (c:any) (d:any) (e:any) (f:any) =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let test_many_params_ann2 (a:'a) (b:'b) (c:'c) (d:'d) (e:'e) (f:'f) =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let test_many_params a b c d e f =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let match_pair (x:any) (y:any) =
  match (x,y) with
  | :(int, bool) -> (add1 x, lnot y)
  | _ -> false
  end

let type_narrowing_fail (f:any -> any) x =
  if f x is int then add1 (f x) else 0

let type_narrowing_ok (f:any -> any) x =
  let y = f x in
  if y is int then add1 y else 0

let type_narrowing2_ok (f:(any -> any) & ('a -> false)) (x:any) =
  if f x then x else 42

let type_narrowing2_ok' ((f,x): ((any -> any) & ('a -> false), any)) =
  if f x then x else 42

(* Examples from Tobin-Hochstadt & Felleisen *)

let is_string = fun x ->
     if x is string then true else false
let is_int = fun x ->
     if x is int then true else false

val f : (int | string) -> int
val g : (int, int) -> int

let example1 = fun (x:any) ->
  if x is int then add1 x else 0

let implicit1 = fun x ->
  if x is int then add1 x else 0


let example2 = fun (x:string|int) ->
  if x is int then add1 x else strlen x

let implicit2 = fun x ->
  if x is int then add1 x else strlen x


let example3 = fun (x: any) ->
  if x is (any \ false) then (x,x) else false

let implicit3 = fun x ->
  if x is (any \ false) then (x,x) else false


let example4 = fun (x : any) ->
  if or_ (is_int x, is_string x) is true then x else 'A'

let implicit4 = fun x ->
  if or_ (is_int x, is_string x) is true then x else 'A'


let example5 = fun (x : any) -> fun (y : any) ->
  if and_ (is_int x, is_string y) is true then
   add x (strlen y) else 0

let implicit5 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add x (strlen y) else 0

(* Annotations for this one are invalid: y can be any only if x is string *)
let example6_invalid = fun (x : int|string) -> fun (y : any) ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x

val example6 : (int -> string -> int) & (string -> any -> int)
let example6 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x

let implicit6 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x


let example7 = fun (x : any) -> fun (y : any) ->
  if (if is_int x is true then is_string y else false) is true then
   add x (strlen y) else 0

let implicit7 = fun x -> fun y ->
  if (if is_int x is true then is_string y else false) is true then
   add x (strlen y) else 0


let example8 = fun (x : any) ->
  if or_ (is_int x, is_string x) is true then true else false

let implicit8 = fun x ->
  if or_ (is_int x, is_string x) is true then true else false


let example9 = fun (x : any) ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0

let implicit9 = fun x  ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0


let example10 = fun (p : (any,any)) ->
  if is_int (fst p) is true then add1 (fst p) else 7

let implicit10 = fun p ->
  if is_int (fst p) is true then add1 (fst p) else 7

let example11 = fun (p : (any, any)) ->
  if and_ (is_int (fst p), is_int (snd p)) is true then g p else No

let implicit11 = fun p ->
  if and_ (is_int (fst p), is_int (snd p)) is true then g p else No

let example12 = fun (p : (any, any)) ->
  if is_int (fst p) is true then true else false

let implicit12 = fun p ->
  if is_int (fst p) is true then true else false


let example13 =
 fun (x : any) ->
   fun (y : any) ->
    if and_ (is_int x, is_string y) is true then 1
    else if is_int x is true then 2
    else 3

let implicit13 =
 fun x ->
   fun y ->
    if and_ (is_int x, is_string y) is true then 1
    else if is_int x is true then 2
    else 3

let example14 = fun (input : int|string) ->
  fun (extra : (any, any)) ->
    if and_(is_int input , is_int(fst extra)) is true then
        add input (fst extra)
    else if is_int(fst extra) is true then
        add (strlen input) (fst extra)
    else 0

let implicit14 = fun input ->
  fun extra ->
    if and_(is_int input , is_int(fst extra)) is true then
        add input (fst extra)
    else if is_int(fst extra) is true then
        add (strlen input) (fst extra)
    else 0
