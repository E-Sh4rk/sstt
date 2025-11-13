  $ cduce --verbose --compile patricia.cd
  val init : [ (Caml_int,'a)* ] -> Dict ('a)
  val update : 'a -> 'a -> 'a
  val iter : ('a -> [  ]) -> Dict ('a) -> [  ]
  val iteri : (Caml_int -> 'a -> [  ]) -> Dict ('a) -> [  ]
  val merge : ('a -> 'a -> 'a) -> (Dict ('a),Dict ('a)) -> Dict ('a)
  val swap : X1 -> X1 where X1 = 'a -> 'a -> 'a
  val max : 'a -> 'b -> 'a | 'b
  val insert : ('a -> 'a -> 'a) -> Caml_int -> 'a -> Dict ('a) -> Branch ('a) | Leaf ('a)
  val join : Caml_int -> X1 -> Caml_int -> X1 -> Branch ('a) where X1 = <brch bit=Caml_int pre=Caml_int>[ X1 X1 ] | <leaf key=Caml_int>'a
  val lookup : Caml_int -> Dict ('a) -> [ 'a? ]
  val zero_bit : Caml_int -> Caml_int -> Bool
  val match_prefix : Caml_int -> Caml_int -> Caml_int -> Bool
  val mask : Caml_int -> Caml_int -> Caml_int
  val branching_bit : Caml_int -> Caml_int -> Caml_int
  val lowest_bit : Caml_int -> Caml_int
  $ cduce --run patricia.cdo
  "0"
  "B"
  "HELLO"
  "WORLD"
