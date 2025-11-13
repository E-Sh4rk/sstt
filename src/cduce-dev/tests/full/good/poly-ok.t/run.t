  $ cduce --verbose --compile poly-ok.cd
  val mmap : ('a -> 'b) -> [ 'a* ] -> [ 'b* ]
  val even : ('a \ Int -> 'a \ Int) & (Int -> Bool)
  val pretty : Int -> String
  val id : 'a -> 'a
  val g : ((Bool -> Bool) -> Bool -> Bool) & ((Int -> Int) -> Int -> Int)
  val gid : (Int -> Int) & (Bool -> Bool)
  val id2g : ((Bool -> Bool) -> Bool -> Bool) & ((Int -> Int) -> Int -> Int)
  val max : 'a -> 'a -> 'a
  val f : 'a | 'b | 'c -> ( *--0 | 4--*) & 'd & 'e -> Any
  val sum : Int -> Int -> Int
  val f : ('a -> 'a -> 'a) -> 'a -> B ('a) | A ('a) -> A ('a)
  val x : Int -> X1 -> <a>Int where X1 = <b>[ X1 ] | <a>Int
  val f : B ('a) -> 'a
  val v : 32
  $ cduce --run poly-ok.cdo
