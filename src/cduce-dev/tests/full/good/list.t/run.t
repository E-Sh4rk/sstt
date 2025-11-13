  $ cduce --verbose --compile list.cd
  val init : Int -> (Int -> 'a) -> [ 'a* ]
  val sort : ('a -> 'a -> Int) -> [ 'a* ] -> [ 'a* ]
  val iter : ('a -> [  ]) -> [ 'a* ] -> [  ]
  val flatten : X1 -> [ 'a* ] where X1 = [ X1* ] | 'a \ [ Any* ]
  val concat : [ 'a* ] -> [ 'b* ] -> [ 'a* 'b* ]
  val rev : [ 'a* ] -> [ 'a* ]
  val rev_append : [ 'a* ] -> [ 'b* ] -> [ ('a | 'b)* ]
  val fold_right : ('a -> 'b -> 'b) -> [ 'a* ] -> 'b -> 'b
  val fold_left : ('a -> 'b -> 'a) -> 'a -> [ 'b* ] -> 'a
  val nth : Int -> [ 'a* ] -> 'a
  val tl : [ 'a+ ] -> [ 'a* ]
  val hd : [ 'a+ ] -> 'a
  val length : [ 'a* ] -> Int
  $ cduce --run list.cdo
