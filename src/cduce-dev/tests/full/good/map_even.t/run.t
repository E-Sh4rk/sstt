  $ cduce --verbose --compile map_even.cd
  val even : ('c \ Int -> 'c \ Int) & (Int -> Bool)
  val fmap : ('a -> 'b) -> [ 'a* ] -> [ 'b* ]
  val l : [ (`HELLO | Bool)* ]
  $ cduce --run map_even.cdo
  [ `false `true `false `true `HELLO ]
