  $ cduce --verbose --compile complex_ok.cd
  val sort_string_list : [ String* ] -> [ String* ]
  val split : [ String [ String* ] [ String* ] String* ] -> [ [ String* ] String* ]
  val print_value : Any -> [  ]
  val p1 : Complex
  val mult : (Int -> Complex -> Complex) & (Int -> Float -> Float) & (Int -> Int -> Int) & (Complex -> Number -> Complex) & (Float -> Real -> Real) & (Float -> Complex -> Complex)
  $ cduce --run complex_ok.cdo
  { re=1. im=1. }
  1.41421356237
  { re=0. im=62.8 }
