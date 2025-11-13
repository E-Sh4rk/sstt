  $ cduce --verbose --compile  integer_bad_div.cd
  File "integer_bad_div.cd", line 1, characters 8-16:
  Warning: This operator may fail
  val x : Int
  $ cduce --run  integer_bad_div.cdo
  Uncaught CDuce exception: "Division_by_zero"
  [1]
