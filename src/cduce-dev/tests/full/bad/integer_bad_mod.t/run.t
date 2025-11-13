  $ cduce --verbose --compile  integer_bad_mod.cd
  File "integer_bad_mod.cd", line 1, characters 8-16:
  Warning: This operator may fail
  val x : Int
  $ cduce --run  integer_bad_mod.cdo
  Uncaught CDuce exception: "Division_by_zero"
  [1]
