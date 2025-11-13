Create a custom toplevel with the Gen module linked in. Gen is part of sedlex's
dependencies which means it will always be present when we run tests. We cannot
easily silence ocamlfind's warning so just ignore the output.
  $ cduce_mktop -cduce cduce -p gen cduce_gen.exe prims >/dev/null 2>&1
  $ ./cduce_gen.exe --compile --verbose test.cd
  val sort_string_list : [ String* ] -> [ String* ]
  val split : [ String [ String* ] [ String* ] String* ] -> [ [ String* ] String* ]
  val print_value : Any -> [  ]
  val of_list_123 : [  ] -> [ 1--3? ]
  val loop : [  ] -> [  ]
  $ ./cduce_gen.exe --run test.cdo
  [ 1 ]
  [ 2 ]
  [ 3 ]
