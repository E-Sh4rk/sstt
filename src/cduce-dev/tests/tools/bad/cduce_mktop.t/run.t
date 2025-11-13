Create a custom toplevel with the Gen module linked in. Gen is part of sedlex's
dependencies which means it will always be present when we run tests.
  $ cduce_mktop -cduce cduce -p gen cduce_gen.exe prims >/dev/null 2>&1
  $ cduce_mktop -cduce cduce -p gen cduce_gen.exe prims
  Error: file 'cduce_gen.exe' already exists, please remove it
  [2]
  $ rm -f cduce_gen.exe; touch prims.cmo ; cduce_mktop -cduce cduce -byte -p gen cduce_gen.exe prims
  Error: file 'prims.cmo' already exists, please remove it
  [2]
  $ rm -f prims.cmo; touch prims.cmx ; cduce_mktop -cduce cduce -p gen cduce_gen.exe prims
  Error: file 'prims.cmx' already exists, please remove it
  [2]
