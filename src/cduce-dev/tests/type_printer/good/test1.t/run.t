  $ ../../bin/test_printer.exe test1.cd
  OK:
  type Int
  -----printed and reparsed correctly
  OK:
  type (Bool |
                                                            Int -> Float) &
                                                            (Int -> Int)
  -----printed and reparsed correctly
  OK:
  type 
  [ Bool | Int* ]
  -----printed and reparsed correctly
  OK:
  type `b | `a
  -----printed and reparsed correctly
  OK:
  type 
  X1 where X1 = (`s,(X1,X1)) | (`c,(S2,S2)) | `e
  -----printed and reparsed correctly
  OK:
  type 
  ((`d,(`e,Any)) -> (`c,(`a,`a))) & ((`c,(`e,Any)) -> (`c,(`e,`e))) &
  ((`d,X3) -> (`c,(`b,`b))) & ((`c,X3) -> (`c,(X2,X2))) &
  ((`c,(X1,Any)) -> (`c,(X1,X1))) & (`e -> `e) where
  X1 = Any \ ((`c,(S2,S2)) | (`d,(S1,S1)) | `e) and
  X2 = (`c,(S2,S2)) | (`d,(S1,S1)) and
  X3 = (X2,Any)
  -----printed and reparsed correctly
