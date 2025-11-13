  $ cduce --verbose --compile addrbook.cd
  val addrbook : Addrbook
  val mkTelListContent : Content -> [ (Name Tel)* ]
  val sort_string_list : [ String* ] -> [ String* ]
  val split : [ String [ String* ] [ String* ] String* ] -> [ [ String* ] String* ]
  val print_value : Any -> [  ]
  val mkTelListAcc : Addrbook -> [ (Name Tel)* ]
  $ cduce --run addrbook.cdo
  [ <name>[ 'Benjamin Pierce' ] <tel>[ '123-456-789' ] ]
  [ <name>[ 'Benjamin Pierce' ] <tel>[ '123-456-789' ] ]
