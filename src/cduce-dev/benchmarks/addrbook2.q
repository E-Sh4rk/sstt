import "xml.q"

type Addrbook = addrbook[Person*]
type Person = person[(Name,Tel?,Email* )]
type Name = name[String]
type Tel = tel[String]
type Email = email[String]

fun mkTelbook (val ps as Person* ) : entry[(Name,Tel)]* =
  filter ps {
    ( person[name[val n], tel[val t], val e]
          { entry[name[n], tel[t]] }
    | person[name[val n], val e as Email*]
          { () }
    )*      
  } 

let val _ = 
 match argv() with
  val fn as String ->
   ( match load_xml(fn) with
       addrbook[val p as Person*] -> mkTelbook(p)
     | Any -> raise("Invalid document") )
 | Any -> raise("Invalid command line")
