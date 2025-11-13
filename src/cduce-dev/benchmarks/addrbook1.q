import "xml.q"

type Addrbook = addrbook[Person*]
type Person = person[(Name,Tel?,Email* )]
type Name = name[String]
type Tel = tel[String]
type Email = email[String]

fun mkTelbook (val ps as Person* ) : entry[(Name,Tel)]* =
  match ps with
    person[name[val n], tel[val t],val e],val rest
        -> entry[name[n], tel[t]], mkTelbook(rest)
  | person[name[val n],val e],val rest 
        -> mkTelbook(rest)
  | () 
        -> ()

let val _ = 
 match argv() with
  val fn as String ->
   ( match load_xml(fn) with
       addrbook[val p as Person*] -> mkTelbook(p)
     | Any -> raise("Invalid document") )
 | Any -> raise("Invalid command line")
