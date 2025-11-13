import "xml.q"

type Person = FPerson | MPerson
type FPerson = person[ @gender["F"], Name, Children ]
type MPerson = person[ @gender["M"], Name, Children ]
type Children = children[Person*]
type Name = name[ String ]

type Man = man[  @name[String], Sons,Daughters ]
type Woman = woman[ @name[String], Sons,Daughters ]
type Sons = sons[ Man* ]
type Daughters = daughters[ Woman* ]

fun split_children (val c as Person* ) : (Sons,Daughters) =
 let val s = filter c {( val m as MPerson { split_m(m) } | FPerson { () })*} in
 let val d = filter c {( MPerson { () } | val f as FPerson { split_f(f) })*} in
 sons[s], daughters[d]

fun split_m (val p as MPerson) : Man =
 match p with
  person[ @gender[String], name[val n], children[val c] ] ->
   man[ @name[n], split_children(c) ] 

fun split_f (val p as FPerson) : Woman =
 match p with
  person[ @gender[String], name[val n], children[val c] ] ->
   woman[ @name[n], split_children(c) ]

fun split_seq (val p as Person* ) : (Man|Woman)* =
 filter p { 
   ( val f as FPerson { split_f(f) } 
   | val m as MPerson { split_m(m) })* 
 }

let val _ = 
 match argv() with
  val fn as String ->
   ( match load_xml(fn) with
       doc[val p as Person*] -> doc[ split_seq(p) ]
     | Any -> raise("Invalid document") )
 | Any -> raise("Invalid command line")
