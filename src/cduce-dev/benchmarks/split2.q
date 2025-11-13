import "xml.q"

type Person = FPerson | MPerson
type MPerson = person[ @gender["M"], Name, Children ]
type FPerson = person[ @gender["F"], Name, Children ]
type Children = children[Person*]
type Name = name[ String ]

type Man = man[  @name[String], Sons,Daughters ]
type Woman = woman[ @name[String], Sons,Daughters ]
type Sons = sons[ Man* ]
type Daughters = daughters[ Woman* ]

fun split_children (val c as Person* ) : (Sons,Daughters) =
 let val s = filter c {( val m as ~[ @gender["M"], ~[Any]* ] { split_m(m) } | ~[Any] { () })*} in
 let val d = filter c {( val f as ~[ @gender["F"], ~[Any]* ] { split_f(f) } | ~[Any] { () })*} in
 sons[s], daughters[d]

fun split_m (val p as MPerson) : Man =
 match p with
  ~[ @gender[String], ~[val n], ~[val c] ] ->
   man[ @name[n], split_children(c) ] 

fun split_f (val p as FPerson) : Woman =
 match p with
  ~[ @gender[String], ~[val n], ~[val c] ] ->
   woman[ @name[n], split_children(c) ]

fun split_seq (val p as Person* ) : (Man|Woman)* =
 filter p { 
   ( val f as ~[ @gender["F"], ~[Any], ~[Any] ] { split_f(f) } 
   | val m as ~[ @gender["M"], ~[Any], ~[Any] ] { split_m(m) })* 
 }

let val _ = 
 match argv() with
  val fn as String ->
   ( match load_xml(fn) with
       doc[val p as Person*] -> doc[ split_seq(p) ]
     | Any -> raise("Invalid document") )
 | Any -> raise("Invalid command line")

