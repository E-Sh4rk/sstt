  $ cduce --verbose --compile red-black.cd
  val cardinal : (<red elem='a>[ X1 X1 ] | <black elem='a>[ (RBtree ('a)) (RBtree ('a)) ] -> 1--*) & ([  ] -> 0) where X1 = <black elem='a>[ (RBtree ('a)) (RBtree ('a)) ] | [  ]
  val singleton : 'a -> Btree ('a)
  val member : ('a -> RBtree ('a) -> Bool) & ('a -> [  ] -> `false)
  val iter : ('a -> [  ]) -> RBtree ('a) -> [  ]
  val is_empty : (Any \ [  ] -> `false) & ([  ] -> `true)
  val insert : 'a -> Btree ('a) -> <black elem='a>[ (RBtree ('a)) (RBtree ('a)) ]
  val balance : (X1 -> X1) & (Unbalanced ('a) -> Rtree ('a)) where X1 = 'b \ Unbalanced (Any)
  $ cduce --run red-black.cdo
  1
  4
  10
  17
  100
  300
  24424
