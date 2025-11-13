  $ cduce --verbose --compile overloading.cd
  val sort2 : (FPerson -> Woman) & (MPerson -> Man)
  val sort : (FPerson -> Woman) & (MPerson -> Man)
  val base : Person
  $ cduce --run overloading.cdo
  <man name="Claude"><sons/><daughters><woman name="V&#233;ronique"><sons/><daughters><woman name="Ilaria"><sons/><daughters/></woman></daughters></woman></daughters></man>
