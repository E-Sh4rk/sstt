let with_tag i t =
  let tt = Obj.dup (Obj.repr t) in
  Obj.set_tag tt i;
  tt
