open Cduce_types

let bdds = ref []
let rand_limit = 1000
let dummy = Types.Int.Dnf.empty
let cache = Array.make rand_limit dummy

let v i =
  let t = cache.(i) in
  if t != dummy then t
  else
    let t = Types.Int.Dnf.var (Var.mk (string_of_int i)) in
    cache.(i) <- t;
    t

let () =
  for j = 0 to 10000 do
    let i = Random.int rand_limit in
    bdds := (j, v i) :: !bdds
  done

let () =
  Format.eprintf "Starting test@\n";
  let t0 = Unix.gettimeofday () in
  let _res =
    List.fold_left
      (fun acc (i, bdd) ->
        if i mod 2 == 0 then Types.Int.Dnf.cap acc bdd
        else Types.Int.Dnf.(cup acc bdd))
      dummy !bdds
  in
  let t1 = Unix.gettimeofday () in
  Format.eprintf "Time: %fms@\n" (1000. *. (t1 -. t0))
