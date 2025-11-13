let random_string n =
  let s = String.create n in
  for i = 0 to n - 1 do
    s.[i] <- Char.chr (65 + Random.int 26)
  done;
  s

let nb = int_of_string Sys.argv.(1);;

print_string "<?xml version=\"1.0\" encoding=\"UTF-8\"?><addrbook>";;

for i = 1 to nb do
  Printf.printf "<person><name>%s</name>" (random_string 20);
  if Random.int 2 = 0 then Printf.printf "<tel>%s</tel>" (random_string 20);
  for j = 1 to Random.int 4 do
    Printf.printf "<email>%s</email>" (random_string 20)
  done;
  Printf.printf "</person>"
done
;;

print_string "</addrbook>"
