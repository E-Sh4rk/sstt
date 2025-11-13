let random_string n =
  let s = String.create n in
  for i = 0 to n - 1 do
    s.[i] <- Char.chr (65 + Random.int 26)
  done;
  s

let nb = int_of_string Sys.argv.(1);;

print_string "<?xml version=\"1.0\" encoding=\"UTF-8\"?><doc>"

let rec person p =
  Printf.printf "<person gender=\"%s\"><name>%s</name><children>\n"
    (if Random.int 2 = 0 then "M" else "F")
    (random_string 20);
  if p < 5 then
    for i = 1 to Random.int 5 do
      person (p + 1)
    done;
  Printf.printf "</children></person>\n"
;;

for i = 1 to nb do
  person 0
done
;;

print_string "</doc>"
