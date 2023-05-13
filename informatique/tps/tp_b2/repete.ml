let () =
  if Array.length Sys.argv = 3 then
    let n = int_of_string Sys.argv.(1) and s = Sys.argv.(2) in
    for i = 1 to n do
      print_string s;
      print_char ' ';
    done
  else
    Printf.printf "usage : %s nb str répète nb fois la chaîne de caractères" Sys.argv.(0)

