type espace_recherche = {
  tab : int array ;       (* le tableau dont on cherche le max *)
  g : int ;               (* la borne gauche, au sens large *)
  d : int ;               (* la borne droite, au sens large *)
  mutable ret : int ;     (* la valeur de retour *)
}

let k = ref 0

let rec task (data: espace_recherche): unit =
  let { g; d; tab; _ } = data in
  incr k;
  Printf.printf "%d -> [| %d, %d |]\n" !k g d;
  if g = d - 2 then
    data.ret <- max tab.(g) tab.(d - 1)
  else if g = d - 1 || g = d then
    data.ret <- tab.(g)
  else begin
    let mid = (g + d) / 2 in
    let a = { data with d = mid; ret = 0 }
    and b = { data with g = mid; ret = 0 } in
    let ta = Thread.create task a
    and tb = Thread.create task b in
    Thread.join ta;
    Thread.join tb;
    data.ret <- max a.ret b.ret
  end

let () =
  let tab = Array.init 256 (fun _ -> Random.int 250) in
  Array.iter (Printf.printf "%d ") tab;
  print_newline();
  let data = { tab; g = 0; d = Array.length tab - 1; ret = 0 } in
  task data;
  print_int data.ret
