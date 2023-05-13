let swap (t: 'a array) (i: int) (j: int): unit =
  let tmp = t.(j) in
  t.(j) <- t.(i);
  t.(i) <- tmp


let n = 200
let tab = Array.init n (fun _ -> Random.int 100)
let est_trie = ref false
let mutexs = Array.init n (fun _ -> Mutex.create())

let trieur (i: int): unit =
  while not !est_trie do
    Mutex.lock mutexs.(i);
    Mutex.lock mutexs.(i + 1);
    if tab.(i) > tab.(i + 1) then
      swap tab i (i + 1);
    Mutex.unlock mutexs.(i);
    Mutex.unlock mutexs.(i + 1)
  done


let verifieur (): unit =
  let exception PasTrie in
  while not !est_trie do
    try
      for i = 0 to n - 2 do
        if tab.(i) > tab.(i + 1) then
          raise PasTrie
      done;
      est_trie := true
    with
    | PasTrie -> est_trie := false
  done

let print_tab (t: int array): unit =
  let str = "[|" ^ (Array.map string_of_int t |> Array.to_list |> String.concat ";") ^ "|]" in
  print_endline str

let tri () =
  print_tab tab;
  let trieurs = Array.init (n - 1) (Thread.create trieur) in
  let verifieur = Thread.create verifieur () in
  Thread.join verifieur;
  Array.iter Thread.join trieurs;
  print_tab tab

let () = tri()
