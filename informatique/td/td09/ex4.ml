type permutation = int array

let identite (n: int): permutation =
  let p = Array.make n 0 in
  for i = 0 to n - 1 do
    p.(i) <- i;
  done;
  p

let inverse (p: permutation): permutation =
  let n = Array.length p in
  let p' = identite n in
  for i = 0 to n - 1 do
    let j = p.(i) in
    p'.(j) <- i;
  done;
  p'

let compose (p: permutation) (p': permutation): permutation =
  let n = Array.length p in
  let p'' = identite n in
  for i = 0 to n - 1 do
    p''.(i) <- p.(p'.(i))
  done;
  p''

let rec fact (n: int): int =
  match n with
  | 0 -> 1
  | _ -> n * fact (n-1)

let card_groupe_engendre (x: permutation list): int =
  match x with
  | [] -> 0
  | e::q ->
      let n = Array.length e in
      let m = List.length x in
      let t = Hashtbl.create (fact m) in
      Hashtbl.add t (identite n) true;
      let m' = ref 1 in
      while !m' <> 0 do
        Hashtbl.iter (fun elem _ ->
          List.iter (fun elem' ->
            Hashtbl.add t (compose elem elem') true
          ) x
        ) t;
        Hashtbl.iter (fun elem _ ->
          List.iter (fun elem' ->
            Hashtbl.add t (compose (inverse elem) elem') true
          ) x
        ) t;
        m' := (Hashtbl.length t) - !m'
      done;
      Hashtbl.length t
