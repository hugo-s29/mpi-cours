type sym = R | C
type word = sym list

let va = function R -> 1 | C -> -1

let est_luka (w: word): bool =
  let l = List.map va w in
  let rec aux (l: int list) (s: int): bool =
    match l with
    | []     -> s = -1
    | x :: q -> s >= 0 && aux q (s + x)
  in aux l 0

let decompose (w: word): word * word =
  assert(est_luka w);
  let l = List.map va w in
  let rec aux (l: int list) (s: int) (i: int): int =
    match l with
    | []     -> i
    | x :: q -> if s = 0 then i - 1 else aux q (s + x) (i - 1)
  in
  let k = (aux l 0 ((List.length w) - 1)) - 1 in
  print_int k;
  match w with
  | R::w' ->
    let rec aux' (w: word) (i: int) (u: word) (v: word): word * word =
      match w with
      | []                 -> (List.rev u, List.rev v)
      | x :: q when i <= k -> aux' q (i+1) (x::u) v
      | x :: q             -> aux' q (i+1) u (x::v)
    in aux' w' 0 [] []
  | _ -> failwith "Wrong length"
