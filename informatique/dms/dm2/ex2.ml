let f_cmp (i: int) (x: 'a array) (y: 'a array): bool = 
  x.(i) <= y.(i)

let swap (t: 'a array) (x: int) (y: int): unit =
  let temp = t.(x) in
  t.(x) <- t.(y); t.(y) <- temp

let partition (t: 'a array) (g: int) (d: int) (p: int) (cmp: 'a -> 'a -> bool): int =
  swap t g d;
  let j = ref g and i = ref g in
  while !i < d do
    if cmp t.(!i) t.(d) then i := !i + 1
    else
      swap t !i !j;
      i := !i + 1;
      j := !j + 1
  done;
  swap t !j d;
  !j

let rec fast_selection (t: 'a array) (d: int) (f: int) (r: int) (cmp: 'a -> 'a -> bool): 'a =
  if d < f then
    let p = Random.int (f-d+1) + d in
    let q = partition t d f p cmp in
    if r = q then t.(r)
    else if r < q then fast_selection t d (q-1) r cmp
    else fast_selection t (q+1) f (r-(q+1)) cmp
  else t.(d)


type vector = float array
type kd_tree =
  | Empty
  | Node of int * vector * kd_tree * kd_tree

let create_kd_tree (t: vector array) (k: int) : kd_tree =
  let rec aux (t: vector array) (i: int) =
    let n = Array.length t in
    if n = 0 then Empty
    else
      let v = fast_selection t 0 (n-1) (n / 2) (f_cmp i) in
      let i' = (i + 1) mod k in
      let vleft = Array.init (n/2) (fun m -> t.(m)) in
      let vright = Array.init (n/2) (fun m -> t.(m + n/2)) in
      let leftbranch = aux vleft i' in
      let rightbranch = aux vright i' in
      Node(i, v, leftbranch, rightbranch)
  in aux t 0

let create_dataset (n: int) : vector array =
  Array.init n (fun i -> Array.init 2 (fun _ -> Random.float 1.))

let nearest_neighbors (n: int) (tree: kd_tree) (v: vector): vector array =
  let rec aux (tree: kd_tree) (neighbors: vector list) (v: vector): vector list =
    match tree with
    | Empty -> []
    | Node(i, u, left, right) ->
        let neighbors =
          if f_cmp i v u then aux left neighbors v
          else aux right neighbors v in
        if List.length neighbors < n then u::neighbors
        else neighbors
  in aux tree [] v |> Array.of_list
