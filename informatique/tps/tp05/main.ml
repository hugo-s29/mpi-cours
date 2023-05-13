module Vector = struct
  type t = float array
  let null (n: int) = Array.make n 0.
  let ( + ) (x: t) (y: t) =
    Array.map2 ( +. ) x y
  let ( * ) (lam: float) (x: t) =
    Array.map (fun y -> lam *. y) x
  let ( - ) (x: t) (y: t) =
    Array.map2 ( -. ) x y
  let sq_norm (x: t) =
    Array.fold_left (fun acc x -> acc +. x *. x) 0. x
  let norm (x: t) = (sq_norm x) |> sqrt
  let dist x y = norm (x - y)
end
type data = Vector.t array

let pi = 4. *. atan 1.

let expo (lam: float) : float =
  (* 1 - exp(-lam x) *)
  let y = Random.float 1. in
  (log (1. -. y)) /. (-. lam)

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let gen_data (nb_elem: int) (nb_cluster: int) (dist: float) : data =
  let cluster_centers_dist = Array.init nb_cluster (fun _ -> (Array.init 2 (fun _ -> 0.1 +. Random.float 0.8), (Random.float dist))) in
  List.init nb_elem (fun _ ->
      let i = Random.int nb_cluster in
      let c, r = cluster_centers_dist.(i) in
      let theta = 2. *. pi *. (Random.float 1.) in
      let dist = expo (1./.dist) in
      let ecart = [|dist *. cos (theta); dist *. sin (theta)|] in
      [Vector.( c + ecart )]
    )
  |> List.flatten
  |> Array.of_list
  |> knuth_shuffle

let rec argmin (t: 'a list): int =
  match t with
  | [] -> 0
  | x :: q ->
      let i = argmin q in
      if List.nth t i <= x then i + 1
      else 0

let find_closest_center (ce: data) (v: Vector.t): int = 
  let k = Array.length ce in
  let dist (i: int) (v: Vector.t) = Vector.dist v ce.(i) in
  let distances = List.map dist (List.init k (fun x -> x)) in
  argmin distances

let modify_clustering (ce: data) (d: data) (cl: int array) : bool =
  let changed = ref false in
  for i = 0 to Array.length d do
    let x = find_closest_center ce d.(i) in
    if x <> cl.(i) then
      changed := true;
      cl.(i) <- x
  done;
  !changed

let recompute_centers (ce: data) (d: data) (cl: int array) : unit =
  let n = Array.length d in
  let p = Array.length d.(0) in
  for k = 0 to Array.length ce do
    let part = List.init n (fun x -> x)
      |> List.filter (fun i -> cl.(i) = k)
      |> List.map (fun i -> d.(i)) in
    let sum = List.fold_left Vector.(+) (Vector.null p) part in
    let m = float_of_int (List.length part) in
    ce.(k) <- Vector.( * ) (1./.m) sum
  done

let k_means_update (ce: data) (d: data) (cl: int array): bool =
  let changed = modify_clustering ce d cl in
  recompute_centers ce d cl;
  not changed

 let knuth_shuffle (a: 'a array): 'a array =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let random_ints (k: int) (n: int): int array =
  Array.init n (fun i -> i)
    |> knuth_shuffle
    |> Array.to_list
    |> List.filteri (fun i x -> i < k)
    |> Array.of_list

let k_means (k: int) (d: data): int array =
  let ce = random_ints k (Array.length d)
    |> Array.map (fun i -> d.(i)) in
  let cl = Array.init (Array.length d) (fun i -> 0) in
  let stable = ref false in
  while not !stable do
    stable := k_means_update ce d cl;
  done;
  cl


