type sad = {
  n : int; (* nb objets *)
  wi : int array; (* poids *)
  vi : int array; (* valeurs *)
  p : int (* poids max *)
}

let ex = {
  n = 6;
  wi = [| 6; 8; 10; 14; 2; 5 |];
  vi = [| 13; 16; 19; 24; 3; 5 |];
  p = 20
}

type masque = bool option array
type solution = bool array

(* a--b représente sous forme de liste l'ensemble d'entiers [a,b] *)
let (--) (a: int) (b: int): int list =
  if a <= b then List.init (b - a + 1) (fun i -> a + i)
  else List.init (a - b + 1) (fun i -> a - i)

let affiche_array_param (pp: 'a -> unit) (m: 'a array) =
  let n = Array.length m in
  Printf.printf "[|";
  for i = 0 to n - 2 do
    pp m.(i);
    Printf.printf "; "
  done;
  if n > 0 then pp m.(n-1);
  Printf.printf "|]"

let affiche_masque (m: masque) =
  let print_masque_atom = function
    | None -> Printf.printf " "
    | Some(true)  -> Printf.printf "Y"
    | Some(false) -> Printf.printf "N"
  in affiche_array_param print_masque_atom m

let affiche_solution (m: solution) =
  let print_solution_atom = function
    | true -> Printf.printf "1"
    | false -> Printf.printf "0"
  in affiche_array_param print_solution_atom m

let est_sad_valide (s: sad): bool =
  let indices = 0--(s.n-2) in
  let (vi, wi) = (s.vi, s.wi) in
  List.for_all (fun i ->
    (* teste si vi.(i) / wi.(i) >= vi.(i+1) / wi.(i+1) *)
    vi.(i) * wi.(i+1) >= vi.(i+1) * wi.(i)
  ) indices

let valeur_sol (s: sad) (sol: solution): int =
  Array.mapi (fun i b -> if b then s.vi.(i) else 0) sol
    |> Array.fold_left (+) 0

let poids_sol (s: sad) (sol: solution): int =
  Array.mapi (fun i b -> if b then s.wi.(i) else 0) sol
    |> Array.fold_left (+) 0

let est_masque_valide (s: sad) (m: masque): bool =
  (* somme des poids du masque <= s.p *)
  let solution_partielle = Array.map (function
    | Some(b) -> b
    | None -> false
   ) m in
  (poids_sol s solution_partielle) <= s.p

(* attention : `s` doit être valide pour le masque `m` *)
let next (m: masque) (s: solution): unit =
  let n = Array.length m in
  let indices = (n-1)--0 in
  (* addition + 1 en binaire *)
  (* tant que le i-ème élément est `true` *)
  (* on le passe à `false` *) 
  (* on ajoute la contrainte du masque *)
  List.fold_left (fun acc i ->
    match m.(i) with
    | _ when acc -> true
    | None -> if s.(i) then (s.(i) <- false; false)
                       else (s.(i) <- true ; true )
    | Some(b) ->            (s.(i) <- b    ; false)
  ) false indices; ()

let brute_force (s: sad) (m: masque): (solution * int) option =
  (* comptage des solutions possibles *)
  let num_sols = Array.fold_left (fun acc -> function
    | None    -> 2 * acc 
    | Some(b) -> acc
  ) 1 m in
  let n = Array.length m in
  let sol = Array.init n (fun i -> match m.(i) with
  | Some(b) -> b
  | None    -> false) in
  (* itération des solutions possibles *)
  1--num_sols |> List.fold_left (fun acc _ ->
      match acc with
      | None -> Some(Array.copy sol, valeur_sol s sol)
      | Some(_,v) -> begin
          next m sol;
          if poids_sol s sol <= s.p && valeur_sol s sol >= v then
            Some(Array.copy sol, valeur_sol s sol)
          else acc
        end
  ) None


let prog_dyn_tab (s: sad): int array array =
  let n = s.n and wi = s.wi and vi = s.vi and p = s.p in
  let arr = Array.make_matrix n (p+1) 0 in
  for i = n - 2 downto 0 do
    for j = 1 to p do
      if wi.(i) <= j then
        arr.(i).(j) <- max
          ( arr.(i+1).(j) )
          ( vi.(i) + arr.(i+1).(j-wi.(i)) )
      else
        arr.(i).(j) <- arr.(i+1).(j)
    done;
  done;
  arr
  
let prog_dyn (s: sad): solution * int =
  let arr = prog_dyn_tab s in
  let sol = Array.make s.n false in
  let w = ref s.p in
  for i = 0 to s.n - 2 do
    if arr.(i).(!w) <> arr.(i + 1).(!w) then begin
      w := !w - s.wi.(i);
      sol.(i) <- true
    end
  done;
  (sol, valeur_sol s sol)

let glouton_n (s: sad) (m: masque): (solution * int) option =
  let sol = Array.init s.n (fun i -> match m.(i) with
    | Some(b) -> b
    | None -> false
  ) in let w = 0--(s.n-1)
  |> List.fold_left (fun acc i ->
      if m.(i) = None && acc + s.wi.(i) <= s.p then
        (sol.(i) <- true; acc + s.wi.(i))
      else acc
  ) (poids_sol s sol) in
  if w = 0 then None
  else Some(sol, valeur_sol s sol)


let rec pgcd (a: int) (b: int): int =
  if b = 0 then a
  else pgcd b (a mod b)

let sign_int (a: int): int =
  if a < 0 then -1
  else if a > 0 then 1
  else 0

module Q = struct
  type t = int * int
  (* (a, b) encode a / b *)
  (* invariant  PGCD(a, b) = 1 et b > 0 et, si a = 0, alors b = 1 *)
  let print ((a,b) : t) = Printf.printf "%d/%d" a b
  let dirty_print ((a,b) : t) = Printf.printf "%.2f" ( (float_of_int a) /. (float_of_int b) )

  (* initialisation d'un rationnel *)
  (* a/b ne vérifie pas encore l'invariant fixé *)
  let init (a: int) (b: int): t =
    let div = pgcd (abs a) (abs b) in
    (* on vérifie maintenant PGCD(a,b) = 1 *)
    let (a,b) = (a/div, b/div) in
    if a = 0 then (0, 1)
    else (sign_int(a*b)*(abs a), abs b)
    (* on vérifie maintenant b > 0 *)

  let add ((a,b): t) ((c,d): t): t = init (a*d + c*b) (b*d)
  let opp ((a,b): t): t = (-a, b)
  let sub (x: t) (y: t):t  = add x (opp y)
  let mul ((a,b): t) ((c,d): t): t = init (a*c) (b*d)
  let invs ((a,b): t): t = init b a
  (* on néglige le cas b = 0, car il ne devrait pas arriver *)
  let div (x: t) (y: t): t = mul x (invs y)
  let sign ((a,b): t) = sign_int a
  let of_int (x: int): t = (x, 1)

  let lt ((a,b): t) ((c,d): t): bool = a*d <  b*c
  let le ((a,b): t) ((c,d): t): bool = a*d <= b*c
  let gt ((a,b): t) ((c,d): t): bool = a*d >  b*c
  let ge ((a,b): t) ((c,d): t): bool = a*d >= b*c
  let is_int ((a,b): t): bool = b = 1

  let one = of_int 1
  let zero = of_int 0
end

let (+:)  = Q.add
let (-:)  = Q.sub
let (~-:) = Q.opp
let ( *:) = Q.mul
let (/:)  = Q.div
let (<:)  = Q.lt
let (<=:) = Q.le
let (>:)  = Q.gt
let (>=:) = Q.ge

type qsolution = Q.t array

let affiche_qsolution (m: qsolution) =
  affiche_array_param Q.print m

let valeur_qsol (s: sad) (sol: qsolution): Q.t =
  Array.mapi (fun i b -> sol.(i) *: (Q.of_int s.vi.(i))) sol
    |> Array.fold_left (+:) Q.zero

let poids_qsol (s: sad) (sol: qsolution): Q.t =
  Array.mapi (fun i b -> sol.(i) *: (Q.of_int s.wi.(i))) sol
    |> Array.fold_left (+:) Q.zero

exception ArretAjout

(* le type `option` était probablement en trop... *)
(* le glouton revoit toujours une valeur          *)
(* même si tous les objets sont trop lourd,       *)
(* on peut n'en prendre qu'une partie.            *)
let glouton_r (s: sad) (m: masque): (qsolution * Q.t) =
  let sol = Array.map (function
    | Some(true) -> Q.one
    | _          -> Q.zero
  ) m in
  let sol' = Array.map (function
    | Some(true) -> true
    | _          -> false
  ) m in
  let sum = ref (poids_sol s sol') in
  (* indices modifiables, d'après le masque *)
  let indices = 0--(s.n-1)
    |> List.filter (fun i -> m.(i) = None)
  in try
    List.iter (fun i ->
      if !sum + s.wi.(i) < s.p then begin
        sum := !sum + s.wi.(i);
        sol.(i) <- Q.one;
      end else begin
        sol.(i) <- Q.init (s.p - !sum) s.wi.(i);
        raise ArretAjout
      end
    ) indices;
    (sol, valeur_qsol s sol)
  with
  | ArretAjout -> 
    (sol, valeur_qsol s sol)

let impose_i (m: masque) (i: int) (b: bool): masque =
  Array.mapi (fun j v ->
    if i = j then Some b
    else v
  ) m

exception Found of int
let find_it_frac (sol: qsolution): int =
  try
    Array.iteri (fun i q ->
      if not(Q.is_int q) then
        raise (Found i)
    ) sol;
    -1
  with
  | Found(i) -> i

let branch_and_bound (s: sad): solution * int =
  let best_val = ref 0 in
  let best_sol = ref (Array.make s.n false) in
  let rec process (todo: masque list):unit =
    match todo with
    | []   -> ()
    | m::rest ->
        if est_masque_valide s m then
          let (s_r, v_r) = glouton_r s m in
          if v_r >: Q.of_int(!best_val) then begin
            match glouton_n s m with
            | None           -> ()
            | Some(s_n, v_n) -> begin
              if v_n > !best_val then begin
                best_val := v_n;
                best_sol := s_n
              end;
              if v_r >=: Q.of_int(v_n + 1) then begin
                let i = find_it_frac s_r in
                process ((impose_i m i false)::(impose_i m i true)::rest)
              end else process rest
            end
          end else process rest
        else process rest
  in process [Array.make s.n None];
  (!best_sol, !best_val)
