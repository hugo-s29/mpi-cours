type 'c btree =
  | N of 'c * 'c btree * 'c btree (* arbre contenant un noeud *)
  | V                             (* arbre vide *)

type 'a ncontent = {
  hauteur : int;                (* hauteur du noeud courant *)
  elem    : 'a ;                (* élément du noeud courant *)
}

type 'a set = 'a ncontent btree

let ex =
  N({hauteur = 2; elem = 6},
    N({hauteur = 1; elem = 3},
      N({hauteur = 0; elem = 1}, V, V),
      N({hauteur = 0; elem = 4}, V, V)
     ),
    N({hauteur = 0; elem = 8}, V, V)
   )

type 'a cmp = 'a -> 'a -> int

(* module utilisé pour les tests automatiques *)
module Test =
struct
  type 'a tset = 'a list
  let rec mem cp (x: 'a) (s: 'a tset) =
    match s with
    | [] -> false
    | y :: ys ->
      let c = cp x y in
      (c = 0) || (c > 0 && (mem cp x ys))
  let insertion cp (x: 'a) (s: 'a tset) =
    let rec aux (prev: 'a list) (after: 'a list): 'a list = 
      match after with
      | [] -> List.rev_append prev [x]
      | y :: ys ->
        let c = cp x y in
        if c = 0 then s
        else if c > 0 then aux (y :: prev) ys
        else List.rev_append prev (x :: after)
    in
    aux [] s
  let suppression cp (x: 'a) (s: 'a tset) =
    let rec aux (prev: 'a list) (after: 'a list): 'a list = 
      match after with
      | [] -> s
      | y :: ys ->
        let c = cp x y in
        if c = 0 then List.rev_append prev ys
        else if c > 0 then aux (y :: prev) ys
        else s
    in
    aux [] s
  let print pp fmt x =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         pp
      ) x
end

(* Génère un entier uniformément dans [|0, m-1|] *)
let gen_int (m: int) = Random.int m

(* Génère une liste de taille moyenne n, dont les entiers sont choisis
   uniformément dans [|0, m-1|] *)
let gen_list (n: int) (m: int) : int list =
  let rho = 1. /. (float_of_int n) in
  let rec aux () =
    if (Random.float 1. >= rho) then (gen_int m) :: (aux ())
    else []
  in aux ()

let rec fold_int f a b acc =
  if a = b then acc else fold_int f (a+1) b (f acc a)

exception Bad
(* vérifie les hauteurs et la propriété AVL*)
let check_heights (x: 'a set): bool =
  let rec aux (x: 'a set): int =
    match x with
    | V -> -1
    | N({hauteur = h}, g, d) ->
      let hg = aux g in
      let hd = aux d in
      let h_theorique = 1 + (max hg hd) in
      let delta = abs (hg - hd) in
      if h != h_theorique || delta > 1 then raise Bad
      else h_theorique
  in
  try let _ = aux x in true
  with
  | Bad -> false

(* vérifie que l'arbre est un ABR *)
let is_abr cp (x: 'a set): bool =
  let rec aux (cp) (x: 'a set) (binf: 'a option) (bsup: 'a option) : bool =
    match x with
    | V -> true
    | N({elem = elem}, g, d) ->
      ( match bsup with
        | Some(bsup) -> (cp elem bsup <= 0)
        | None -> true)
      &&
      ( match binf with
        | Some(binf) -> (cp binf elem <= 0)
        | None -> true
      )
      && aux cp g (binf) (Some(elem))
      && aux cp d (Some(elem)) (bsup)
  in
  aux cp x None None

(* vérifie que l'arbre est un avl *)
let is_avl (cp) (x: 'a set): bool =
  (is_abr cp x) && (check_heights x)

let gen_param_length = 100
let gen_param_ampl   = 100

let test_insertion () =
  let test_size = 1000 in
  for _ = 0 to test_size do
    let t = gen_list gen_param_length gen_param_ampl in
    let rep_stu =
      List.fold_left (fun acc x ->
        insertion (-) x acc 
      ) V t in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.insertion (-) x acc 
        ) [] t
    in
    if not (is_avl (-) rep_stu) then
      begin
        Format.printf "Échec de l'insertion --pas un AVL-- sur entrée : @. %a@."
          (Test.print Format.pp_print_int) t;
        assert false
      end;

    if ((elements rep_stu) <> rep_cor) then
      begin
        Format.printf "Échec de l'insertion --pas bons éléments-- sur entrée : @. %a@."
          (Test.print Format.pp_print_int) t;
        assert false
      end
  done

let test_suppression () =
  let test_size = 1000 in
  for _ = 0 to test_size do
    let tins = gen_list gen_param_length gen_param_ampl in
    let tsup = gen_list gen_param_length gen_param_ampl in
    let rep_stu =
      List.fold_left (fun acc x ->
          insertion (-) x acc 
        ) V tins in
    let rep_stu =
      List.fold_left (fun acc x ->
          suppression (-) x acc 
        ) rep_stu tsup in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.insertion (-) x acc 
        ) [] tins in
    let rep_cor =
      List.fold_left (fun acc x ->
          Test.suppression (-) x acc 
        ) rep_cor tsup in
    if not (is_avl (-) rep_stu) then
      begin
        Format.printf "Échec de la suppression --pas un AVL-- sur entrée : @. Insertion : %a@.Suppression : %a@."
          (Test.print Format.pp_print_int) tins
          (Test.print Format.pp_print_int) tsup;
        assert false
      end;

    if ((elements rep_stu) <> rep_cor) then
      begin
        Format.printf "Échec de la suppression --pas bons éléments-- sur entrée : @. Insertion : %a@.Suppression : %a@."
          (Test.print Format.pp_print_int) tins
          (Test.print Format.pp_print_int) tsup;
        assert false
      end
  done
