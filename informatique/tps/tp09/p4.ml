module Graphics = Graphicspdf

(* les couleurs des jetons                                 *)
type couleur = Rouge | Jaune

(* les dimensions du plateau de jeu                        *)
let nb_colonnes = 7
let nb_lignes   = 6

(* le plateau des jeux, de dimension nb_lignes * nb_colonnes *)
type game = couleur option array array

(* un état du jeu                                          *)
type etat = { game : game ; joueur : couleur }

let autre = function Rouge -> Jaune | Jaune -> Rouge

(* Quelques fonctions graphiques *)
let color_of_couleur = function Rouge -> Graphics.red | _ -> Graphics.yellow

let cx = 80                     (* nb pixel x case jeu *)
let cy = 80                     (* nb pixel y case jeu *)
let sx = cx * nb_colonnes
let sy = cy * nb_lignes

let draw_game (e: etat) =
  for i = 0 to nb_colonnes do
    Graphics.moveto 0 (i * cx);
    Graphics.lineto sx (i * cx);
  done;
  for j = 0 to nb_lignes   do
    Graphics.moveto (j * cy) 0;
    Graphics.lineto (j * cy) sy;
  done;
  for i = 0 to (nb_colonnes-1) do
    for j = 0 to (nb_lignes  -1) do
      match e.game.(i).(j) with
      | None -> ()
      | Some c -> begin
          Graphics.set_color (color_of_couleur c);
          Graphics.fill_circle (i*cx + cx/2) (j*cy + cy/2) ((min cx cy)/2);
          Graphics.set_color Graphics.black end
    done
  done

(* Boite à outils *)
(* d encode une direction : E, NE, N, NO, O, SO, S, SE
                            0   1  2   3, 4,  5, 6,  7
   on calcule la case obtenue en se déplaçant k fois dans la direction d, depuis la case (i, j)
*)
let rec move (k: int) (i, j) (d: int) =
  match d with
  | 0 -> (i+k, j)               (* E *)
  | 1 -> (i+k, j+k)             (* NE *)
  | 2 -> (i  , j+k)             (* N *)
  | 3 -> (i-k, j+k)             (* NO *)
  | _ -> move (-k) (i, j) (d-4)

(*
  let directions = [(0,1);(0,-1);(1,0);(-1,0);(1,1);(1,-1);(-1,1);(-1,-1)] in
  let len = [-3;-2;-1;0;1;2;3] in
*)

(* permet de tester si une case (i, j) est dans le plateau de jeu *)
let is_in (i, j) =
  i >= 0 && j >= 0 && i < nb_colonnes && j < nb_lignes
(* permet de tester si une case (i, j) du plateau g est libre *)
let is_free g (i, j) =
  is_in (i, j) && g.(i).(j) = None
(* permet de tester si une case (i, j) du plateau g contient la couleur c *)
let is_color g (i, j) c =
  is_in (i, j) && g.(i).(j) = Some c

(* retourne une copie de la matrice mm *)
let copy_matrix mm =
  let n = Array.length mm in
  if n = 0 then [||]
  else
    let m = Array.length mm.(0) in
    if m = 0 then Array.init n (fun i -> [||])
    else
      Array.init n (fun i -> Array.copy mm.(i))

(* matrice des valeurs des cases *)
let val_cases =
  [|
    [|3 ; 4 ; 5  ; 7  ; 5  ; 4 ; 3|];
    [|4 ; 6 ; 8  ; 10 ; 8  ; 6 ; 4|];
    [|5 ; 8 ; 11 ; 13 ; 11 ; 8 ; 5|];
    [|5 ; 8 ; 11 ; 13 ; 11 ; 8 ; 5|];
    [|4 ; 6 ; 8  ; 10 ; 8  ; 6 ; 4|];
    [|3 ; 4 ; 5  ; 7  ; 5  ; 4 ; 3|];
  |]

let heuristique (e: etat) (c: couleur): int =
  let { game; joueur } = e in
  let (score, _) = Array.fold_left (fun (sum, i) l ->
    let (score, _) = Array.fold_left (fun (sum, j) v ->
      match game.(i).(j) with
      | Some(c') ->
          let signe = (if c = c' then 1 else -1) in
          (sum + signe * val_cases.(i).(j), j + 1)
      | None     -> (sum, j + 1)
    ) (0, 0) l in
    (sum + score, i + 1)
  ) (0, 0) val_cases in
  score

(* fonctions à compléter, elles sont définies ici pour pouvoir faire typer le reste du programme *)
let place_jeton (e: etat) (i: int): etat option =
  let { game = game'; joueur = player } = e in
  let game = copy_matrix game' in
  let j = ref (nb_lignes - 1) in
  if game.(i).(!j) <> None then None
  else begin
    while game.(i).(!j) = None && !j > 0 do
      decr j;
    done;
    if !j = 0 && game.(i).(!j) = None then game.(i).(0) <- Some player
    else game.(i).(!j + 1) <- Some player;
    Some { game; joueur = autre player }
  end

let next (e: etat): etat list =
  List.init nb_colonnes (fun x -> x)
    |> List.filter_map (place_jeton e)

let gagne (e: etat) (c: couleur): bool =
  let exception Gagne in
  let { game } = e in
  let dx = List.init 4 (fun x -> x) in
  try
    for j = 0 to nb_lignes do
      for i = 0 to nb_colonnes - 4 do
        if List.for_all (fun di -> game.(i + di).(j) = Some(c)) dx then
          raise Gagne
      done;
    done;
    for i = 0 to nb_colonnes do
      for j = 0 to nb_lignes - 4 do
        if List.for_all (fun dj -> game.(i).(j + dj) = Some(c)) dx then
          raise Gagne
      done;
    done;
    for i = 0 to nb_colonnes - 4 do
      for j = 0 to nb_lignes - 4 do
        if List.for_all (fun d -> game.(i + d).(j + d) = Some(c)) dx then
          raise Gagne
      done;
    done;
    false
  with
  | Gagne -> true

let inf = 100000

let minmax (p: int) (e: etat) (c: couleur): int =
  let succ = next e in
  if succ = [] then
    if gagne e c then inf
    else if gagne e (autre c) then -inf
    else 0
  else if p = 0 then heuristique e c

let joue (e: etat) (d: int): etat =
  assert false

(* attend que le joueur clique sur l'interface graphique et retourne l'état du jeu après *)
let rec read_player_move (e: etat) =
  print_string "Colonne ?";
  let i = read_int () in
  print_newline();
  match place_jeton e i with
  | None -> read_player_move e
  | Some(g') -> g'

let main () =
  (* définition du jeu initial *)
  let init = { game = Array.make_matrix nb_colonnes nb_lignes None ; joueur = Rouge } in
  let rec play_with_user (e: etat) =
    Graphics.open_pdf "out.pdf";
    Graphics.open_graph (" " ^ (string_of_int sx) ^ "x" ^ (string_of_int sy));
    Graphics.clear_graph ();
    draw_game e;
    Graphics.close_graph ();
    (* le joueur joue *)
    let e' = read_player_move e in
    Graphics.clear_graph ();
    draw_game e';
    (* l'ordinateur joue *)
(*     let e'' = joue e' 5 in *)
    play_with_user e'
  in
  play_with_user init

