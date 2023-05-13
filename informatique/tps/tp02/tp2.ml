module Set = struct
  (* Représentation d'un ensemble par une liste sans doublons *)
  type 'a t = 'a list

  (* Ensemble vide *)
  let empty = []

  (* Test d'appartenance *)
  let rec mem (x: 'a) (s: 'a t): bool =
    List.mem x s

  (* Ajout d'un élément *)
  let add (x: 'a) (s: 'a t): 'a t =
    if mem x s then s else x :: s

  (* Suppression d'un élément *)
  let remove (x: 'a) (s: 'a t): 'a t =
    List.filter (fun y -> y <> x) s

  (* Union de deux ensembles *)
  let union (s1: 'a t) (s2: 'a t) =
    List.fold_left (fun acc x -> add x acc) s1 s2

  (* Intersection de deux ensembles *)
  let intersection (s1: 'a t) (s2: 'a t) =
    List.filter (fun x -> mem x s2) s1
end

type ('a, 'b) automaton =
  {
    letters    : 'b list;
    states     : 'a list;
    transitions: ('a * 'b * 'a) list;
    ini_states : 'a list;
    fin_states : 'a list;
  }

let print_list vide sep printer l =
  match l with
  | [] -> Printf.printf "%s" vide
  | x :: l' ->
    printer x;
    List.iter (fun y -> Printf.printf "%s" sep ; printer y ; ) l'

let pp_set_inline pp x =
  print_list "{}" ", " pp x

let print_automaton (state_to_string : 'a -> string) (letter_to_string: 'b -> string) (auto : ('a, 'b) automaton) : unit =
  Printf.printf "{\n";
  Printf.printf "  letters : ";
  pp_set_inline (fun x -> Printf.printf "%s"  (letter_to_string x)) auto.letters;
  Printf.printf "\n";
  Printf.printf "  states : ";
  pp_set_inline (fun x -> Printf.printf "%s"  (state_to_string x)) auto.states;
  Printf.printf "\n";
  Printf.printf "  init_states : ";
  pp_set_inline (fun x -> Printf.printf "%s"  (state_to_string x)) auto.ini_states;
  Printf.printf "\n";
  Printf.printf "  fin_states : ";
  pp_set_inline (fun x -> Printf.printf "%s"  (state_to_string x)) auto.fin_states;
  Printf.printf "\n";
  Printf.printf "  transitions : ";
  pp_set_inline (fun (q, l, q') -> Printf.printf "(%s, %s, %s)" (state_to_string q) (letter_to_string l) (state_to_string q')) auto.transitions;
  Printf.printf "\n";
  Printf.printf "}"

let auto1: (int, char) automaton = {
  letters = ['a'; 'b'; 'c'];
  states = [0; 1; 2; 3];
  ini_states = [0];
  fin_states = [2; 3];
  transitions = [
    (0, 'a', 0);
    (0, 'b', 0);
    (0, 'a', 1);
    (0, 'c', 3);
    (1, 'a', 2);
    (1, 'b', 2);
    (1, 'c', 3);
    (2, 'c', 3);
    (3, 'a', 3);
    (3, 'b', 3);
  ]
}

let auto2: (int, char) automaton  = {
  letters = ['a'; 'b'; 'c'; 'd'; 'e'];
  states = [0; 1; 2; 3];
  ini_states = [0];
  fin_states = [2; 3];
  transitions = [
    (0, 'a', 1);
    (1, 'a', 2);
    (2, 'a', 3);
    (3, 'a', 0);
    (0, 'b', 0);
    (0, 'c', 0);
    (1, 'd', 1);
    (2, 'b', 2);
    (2, 'c', 2);
    (3, 'e', 3);
  ]
}

let delta_from_state (auto: ('a, 'b) automaton) (q: 'a) (l: 'b): 'a list = 
  List.filter (fun (u, l', v) -> u = q && l = l') auto.transitions
  |> List.map (fun (_, _, v) -> v)

let rec delta_from_set (auto: ('a, 'b) automaton) (s: 'a list) (a: 'b): 'a list =
  match s with
  | [] -> []
  | p :: q -> Set.union (delta_from_state auto p a) (delta_from_set auto q a)

let rec delta_etendue (auto: ('a, 'b) automaton) (s: 'a list) (w: 'b list): 'a list =
  match w with
  | [] -> s
  | x :: w' -> delta_etendue auto (delta_from_set auto s x) w'

;;

let accepte (auto: ('a, 'b) automaton) (w: 'b list): bool =
  List.length
    (Set.intersection (delta_etendue auto auto.ini_states w) auto.fin_states)
  > 0

let print_bool (b: bool): unit =
  if b then print_string "true"
  else print_string "false"

let str_seq (s: string): char list =
  s
  |> String.to_seq
  |> List.of_seq

(*let rec accepte_depuis (auto: ('a, 'b) automaton) (q: 'a) (w: 'b list) : bool =
  match w with
  | [] -> List.mem q auto.fin_states
  | x :: w' ->
      delta_from_state auto q x
      |> List.map (fun q' -> accepte_depuis auto q' w')
      |> List.mem true*)

let rec accepte_depuis (auto: ('a, 'b) automaton) (q: 'a) (w: 'b list) : bool =
  match w with
  | [] -> List.mem q auto.fin_states
  | x :: w' ->
      delta_from_state auto q x
      |> List.map (fun q' -> accepte_depuis auto q' w')
      |> List.exists ((=) true)

let accepte_profondeur (auto: ('a, 'b) automaton) (w: 'b list) : bool =
  auto.ini_states
  |> List.map (fun q -> accepte_depuis auto q w)
  |> List.exists ((=) true)

(*
let complete (auto: ('a, 'b) automaton): ('a option, 'b) automaton =
  let rename = List.map (fun x -> Some x) in
  let aux (l: 'b) (q: 'a): ('a option * 'b * 'a option) option =
    if List.for_all (fun q' -> List.mem (q, l, q') auto.transitions) auto.states
    then Some (Some q, l, None)
    else None
  in
  {
    letters = auto.letters;
    states = rename auto.states;
    ini_states = rename auto.ini_states;
    fin_states = rename auto.fin_states;
    transitions =
      List.map aux auto.letters
      |> List.map (fun f -> f auto.ini_states)
      |> List.flatten
      |> List.filter ((<>) None)
      |> List.map get
      |> Set.union
        (List.map (fun (x,y,z) -> (Some x, y, Some z)) auto.transitions)
  }


let complete (auto: ('a, 'b) automaton): ('a option, 'b) automaton =
  let rename = List.map (fun x -> Some x) in
  in
  {
    letters = auto.letters;
    states = rename auto.states;
    ini_states = rename auto.ini_states;
    fin_states = rename auto.fin_states;
    transitions =
      Set.union (List.map (fun (x,y,z) -> (Some x, y, Some z)) auto.transitions)
      (List.map
        (fun l ->
          List.map
          (fun q ->
            pass
          )
        )
      )
  }
*)

;;


(*
str_seq "aabaa"
|> accepte_recherche auto1
|> print_bool*)




