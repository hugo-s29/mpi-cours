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

let rec fill_mat (auto: (int, char) automaton) (m: char list list array array) (n: int) =
  match n with
  | 0 -> for i = 0 to (List.length auto.states) do
      if List.mem i auto.ini_states then
        m.(0).(i) <- [[]]
      else
        m.(0).(i) <- []
    done;
  | _ ->

