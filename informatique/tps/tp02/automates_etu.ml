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

let auto1 = {
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
    (3, 'b', 3);
  ]
}

let auto2 = {
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
