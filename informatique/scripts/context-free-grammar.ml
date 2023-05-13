#require "str";;

type production_rule = char * string

(* 𝒢 = (𝒱, 𝛴, 𝑃, 𝑆) *)
type grammar = char list * char list * production_rule list * char

let toString x = List.to_seq x |> String.of_seq

let print_out (out : char list list): unit =
  let words = List.map toString out in
  List.fold_left (fun acc x -> x ^ ";" ^ acc) "" words
  |> print_endline

let apply_rule (word: string) (rule: char * string) : string list =
  let (c, s) = rule in
  let pattern = Str.regexp (Str.quote (String.make 1 c)) in
  try
    let result = Str.replace_first pattern s word in
    if result = word then []
    else [result]
  with
  | Not_found -> []

let remove_elt e l =
  let rec aux l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> aux xs acc
    | x::xs -> aux xs (x::acc)
  in aux l []

let remove_duplicates l =
  let rec aux l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> aux (remove_elt x xs) (x::acc)
  in aux l []

let id x = x

let derive (g: grammar) (depth: int): string list =
  let (nterm, term, rules, init) = g in
  let generate_from (w: string): string list =
    let z = List.map (apply_rule w) rules
    |> List.flatten
    |> remove_duplicates
    in
    let () = String.concat ";" z |> print_endline in
    z
  in
  List.init depth id
  |> List.fold_left (fun acc i -> List.map generate_from acc |> List.flatten)
  [String.make 1 init]
  |> List.filter (String.for_all (fun c -> List.mem c term))

let rec generate_strings length max_length =
  if length > max_length then Seq.empty
  else Seq.append
         (Seq.return "")
         (Seq.append
            (Seq.map (fun s -> "a" ^ s) (generate_strings (length+1) max_length))
            (Seq.map (fun s -> "b" ^ s) (generate_strings (length+1) max_length)))

let count (str: string) (c: char) =
  String.fold_left (fun sum x -> if x = c then sum + 1 else sum) 0 str

let seq = generate_strings 0 6
  |> Seq.filter (fun s -> count s 'a' >= count s 'b')

let g = (['X'], ['a'; 'b'],
  [('S', "aSb"); ('S', "bSa"); ('S', "aS"); ('S', "SS"); ('S', "")], 'S')

let derived = derive g 4
  |> List.filter (fun x -> String.length x <= 6)
  |> List.sort_uniq compare

let not_found = Seq.filter (fun x -> not (List.mem x derived)) seq |> List.of_seq



type symbol =
  | Terminal of char
  | Nonterminal of char

type rule = char * symbol list

type grammar = {
  nonterminals: char list;
  terminals: char list;
  rules: rule list;
  start: char;
}

let rec derive (word: symbol list) ((s, res): rule): symbol list list =
  match word with
  | [] -> [[]]
  | Nonterminal c :: q when c = s ->
      derive q (s,res)
      |> List.map (fun x -> [ Nonterminal c :: x; res @ x ])
      |> List.flatten
      |> List.sort_uniq compare
  | c :: q ->
      derive q (s,res)
      |> List.map (fun x -> c :: x)
      |> List.sort_uniq compare

let symbols_of_string (s: string): symbol list =
  String.to_seq s
  |> List.of_seq
  |> List.map (fun c ->
      if c = Char.lowercase_ascii c then Terminal c
      else Nonterminal c)

let rec string_of_symbols = function
  | [] -> ""
  | Terminal c :: q -> (String.make 1 c) ^ (string_of_symbols q)
  | Nonterminal c :: q -> (String.make 1 c) ^ (string_of_symbols q)

let print_list (f: 'a -> unit) (l: 'a list): 'a list =
  let n = List.length l in
  List.iteri (fun i x ->
    f x;
    if i < n - 1 then print_char ';') l;
  l


let derive_all (grammar: grammar) (depth: int): string list =
  List.init depth id
  |> List.fold_left (fun acc _ ->
      List.map (fun word -> List.map (derive word) grammar.rules) acc
      |> List.flatten
      |> List.flatten
      |> List.sort_uniq compare)
    [[Nonterminal grammar.start]]
  |> List.filter (List.for_all (function
    | Terminal c -> true
    | Nonterminal c -> false))
  |> List.map string_of_symbols

let my_grammar : grammar = {
  nonterminals = ['S'; 'X'];
  terminals = ['a'; 'b'];
  rules = [
    ('S', [Terminal 'b'; Nonterminal 'X']);
    ('S', [Nonterminal 'X'; Terminal 'b']);
    ('S', [Nonterminal 'X'; Terminal 'b'; Nonterminal 'X']);
    ('X', [Terminal 'a'; Nonterminal 'X'; Terminal 'b']);
    ('X', [Terminal 'b'; Nonterminal 'X'; Terminal 'a']);
    ('X', [Terminal 'b'; Nonterminal 'X']);
    ('X', [Nonterminal 'X'; Nonterminal 'X']);
    ('X', [])
  ];
  start = 'S';
}

let derived = derive_all my_grammar 6
  |> List.filter (fun x -> String.length x <= 6)
  |> List.sort_uniq compare

let seq = generate_strings 0 5
  |> Seq.filter (fun s -> count s 'a' < count s 'b')

let not_found = Seq.filter (fun x -> not (List.mem x derived)) seq |> List.of_seq
