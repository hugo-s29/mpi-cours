type formule =
  | Var   of string
  | And   of formule * formule
  | Or    of formule * formule
  | Imply of formule * formule
  | Equiv of formule * formule
  | Top
  | Bot
  | Not   of formule

let prec (c: char) : int =
  match c with
  | 'a' -> 5
  | '&' -> 4
  | '|' -> 3
  | '>' -> 2
  | _   -> 1

let find_op_out_parent (s: string) (i: int) (j: int) =
  let rec aux (i: int) (p: int) =
    if i > j then []
    else
      match s.[i], p with
      | '&', 0 -> ('&', i) :: (aux (i+1) p)
      | '|', 0 -> ('|', i) :: (aux (i+1) p)
      | '>', 0 -> ('>', i) :: (aux (i+1) p)
      | '=', 0 -> ('=', i) :: (aux (i+1) p)
      | '(', _ -> aux (i+1) (p+1)
      | ')', _ -> aux (i+1) (p-1)
      | _, _ -> aux (i+1) p
  in
  List.fold_left (fun (op, i) (op', i') -> if prec op < prec op' then (op, i) else (op', i')) ('a', -1) (aux i 0)

let rec all_var_char (s: string) (i: int) (j: int) =
  (i > j) ||
  ((int_of_char 'a' <= int_of_char (s.[i]) && int_of_char (s.[i]) <= int_of_char 'z')
   && all_var_char s (i+1) j)

let rec reader (s: string) (i: int) (j: int) =
  if s.[i] = ' ' then
    reader s (i+1) j
  else if s.[j] = ' ' then
    reader s i (j-1)
  else
    let (op, k) = find_op_out_parent s i j in
    if k > 0 then
      match op with
      | '&' -> And(reader s i (k-1), reader s (k+1) (j))
      | '|' -> Or(reader s i (k-1), reader s (k+1) (j))
      | '>' -> Imply(reader s i (k-1), reader s (k+1) (j))
      | _   -> Equiv(reader s i (k-1), reader s (k+1) (j))
    else
      match s.[i], j-i with
      | '~', _ -> Not(reader s (i+1) j)
      | 't', 0 -> Top
      | 'b', 0 -> Bot
      | '(', _ -> reader s (i+1) (j-1)
      | _ -> (assert (all_var_char s i j) ; Var (String.sub s i (j-i+1)))

let parse (s: string) =
  reader s 0 ((String.length s) - 1)

type env_prop = (string * bool) list

let print_list start vide sep en printer l =
  match l with
  | [] -> Printf.printf "%s" vide
  | x :: l' ->
    Printf.printf "%s" start;
    printer x;
    List.iter (fun y -> Printf.printf "%s " sep ; printer y ; ) l';
    Printf.printf "%s" en

let print_env_prop = print_list "(" "()" ", " ")" (fun (x, y) -> Printf.printf "%s ~> %b" x y)

module Set = struct
  (* Représentation d'un ensemble par une liste triée sans doublons *)
  type 'a t = 'a list

  (* Ensemble vide *)
  let empty = []

  (* Test d'appartenance *)
  let rec mem (x: 'a) (s: 'a t): bool =
    match s with
    | y :: s' -> (x = y) || (x > y) && (mem x s')
    | [] -> false

  (* Ajout d'un élément *)
  let add (x: 'a) (s: 'a t): 'a t =
    let rec aux (s: 'a t) (prev: 'a t): 'a t =
    match s with
    | [] -> List.rev_append prev [x]
    | y::s' ->
      if x = y then List.rev_append prev s
      else if x > y then aux s' (y::prev)
      else List.rev_append (prev) (x::y::s')
    in aux s []

  (* Suppression d'un élément *)
  let remove (x: 'a) (s: 'a t): 'a t =
    let rec aux (s: 'a t) (prev: 'a t): 'a t =
      match s with
      | [] -> List.rev_append prev []
      | y::s' ->
        if x = y then List.rev_append prev s'
        else if x > y then aux s' (y::prev)
        else List.rev_append prev s
    in aux s []

  (* Union de deux ensembles *)
  let union (s1: 'a t) (s2: 'a t) =
    let rec aux (s1: 'a t) (s2: 'a t) (res: 'a t): 'a t =
      match s1, s2 with
      | [], s | s, [] -> List.rev_append s res
      | x1:: s1', x2:: s2' ->
        if x1 < x2 then aux s1' s2 (x1 :: res)
        else if x2 < x1 then aux s1 s2' (x2 :: res)
        else aux s1' s2' (x1 :: res)
    in aux s1 s2 []

  (* Intersection de deux ensembles *)
  let intersection (s1: 'a t) (s2: 'a t) =
    let rec aux (s1: 'a t) (s2: 'a t) (res: 'a t): 'a t =
      match s1, s2 with
      | [], s | s, [] -> List.rev_append s res
      | x1:: s1', x2:: s2' ->
        if x1 < x2 then aux s1' s2 (res)
        else if x2 < x1 then aux s1 s2' (res)
        else aux s1' s2' (x1 :: res)
    in aux s1 s2 []

  let of_list (l: 'a list) =
    List.fold_left (fun acc x -> add x acc) [] l
end

type 'a set = 'a Set.t

type litteral = {
  var : string;
  pn  : bool                    (* true pour un littéral positif *)
}

let string_of_litteral (l: litteral): string =
  if l.pn then l.var
  else "¬" ^ l.var

type fnc = litteral set set
type fnd = litteral set set

let print_list vide sep printer l =
  match l with
  | [] -> Printf.printf "%s" vide
  | x :: l' ->
    printer x;
    List.iter (fun y -> Printf.printf " %s " sep ; printer y ; ) l'

let pp_print_fn b x =
  let s1, s2, s3, s4 = if b then "∧", "∨", "⊤", "⊥" else "∨", "∧", "⊥", "⊤" in
  print_list s3 s1 (
    fun x -> (Printf.printf "(" ; print_list s4 s2 (fun l -> Printf.printf "%s" (string_of_litteral l)) x; Printf.printf ")" ; )
  ) x

let print_fnc (x: fnc) =
  pp_print_fn false x
let print_fnd (x: fnd) =
  pp_print_fn true x

let fnc_true  : fnc = Set.empty
let fnc_false : fnc = Set.add Set.empty Set.empty
let fnd_true  : fnd = Set.add Set.empty Set.empty
let fnd_false : fnd = Set.empty

let fnc_mk_lit (l: litteral): fnc =
  match l.pn with
  | true -> fnc_true
  | false -> fnc_false

let fc_clause (ls: litteral set): bool =
  List.exists (fun l ->
    List.exists (fun l' -> l.pn = not l'.pn && l.var = l'.var) ls
  ) ls

let wf_fnc (f: fnc): fnc =
  List.filter (fun c -> not (wc_clause c)) f

(* autre solution *)
let rec wc_clause (ls: litteral set): bool =
  match ls with
  | [] -> false
  | l::ls' -> if List.mem {var = l.var; pn = not l.pn } ls' then true
  else wf_clause ls'

let assume (l: litteral) (f: fnc): fnc =
  let lv = l and lf = { var = l.var; pn = not l.pn } in
  let rec aux (f: fnc) : fnc =
    match f with
    | [] -> []
    | c::f' -> if Set.mem lv c then aux f'
               else if Set.mem lf c then (Set.remove lf c)::(aux f')
               else c::(aux f')
  in aux f

let is_valid (f: fnc): bool = f = fnc_true
let is_invalid (f: fnc): bool = f = fnc_false

let rec vars (f: fnc): string set =
  match f with
  | []    -> Set.empty
  | c::f' ->
      c
      |> List.map (fun l -> l.var)
      |> Set.of_list
      |> Set.union (vars f')

