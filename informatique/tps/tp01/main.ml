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

(*let print_env_prop (e: env_prop): unit =
  Format.printf "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (s, b) -> Format.fprintf fmt "%s ↦ %s" s (if b then "V" else "F"))
    ) e*)
let print_env_prop (e: env_prop): unit =
  let strings = List.map
    (fun (p, b) -> p ^ "%*$\mapsto$*)" ^ (if b then "V" else "F"))
    e in
  let concatenated = List.fold_left (fun s p -> s ^ "," ^ p) "" strings in
  print_string("{" ^ concatenated ^ "}")


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

let pp_print_fn b fmt (x) =
  let s1, s2 = if b then "∧", "∨" else "∨", "∧" in
  Format.fprintf fmt  "@[<v%a@]" (
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s@," s1)
      (fun fmt l2 -> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " %s " s2)
          (fun fmt x -> Format.fprintf fmt "%s" (string_of_litteral x)) fmt l2
      )
  ) x

let print_fnc (x: fnc) =
  Format.printf "%a@." (pp_print_fn true) x
let print_fnd (x: fnc) =
  Format.printf "%a@." (pp_print_fn false) x

let rec print_formule (x: formule) : unit =
  match x with
  | Var(p) -> print_string p;
  | And(p,q) ->
      print_string "(";
      print_formule p;
      print_string "%*$\land$*)";
      print_formule q;
      print_string ")";
  | Or(p,q) ->
      print_string "(";
      print_formule p;
      print_string "%*$\lor$*)";
      print_formule q;
      print_string ")";
  | Imply(p,q) ->
      print_string "(";
      print_formule p;
      print_string "%*$\rightarrow$*)";
      print_formule q;
      print_string ")";
  | Equiv(p,q) ->
      print_string "(";
      print_formule p;
      print_string "%*$\leftrightarrow$*)";
      print_formule q;
      print_string ")";
  | Top -> print_string "%*$\top$*)";
  | Bot -> print_string "%*$\bot$*)";
  | Not(p) ->
      print_string "(";
      print_string "%*$\lnot$*)";
      print_formule p;
      print_string ")"

let rec vars (x: formule) : string set =
  match x with
  | Var(p) -> Set.add p Set.empty
  | And(p,q) | Or(p,q) | Imply(p,q) | Equiv(p,q) -> Set.union (vars p) (vars q)
  | Top | Bot -> Set.empty
  | Not(p) -> vars p

exception Missing_Env

let rec interprete (f: formule) (e: env_prop) : bool =
  match f with
  | Var(p) ->
      let rec aux e =
        match e with
        | [] -> raise Missing_Env
        | (v,t)::_ when t -> true
        | _::q -> aux q
      in aux e
  | And(p,q) -> (interprete p e) && (interprete q e)
  | Or(p,q) -> (interprete p e) || (interprete q e)
  | Imply(p,q) -> interprete (Or(q, Not(p))) e
  | Equiv(p,q) -> (interprete p e) = (interprete q e)
  | Top -> true
  | Bot -> false
  | Not(p) -> not (interprete p e)

let rec all_envs (vars: string list): env_prop list =
  match vars with
  | x :: q -> let envs = all_envs q in
    let aux1 e = (x, true) :: e in
    let aux2 e = (x, false) :: e in
    (List.map aux1 envs) @ (List.map aux2 envs)
  | [] -> []

exception Unsat

let sat (f: formule): env_prop =
  let envs = all_envs (vars f) in
  let envs_valides = List.filter (interprete f) envs in
  match envs_valides with
  | [] -> raise Unsat
  | x::_ -> x

let est_valide (f: formule): bool =
  let envs = all_envs (vars f) in
  List.for_all (interprete f) envs

let est_cons_semantique (f: formule) (g: formule): bool =
  let envs = all_envs (vars f) in
  let envs_f_valides = List.filter (interprete f) envs in
  List.for_all (interprete g) envs_f_valides

let equiv (f: formule) (g: formule): bool =
  let envs = all_envs (vars f) in
  let envs_f_valides = List.filter (interprete f) envs in
  let envs_g_valides = List.filter (interprete g) envs in
  envs_f_valides = envs_g_valides

let models (f: formule): env_prop set =
  let envs = all_envs (vars f) in
  Set.of_list (List.filter (interprete f) envs)


(* Exercice 2 *)
type fct_bool = env_prop -> bool

let formule_of_fct_bool (vars: string list) (f: fct_bool): formule =
  let envs = all_envs vars in
  let envs_valides = List.filter f envs in
  let rec gen_conj (rho: env_prop): formule =
    match rho with
    | [] -> Top
    | (p,b)::q -> if b then And(Var(p), gen_conj q)
                  else And(Not(Var(p)), gen_conj q)
  in
  let conjs = List.map gen_conj envs_valides in
  List.fold_left (fun x a -> Or(x, a)) Bot conjs

let formule_of_fct_bool2 (vars: string list) (f: fct_bool): formule =
  let f' rho = not (f rho) in
  let h = formule_of_fct_bool vars f' in
  let rec convert_not (h: formule) =
    match h with
    | Or(a, b) -> And(convert_not a, convert_not b)
    | And(a, b) -> Or(convert_not a, convert_not b)
    | Not(a) -> convert_not a
    | Var(p) -> Not(Var(p))
    | Imply(a,b) -> convert_not (Or(b, Not(a)))
    | Equiv(a,b) -> convert_not (And(Imply(a, b), Imply(b, a)))
    | Top -> Bot
    | Bot -> Top
  in convert_not h

(* Exercice 3 *)
type rewrite = formule -> formule option

let rec rewrite_one (r: rewrite) (f: formule): formule option =
  match r f with
  | Some(f') -> Some(f')
  | None -> begin
      match f with
      | Or(a, b) -> let a' = rewrite_one r a in
        if a' = None then
          let b' = rewrite_one 
    end
















































