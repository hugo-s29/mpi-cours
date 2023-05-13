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

let print_env_prop (e: env_prop): unit =
  Format.printf "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (s, b) -> Format.fprintf fmt "%s ~> %s" s (if b then "V" else "F"))
    ) e
