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
