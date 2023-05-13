exception Invalid_argument;;
let rec r1 (l: 'a list): 'a =
        match l with
        | [] -> raise Invalid_argument
        | x::q -> if q = [] then x else r1 q

let rec r2 (l: 'a list): 'a option =
        match l with
        | [] -> None
        | x::q -> if q = [] then Some(x) else r2 q

let r3 (l: 'a list): 'a list =
        let rec transvase l = function
        | x :: q -> transvase (x::l) q
        | []     -> l
        in transvase [] l


let r4 (l2: 'a list): 'a list =
        List.fold_left (fun l a -> a::l) [] l2

let rec r5 (l1: 'a list) (l2: 'a list): 'a list =
        match l1 with
        | [] -> l2
        | x :: q -> x :: (r5 q l2)

let r6 (l2: 'a list) (l1: 'a list): 'a list =
        List.fold_left (fun l a -> a::l) l1 (List.rev l2)

let rec r7 (l: 'a list) (x: 'a): 'a list =
        match l with
        | [] -> []
        | a :: q ->
                if a = x then r7 q x
                else a :: (r7 q x)

let r8 (l: 'a list) (i: int): ('a list) * ('a list) =
        let rec aux (l: 'a list) (l_inf: 'a list) (l_sup: 'a list) (k: int) =
                match l with
                | x :: q when k < i -> aux q (x::l_inf) l_sup (k+1)
                | x :: q            -> aux q l_inf (x::l_sup) (k+1)
                | []                -> (l_inf, l_sup)
        in let (inf, sup) = aux l [] [] 0 in
        (List.rev inf, List.rev sup)

let rec r9 (l: 'a list) (i: int): ('a list) * ('a list) =
        match l with
        | l when i <= 0 -> ([], l)
        | x :: q -> let (inf, sup) = r9 q (i - 1) in (x::inf, sup)
        | [] -> ([], [])

(* les indices de la liste commencent à 0 *)
let rec r10 (l: 'a list): ('a list) * ('a list) =
        match l with
        | x :: y :: q -> let (pair, impair) = r10 q in (x::pair, y::impair)
        | x :: []     -> ([x], [])
        | []          -> ([], [])


let r11 (l: 'a list): bool = (List.rev l) = l

let rec r12 (l: 'a list list): 'a list =
        match l with
        | [] -> []
        | a :: q -> a @ (r12 q)

let r13 (l: 'a list list): 'a list =
        let rec aux (l: 'a list list) (l': 'a list) =
                match l with
                | x :: q -> aux q ((List.rev x) @ l')
                | [] -> l'
        in List.rev(aux l [])


let rec r14 (l: 'a list list): 'a list =
        let aux l =
                match l with
                | [] -> []
                | a :: q -> List.fold_left (fun acc x -> x::acc) (List.rev a) (r14 q)
        in aux (List.rev l)

let rec r15 (l: 'a list): 'a * 'a =
        match l with
        | [] -> raise Invalid_argument
        | a :: [] -> (a, a)
        | a :: q ->
                let (u, v) = r15 q in
                min a u, max a v

let r16 (l: 'a list): 'a * 'a =
        match l with
        | [] -> raise Invalid_argument
        | m :: _ ->
                let rec aux (u: 'a) (v: 'a) (l: 'a list) =
                        match l with
                        | [] -> u, v
                        | a :: q -> aux (min a u) (max a v) q
                in aux m m l

let r17 
