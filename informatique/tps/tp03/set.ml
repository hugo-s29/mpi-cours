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

