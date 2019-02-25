let skip s l =
  let a = Array.of_list l in
  let visited = Array.make (Array.length a) false in
  let rec walk first cycle current =
    if current = first then
      List.rev cycle
    else (
      visited.(current) <- true;
      walk first (a.(current) :: cycle) ((current + s) mod Array.length a)
    )
  in
  let rec f acc i =
    if i = Array.length a then
      List.rev acc
    else if visited.(i) then
      (* already in a previous cycle... *)
      f acc (i + 1)
    else
      match walk i [a.(i)] ((i + s) mod Array.length a) with
      | [_] -> f acc (i + 1) (* useless *)
      | c -> f (c :: acc) (i + 1)
  in
  f [] 0

let pow n l =
  List.map (skip n) l |> List.flatten

let order l =
  match List.map List.length l with
  | [] -> 1
  | h :: t -> List.fold_left Euclid.lcm h t

let inv l =
  List.map List.rev l


(*
let disjoint l =
  let rec f = function
    | [] -> true
    | x :: y :: _ when x = y -> false
    | _ :: t -> f t
  in
  f (List.flatten l |> List.sort compare)

let to_disjoint_cycles a =
  let visited = Array.make (Array.length a) false in
  let rec walk first cycle current =
    if current = first then
      List.rev cycle
    else (
      visited.(current) <- true;
      walk first (current :: cycle) (get a current)
    )
  in
  let rec f acc i =
    if i = Array.length a then
      List.rev acc
    else if visited.(i) then
      (* already in a previous cycle... *)
      f acc (i + 1)
    else
      match walk i [i] (get a i) with
      | [_] -> f acc (i + 1) (* useless *)
      | c -> f (c :: acc) (i + 1)
  in
  f [] 0

let of_disjoint_cycles n l =
  let a = id n in
  l |> List.iter (fun c ->
    let fst = List.hd c in
    (* create a cycle in the array (the accumulator becomes the last index) *)
    a.(List.fold_left (fun prev i -> a.(prev) <- i; i) fst (List.tl c)) <- fst
  );
  a
*)
