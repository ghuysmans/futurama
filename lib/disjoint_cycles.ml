let pow n l =
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
  in
  List.map (skip n) l |> List.flatten

let order l =
  match List.map List.length l with
  | [] -> 1
  | h :: t -> List.fold_left Algebra.Euclid.lcm h t

let inv l =
  List.map List.rev l


let disjoint l =
  let rec f = function
    | [] -> true
    | x :: y :: _ when x = y -> false
    | _ :: t -> f t
  in
  f (List.flatten l |> List.sort compare)

let of_array a =
  let visited = Array.make (Array.length a) false in
  let rec walk first cycle current =
    if current = first then
      List.rev cycle
    else (
      visited.(current) <- true;
      walk first (current :: cycle) a.(current)
    )
  in
  let rec f acc i =
    if i = Array.length a then
      List.rev acc
    else if visited.(i) then
      (* already in a previous cycle... *)
      f acc (i + 1)
    else
      match walk i [i] a.(i) with
      | [_] -> f acc (i + 1) (* useless *)
      | c -> f (c :: acc) (i + 1)
  in
  f [] 0

let to_array n l =
  let a = Array.init n (fun i -> i) in
  l |> List.iter (fun c ->
    let fst = List.hd c in
    (* create a cycle in the array (the accumulator becomes the last index) *)
    a.(List.fold_left (fun prev i -> a.(prev) <- i; i) fst (List.tl c)) <- fst
  );
  a


let to_swaps cycles =
  let rec successive_pairs = function
    | [] | [_] -> []
    | a :: (b :: _ as t) ->
      (a, b) :: successive_pairs t
  in
  List.map (fun l -> successive_pairs (List.rev l)) cycles |> List.flatten

type 'a m = {
  dst: 'a;
  src: 'a;
}

let to_moves tmp cycles =
  let f = function
    | [] | [_] -> []
    | first :: l ->
      let rec g dst = function
        | [] -> [{src = tmp; dst}]
        | h :: t -> {src = h; dst} :: g h t
      in
      {src = first; dst = tmp} :: g first (List.rev l)
  in
  List.map f cycles |> List.flatten

(*
let of_moves l =
  test for disjoint cycles...
*)

let to_dot f cycles = Odot.{
  strict = false;
  kind = Graph;
  id = None;
  stmt_list =
    cycles |>
    List.map (function
      | [] | [_] -> []
      | h :: t -> [Stmt_edge (
        Edge_node_id (simple_node_id (f h)),
        List.map (fun n -> Edge_node_id (simple_node_id (f n))) t,
        []
      )]
    ) |>
    List.flatten
}
