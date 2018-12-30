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
