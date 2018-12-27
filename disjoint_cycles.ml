let skip n l =
  let n = n mod List.length l in (* avoid overflows? *)
  let a = Array.of_list l in (* for random access *)
  let rec f acc i =
    if i = 0 then
      (* new cycle: we're done! *)
      List.rev acc
    else
      f (a.(i) :: acc) ((i + n) mod List.length l)
  in
  f [a.(0)] n

let pow n =
  List.map (skip n)
