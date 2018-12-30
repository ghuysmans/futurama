type move =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

exception Invalid_move of string

let read_move () =
  match Scanf.scanf "%c" (fun x -> x) with
  | 's' -> Scanf.scanf "%d" (fun x -> Spin x)
  | 'x' -> Scanf.scanf "%d/%d" (fun x y -> Exchange (x, y))
  | 'p' -> Scanf.scanf "%c/%c" (fun x y -> Partner (x, y))
  | c -> failwith @@ Printf.sprintf "unexpected character %c" c

let read_moves () =
  let rec f acc =
    Scanf.scanf "%c" (function
      | ',' -> f (read_move () :: acc)
      | '\n' -> List.rev acc
      | _ -> failwith "comma expected"
    )
  in
  f [read_move ()]

let rec step a = function
  | Spin i ->
    let l = String.length a - i in
    String.sub a l i ^ String.sub a 0 l
  | Exchange (p, p') ->
    let p, p' = min p p', max p p' in
    String.sub a 0 p ^ (* before p *)
    String.sub a p' 1 ^ (* p' *)
    String.sub a (p + 1) (p' - p - 1) ^ (* between p and p' *)
    String.sub a p 1 ^ (* p *)
    String.sub a (p' + 1) (String.length a - p' - 1) (* after p' *)
  | Partner (c, c') ->
    let p = String.index a c in
    let p' = String.index a c' in
    step a (Exchange (p, p'))

let find_cycle_and_ff init steps n =
  let rec move (x, l) =
    match l with
    | [] -> move (x, steps)
    | s :: t -> step x s, t
  in
  let (<=>) (x, _) (y, _) = x = y in
  let rec f (tortoise, hare) rem = function
    | `Race ->
      if tortoise <=> hare then
        f (move tortoise, hare) rem (`Cycle 1)
      else
        f (move tortoise, move (move hare)) (rem - 1) `Race
    | `Cycle cycle ->
      if tortoise <=> hare then
        f (tortoise, hare) (rem mod cycle) `Last
      else
        f (move tortoise, hare) rem (`Cycle (cycle + 1))
    | `Last ->
      if rem = 0 then
        fst tortoise
      else
        f (move tortoise, hare) (rem - 1) `Last
  in
  f ((init, steps), move (move (init, steps))) n `Race


let () =
  let script = read_moves () in
  let init = "abcdefghijklmnop" in
  Printf.eprintf "%s\n" (find_cycle_and_ff init script 1_000_000_000)
