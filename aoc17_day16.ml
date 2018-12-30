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

let tortoise_and_hare init script n =
  let rec move (x, l) =
    match l with
    | [] -> move (x, script)
    | s :: t -> step x s, t
  in
  let (<=>) (x, _) (y, _) = x = y in
  Floyd.pow (<=>) move (init, []) n |> fst


let () =
  let script = read_moves () in
  let n, a =
    match Sys.argv with
    | [| _ |] -> 1, `Naive
    | [| _; n |] -> int_of_string n, `Naive
    | [| _; "-t"; n |] -> int_of_string n, `Tortoise
    | [| _; "-c"; n |] -> int_of_string n, `Cycle
    | [| _; "-p"; n |] | [| _; "-g"; n |] -> int_of_string n, `Pow
    | _ ->
      Printf.eprintf "usage: %s [-t|-c|-p] [n]\n" Sys.argv.(0);
      exit 1
  in
  let init = "abcdefghijklmnop" in
  let result =
    match a with
    | `Naive -> Util.pow (fun x -> List.fold_left step x script) init n
    | `Tortoise -> tortoise_and_hare init script n
    | `Cycle | `Pow as a' ->
      let pos, chars =
        let f m (pos, chars) =
          match m with
          | Spin n -> Fixed.Rotate_right n :: pos, chars
          | Exchange (i, j) -> Fixed.Transpose (i, j) :: pos, chars
          | Partner (x, y) ->
            pos, Fixed.Transpose (Char.code x - 97, Char.code y - 97) :: chars
        in
        List.fold_right f script ([], [])
      in
      let pos, chars = Fixed.of_list 16 pos, Fixed.of_list 16 chars in
      let pos, chars =
        match a' with
        | `Cycle ->
          let fast_forward p n =
            Fixed.to_disjoint_cycles p |>
            Disjoint_cycles.pow n |>
            Fixed.of_disjoint_cycles 16
          in
          fast_forward pos n, fast_forward chars n
        | `Pow ->
          Fixed.pow pos n, Fixed.pow chars n
      in
      Fixed.(explain chars init |> transform pos)
  in
  Printf.eprintf "%s\n" result
