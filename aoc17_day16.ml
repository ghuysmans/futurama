type move =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

exception Invalid_move of string

let move_of_string s =
  let open Tyre in
  let p = regex (Re.rg 'a' 'p') in
  let re = route [
    ((start *> char 's' *> int <* stop) --> fun x ->
      Spin x);
    ((start *> char 'x' *> int <&> char '/' *> int <* stop) --> fun (a, b) ->
      Exchange (a, b));
    ((start *> char 'p' *> p <&> char '/' *> p <* stop) --> fun (p, q) ->
      Partner (p.[0], q.[0]));
  ] in
  match Tyre.exec re s with
  | Ok x -> x
  | Error _ -> raise (Invalid_move s)

let moves_of_string s =
  Re.(split (char ',' |> compile) s) |>
  List.map move_of_string

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
