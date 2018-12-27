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

let eval charset ~pos ~chars =
  Fixed.explain chars charset |> (* substitute *)
  Fixed.transform pos (* move *)


let () =
  let script = read_line () |> moves_of_string in
  let pos, chars =
    let f m (pos, chars) =
      match m with
      | Spin n -> Fixed.Rotate_right n :: pos, chars
      | Exchange (i, j) -> Fixed.Transpose (i, j) :: pos, chars
      | Partner (x, y) ->
        pos, Fixed.Transpose (Char.code x - 97, Char.code y - 97) :: chars
    in
    let pos, chars = List.fold_right f script ([], []) in
    Fixed.of_list 16 pos, Fixed.of_list 16 chars
  in
  let charset = "abcdefghijklmnop" in
  Printf.eprintf "naive: %s\n" (List.fold_left step charset script);
  Printf.eprintf "fast: %s\n" (eval charset ~pos ~chars);
  let pos' = Fixed.to_disjoint_cycles pos in
  let chars' = Fixed.to_disjoint_cycles chars in
  Printf.printf "%a" Dot.header ();
  List.iter (Printf.printf "%a" Dot.(cycle int)) pos';
  let letter ch i = Printf.fprintf ch "%c" charset.[i] in
  List.iter (Printf.printf "%a" Dot.(cycle letter)) chars';
  Printf.printf "%a" Dot.footer ();
  let n = 1_000_000_000 in
  let pos = Disjoint_cycles.pow n pos' |> Fixed.of_disjoint_cycles 16 in
  let chars = Disjoint_cycles.pow n chars' |> Fixed.of_disjoint_cycles 16 in
  Printf.eprintf "%d: %s\n" n (eval charset ~pos ~chars)
