module Simulation = struct
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

  let run script x =
    List.fold_left step x script
end

module Algebra = struct
  module S_16 = Circle.Make (struct let n = 16 end)
  module Iso = Adapter.Char (struct let from = 'a' end)
  module Characters = Adapter.Adapt (S_16) (Iso)
  include Group.Tools (Group.Direct_product (S_16) (Characters))
end
