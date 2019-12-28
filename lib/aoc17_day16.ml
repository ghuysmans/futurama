module type S = sig
  type t =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

  include Operation.Minimal with type t := t

  val read_list : unit -> t list
end

module Make (P : sig val n : int end) = struct
  type t =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

  let order = P.n

  let inv = function
    | Spin n -> Spin (order - n)
    | Exchange _ as t -> t
    | Partner _ as t -> t

  let read () =
    let len l =
      if l >= 0 && l <= order then
        l
      else
        failwith @@ Printf.sprintf "invalid length %d" l
    in
    let pos i =
      if i >= 0 && i < order then
        i
      else
        failwith @@ Printf.sprintf "invalid position %d" i
    in
    let char c =
      if c >= 'a' && c < Char.chr (97 + order) then
        c
      else
        failwith @@ Printf.sprintf "invalid character %C" c
    in
    match Scanf.scanf "%c" (fun x -> x) with
    | 's' -> Scanf.scanf "%d" (fun x -> Spin (len x))
    | 'x' -> Scanf.scanf "%d/%d" (fun x y -> Exchange (pos x, pos y))
    | 'p' -> Scanf.scanf "%c/%c" (fun x y -> Partner (char x, char y))
    | c -> failwith @@ Printf.sprintf "invalid move %C" c

  let read_list () =
    let rec f acc =
      Scanf.scanf "%c" (function
        | ',' -> f (read () :: acc)
        | '\n' -> List.rev acc
        | _ -> failwith "comma expected"
      )
    in
    f [read ()]
end

module Original = Make (struct let n = 16 end)

module To_composite (S : S) = struct
  module S_n = Operation.Standard (struct
    type item = int
    let order = S.order
  end)

  include Operation.Direct_product.Make (S_n) (S_n)

  let read_list () = S.read_list () |> List.map @@ fun t ->
    let char c = Char.code c - 97 in (* FIXME? easier for now... *)
    match t with
    | S.Spin n -> Left (Rotate n)
    | Exchange (i, j) -> Left (Transpose (i, j))
    | Partner (x, y) -> Right (Transpose (char x, char y))
end

module Only_positions (S : S) = struct
  include Operation.Standard (struct
    type item = int
    let order = S.order
  end)

  let read_list () =
    S.read_list () |>
    List.map (function
      | S.Spin n -> [Rotate n]
      | Exchange (i, j) -> [Transpose (i, j)]
      | Partner _ -> []) |>
    List.flatten
end

module Only_characters (S : S) = struct
  include Operation.Standard (struct
    type item = char
    let order = S.order
  end)

  let read_list () =
    S.read_list () |>
    List.map (function
      | S.Spin _ -> []
      | Exchange _ -> []
      | Partner (x, y) -> [Transpose (x, y)]) |>
    List.flatten
end

module Morph = struct
  module type S = sig
    include Operation.S
    val read_list : unit -> t list
  end

  module type M = sig
    include Operation.Morph.M
    val read_list : unit -> t list
  end

  module Make (I : Algebra.Isomorphism.S) (S : S with type item := I.i) = struct
    include Operation.Morph.Make (I) (S)
    let read_list () = S.read_list () |> List.map map
  end
end
