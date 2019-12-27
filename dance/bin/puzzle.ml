open Permutations
module Op = Aoc17_day16.Original

let () =
  match Sys.argv with
  | [| _ |] ->
    let module N = Naive.Make (Op) in
    let t = N.make () in
    List.iter (N.update t) (Op.read_list ());
    print_endline (Bytes.unsafe_to_string t)
  | [| _; "-2" |] ->
    let module Op2 = Aoc17_day16.To_composite (Op) in
    let module C = Circle.Make (Op2.S_n) in
    let module G = struct
      include Model.Direct_product.I (C) (C) (Op2)
      include (Algebra.Group.Direct_product.I (C) (C) :
        Algebra.Group.I with type t := t)
    end in
    let module T = Algebra.Monoid.Tools.I (G) in
    let pos, char =
      Tools.I.Monoid.of_list (module G) (Op2.read_list ()) |>
      T.fastpow ~b:1_000_000_000
    in
    pos |> C.inv |> C.iter (fun _ i' ->
      print_char (C.get char i' + 97 |> Char.chr)
    );
    print_newline ()
  | _ ->
    Printf.eprintf "usage: %s [-2]\n" Sys.argv.(1);
    exit 1
