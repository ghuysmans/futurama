open Permutations

module Op = Aoc17_day16.Original
module N = Naive.Make (Op)

let () =
  let t = N.make () in
  List.iter (N.update t) (Op.read_list ());
  print_endline (Bytes.unsafe_to_string t)
