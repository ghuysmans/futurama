open Permutations

module C = Circle.Make (struct let n = 16 end)
module K = Permutation.Make (C)

let script = Permutation.Operation.[
  Rotate 3;
  Transpose (1, 2);
  Transpose (3, 4);
]

let dump arr =
  Array.iter (Printf.printf "%d") arr;
  Printf.printf "\n"

let dump t = dump (C.to_array t)

let () =
  let p = K.of_list `Invert_twice script in
  Printf.printf "Invert_twice\n";
  dump p

let () =
  let p = K.of_list `Reverse script in
  Printf.printf "Reverse\n";
  dump p

let () =
  let p = C.make () in
  C.update p (Rotate 4);
  let q = C.make () in
  C.update q Permutation.Operation.(inv C.n (Rotate 4));
  Printf.printf "correct inv rot\n";
  dump C.(p @ q)
