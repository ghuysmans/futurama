open Permutations

open Circle
module K = Group.Make (Circle)

let script = Group.Operation.[
  Rotate 3;
  Transpose (1, 2);
  Transpose (3, 4);
]

let dump arr =
  Array.iter (Printf.printf "%d") arr;
  Printf.printf "\n"

let dump t = dump (to_array t)

let () =
  let p = K.of_list `Invert_twice 10 script in
  Printf.printf "Invert_twice\n";
  dump p

let () =
  let p = K.of_list `Reverse 10 script in
  Printf.printf "Reverse\n";
  dump p

let () =
  let p = make 10 in
  update p (Rotate 4);
  let q = make 10 in
  update q Group.Operation.(inv 10 (Rotate 4));
  Printf.printf "correct inv rot\n";
  dump K.(p @ q)
