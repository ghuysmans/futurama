open Permutations

module Op = Operation.Standard (struct type item = int let order = 16 end)
module C = Circle.Make (Op)
let of_list_by_reversing = Tools.I.of_list_by_reversing (module C)
let of_list_by_inverting_twice = Tools.I.of_list_by_inverting_twice (module C)

let script = C.Op.[
  Rotate 3;
  Transpose (1, 2);
  Transpose (3, 4);
]

let dump t =
  C.iter (fun _ -> Printf.printf "%d ") t;
  Printf.printf "\n"

let () =
  let p = of_list_by_inverting_twice script in
  Printf.printf "Invert_twice\n";
  dump p

let () =
  let p = of_list_by_reversing script in
  Printf.printf "Reverse\n";
  dump p

let () =
  let p = C.make () in
  C.update p (Rotate 4);
  let q = C.make () in
  C.update q C.Op.(inv (Rotate 4));
  Printf.printf "correct inv rot\n";
  dump C.(p @ q)
