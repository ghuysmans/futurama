let header ch () = Printf.fprintf ch "digraph {\n"

let string ch s = Printf.fprintf ch "\"%s\"" (String.escaped s)
let int ch i = Printf.fprintf ch "%d" i

let cycle f ch l =
  List.iter (fun x -> Printf.fprintf ch "%a -> " f x) l;
  Printf.fprintf ch "%a\n" f (List.hd l)

let footer ch () = Printf.fprintf ch "}\n"
