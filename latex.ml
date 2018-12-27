let draw ch a =
  Fixed.iteri (fun i i' -> Printf.fprintf ch "\\draw(%d,1)--(%d,0);\n" i i') a

let tikz ch a =
  Printf.fprintf ch "\\begin{tikzpicture}\n%a\\end{tikzpicture}\n" draw a

let tikz_pic name ch a =
  Printf.fprintf ch "\\tikzset{%s/.pic={code={\n%a}}}\n" name draw a

let tikz_graph f ch l =
  Printf.fprintf ch "\\tikz \\graph { %a" f (List.hd l);
  List.iter (fun x -> Printf.fprintf ch " -> %a" f x) (List.tl l);
  Printf.fprintf ch " };"

let chain f ch l =
  Printf.fprintf ch "$%a" f (List.hd l);
  List.iter (fun x -> Printf.fprintf ch " \\rightarrow %a" f x) (List.tl l);
  Printf.fprintf ch "$"

let list n f ch l =
  Printf.fprintf ch "\\begin{%s}\n" n;
  List.iter (fun x -> Printf.fprintf ch "\\item %a\n" f x) l;
  Printf.fprintf ch "\\end{%s}\n" n

let int ch = Printf.fprintf ch "%d"
let string ch = Printf.fprintf ch "%s"
