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
