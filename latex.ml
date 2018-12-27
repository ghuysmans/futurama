open Fixed

let draw ch a =
  iteri (fun i i' -> Printf.fprintf ch "\\draw(%d,1)--(%d,0);\n" i i') a

let tikz ch a =
  Printf.fprintf ch "\\begin{tikzpicture}\n%a\\end{tikzpicture}\n" draw a

let tikz_pic name ch a =
  Printf.fprintf ch "\\tikzset{%s/.pic={code={\n%a}}}\n" name draw a
