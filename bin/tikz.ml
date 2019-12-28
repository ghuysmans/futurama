module Make (I: Permutations.Model.Enumerable with type item := int) = struct
  let draw ch a =
    a |> I.iter @@ fun i i' ->
      Printf.fprintf ch "\\draw(%d,1)--(%d,0);\n" i i'

  let tikz ch a =
    Printf.fprintf ch "\\begin{tikzpicture}\n%a\\end{tikzpicture}\n" draw a

  let tikz_pic name ch a =
    Printf.fprintf ch "\\tikzset{%s/.pic={code={\n%a}}}\n" name draw a

  let tikz_graph f ch l =
    Printf.fprintf ch "\\tikz \\graph { %a" f (List.hd l);
    List.iter (fun x -> Printf.fprintf ch " -> %a" f x) (List.tl l);
    Printf.fprintf ch " };"
end
