let () =
  let a, h, l, p, b, e, w, f, z = 0, 1, 2, 3, 4, 5, 6, 7, 8 in
  let moves =
    [p, a; a, b; p, l; a, w; f, z; e, w; h, l] |>
    List.map (fun (x, y) -> Fixed.Transpose (x, y))
  in
  let charset = "ahlpbewfz" in
  let letter ch i = Printf.fprintf ch "%c" charset.[i] in
  Explain.latex stdout 16 letter moves
