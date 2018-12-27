let dot ch charset n ~pos ~chars =
  let len = String.length charset in
  let pos, chars = Fixed.of_list len pos, Fixed.of_list len chars in
  let pos' = Fixed.to_disjoint_cycles pos in
  let chars' = Fixed.to_disjoint_cycles chars in
  Dot.header ch ();
  List.iter Dot.(cycle int ch) pos';
  let letter ch i = Printf.fprintf ch "%c" charset.[i] in
  List.iter Dot.(cycle letter ch) chars';
  Dot.footer ch ();
  let pos = Disjoint_cycles.pow n pos' |> Fixed.of_disjoint_cycles len in
  let chars = Disjoint_cycles.pow n chars' |> Fixed.of_disjoint_cycles len in
  Fixed.explain chars charset |> (* substitute *)
  Fixed.transform pos |> (* move *)
  Printf.eprintf "%d: %s\n" n

let latex ch len f moves =
  moves |> List.iter (fun m ->
    (match m with
    | Fixed.Transpose (x, y) ->
      Printf.fprintf ch "$%a \\leftrightarrow %a$" f x f y
    | Fixed.Rotate_right i ->
      Printf.fprintf ch "$>>%d$" i);
    Printf.fprintf ch "\n\n%a\n" Latex.tikz (Fixed.of_list len [m]);
  );
  let permut = Fixed.of_list len moves in
  Printf.fprintf ch "one-step equivalent:\n\n%a\n" Latex.tikz permut;
  let cycles = Fixed.to_disjoint_cycles permut in
  Printf.fprintf ch "cycles:\n%a\n" Latex.(list "itemize" (chain f)) cycles
