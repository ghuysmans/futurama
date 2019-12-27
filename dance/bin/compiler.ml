open Permutations

module Op = Aoc17_day16.Only_positions (Aoc17_day16.Original)
module C = Circle.Make (Op)
module T = Algebra.Monoid.Tools.I (C)


let dump_cycles cmt =
  List.iter (fun cycle ->
    List.map string_of_int cycle |>
    String.concat " -> " |>
    cmt
  )

let c =
  (fun {Disjoint_cycles.dst; src} ->
    let var = function
      | -1 -> "tmp"
      | i -> Printf.sprintf "v%d" i
    in
    Printf.printf "%s = %s;\n" (var dst) (var src)),
  fun comment l -> if comment then dump_cycles (Printf.printf "//%s\n") l

let latex =
  (fun {Disjoint_cycles.dst; src} ->
    let var = function
      | -1 -> "t"
      | i -> Printf.sprintf "v_{%d}" i
    in
    Printf.printf "$%s \\leftarrow %s$\n\n" (var dst) (var src)),
  fun comment l ->
    if comment then dump_cycles (Printf.printf "%%%s\n") l;
    () (* TODO draw something? *)

let cycles =
  ignore,
  fun _ -> dump_cycles (Printf.printf "%s\n")

let main (emit, show_cycles) n comment opt =
  let compile t =
    let open Disjoint_cycles in
    let cycles = C.to_array t |> of_array in
    if comment then show_cycles comment cycles;
    to_moves (-1) cycles |> List.iter emit
  in
  let moves = Op.read_list () in
  if opt then
    let t = Tools.I.Monoid.of_list (module C) moves |> T.fastpow ~b:n in
    compile t
  else
    for _ = 1 to n do
      moves |> List.iter (fun m -> compile (C.just m))
    done


open Cmdliner

let lang =
  let doc = "output language" in
  (* FIXME this breaks the help page! *)
  let my_conv = Arg.enum [
    "c", c;
    "cycles", cycles;
    "javascript", c;
    "js", c;
    "latex", latex;
    "python", c; (* semicolons are ignored *)
  ] in
  Arg.(value & opt my_conv c & info ~doc ["l"; "language"])

let n =
  let doc = "iteration count" in
  Arg.(value & opt int 1 & info ~doc ["n"])

let opt =
  let doc = "enable optimizations" in
  Arg.(value & flag & info ~doc ["O"])

let comment =
  let doc = "insert comments" in
  Arg.(value & flag & info ~doc ["c"; "comment"])

let main_t =
  Term.(const main $ lang $ n $ comment $ opt)

let info =
  let doc = "a permutation compiler" in
  Term.info "permut" ~doc

let () =
  Term.exit @@ Term.eval (main_t, info)
