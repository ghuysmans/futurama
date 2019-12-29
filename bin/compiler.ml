open Permutations

module type S = sig
  include Operation.S with type item := int
  val read_list : unit -> t list
  val string_of_item : int -> string
end

module Make (Op : S) = struct
  module C = Circle.Make (Op)
  module T = Algebra.Monoid.Tools.I (C)

  let dump_cycles cmt =
    List.iter (fun cycle ->
      List.map Op.string_of_item cycle |>
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
        | -1 -> "T"
        | i ->
          (* FIXME? *)
          if Op.string_of_item == string_of_int then
            Printf.sprintf "v_{%d}" i
          else
            Op.string_of_item i (* TODO escape *)
      in
      Printf.printf "$%s \\leftarrow %s$\n\n" (var dst) (var src)),
    fun comment l ->
      if comment then dump_cycles (Printf.printf "%%%s\n") l;
      () (* TODO draw something? *)

  let cycles =
    ignore,
    fun _ -> dump_cycles (Printf.printf "%s\n")

  let dot =
    ignore,
    fun _ l -> Disjoint_cycles.to_dot Op.string_of_item l |> Odot.print stdout

  let main (emit, show_cycles) n comment opt =
    let compile t =
      let open Disjoint_cycles in
      let cycles = C.to_array t |> of_array in
      show_cycles comment cycles;
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
end


open Cmdliner

type t =
  | Positions
  | Characters

type l =
  | C
  | Cycles
  | Dot
  | LaTeX

let main typ order lang n comment opt =
  let module Full = Aoc17_day16.Make (struct let n = order end) in
  let (module Op : S) =
    match typ with
    | Positions -> (module struct
        include Aoc17_day16.Only_positions (Full)
        let string_of_item = string_of_int
      end)
    | Characters ->
      let module Ch = Aoc17_day16.Only_characters (Full) in
      let module I = Algebra.Isomorphism.Char (struct let from = 'a' end) in
      (module struct
        include Aoc17_day16.Morph.Make (I) (Ch)
        let string_of_item i = String.make 1 (I.f' i)
      end)
  in
  let module M = Make (Op) in
  let lang =
    match lang with
    | C -> M.c
    | Cycles -> M.cycles
    | Dot -> M.dot
    | LaTeX -> M.latex
  in
  M.main lang n comment opt

let typ =
  let doc = "move type" in
  let my_conv = Arg.enum [
    "c", Characters;
    "char", Characters;
    "characters", Characters;
    "p", Positions;
    "pos", Positions;
    "positions", Positions;
  ] in
  Arg.(value & opt my_conv Positions & info ~doc ["m"; "move"])

let order =
  let doc = "order" in
  Arg.(value & opt int 16 & info ~doc ["l"; "w"; "length"; "width"])

let lang =
  let doc = "output language" in
  let my_conv = Arg.enum [
    "c", C;
    "cycles", Cycles;
    "dot", Dot;
    "javascript", C;
    "js", C;
    "latex", LaTeX;
    "python", C;
  ] in
  Arg.(value & opt my_conv C & info ~doc ["f"; "format"; "language"])

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
  Term.(const main $ typ $ order $ lang $ n $ comment $ opt)

let info =
  let doc = "a permutation compiler" in
  Term.info "permut" ~doc

let () =
  Term.exit @@ Term.eval (main_t, info)
