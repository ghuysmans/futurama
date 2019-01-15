open Permut

let () =
  let ffrr =
    [["df"; "uf"];
     ["dr"; "ur"];
     ["br"; "fr"; "fl"];
     ["dbr"; "ufr"; "dfl"];
     ["ulf"; "urb"; "drf"]] |>
    Named.of_disjoint_cycles
  in
  print_endline (Named.get ffrr "ufr")
