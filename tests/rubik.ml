open Permutations

let ffrr = [
  ["df"; "uf"];
  ["dr"; "ur"];
  ["br"; "fr"; "fl"];
  ["dbr"; "ufr"; "dfl"];
  ["ulf"; "urb"; "drf"]
]

module Op = Operation.Named (struct
  type t = string
  let elements = List.flatten ffrr |> Array.of_list
end)

module G = Table.Make (Op.Indexed)
module V = Model.Enumerable.Morph (Op.Iso) (G)


let () =
  let cycles = List.map (List.map Op.Iso.f') ffrr in
  let t = Disjoint_cycles.to_array Op.order cycles in
  assert (V.get t "df" = "uf");
  assert (V.get t "uf" = "df");
  assert (V.get t "dr" = "ur");
  assert (V.get t "ur" = "dr");
  assert (V.get t "br" = "fr");
  assert (V.get t "fr" = "fl");
  assert (V.get t "fl" = "br");
  assert (V.get t "dbr" = "ufr");
  assert (V.get t "ufr" = "dfl");
  assert (V.get t "dfl" = "dbr");
  assert (V.get t "ulf" = "urb");
  assert (V.get t "urb" = "drf");
  assert (V.get t "drf" = "ulf")
