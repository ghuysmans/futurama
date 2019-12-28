open Permutations

type t =
  | Red
  | Green
  | Blue

module Op = Operation.Named (struct
  type nonrec t = t
  let elements = [| Red; Green; Blue |]
end)

module G = Table.Make (Op.Indexed)
module V = Model.Enumerable.Morph (Op.Iso) (G)

let t =
  [Red, Green; Blue, Red; Green, Red] |>
  List.map (fun (x, y) -> Op.(map (Transpose (x, y)))) |>
  Tools.I.Group.of_list (module G)

let () =
  assert (V.get t Red = Red);
  assert (V.get t Green = Blue);
  assert (V.get t Blue = Green)
