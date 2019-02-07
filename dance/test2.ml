open Permutations

module A' = Adapter.Char (struct let from = 'A' end)

module A = Adapter.Named (struct
  type t = [`A | `B | `C]
  let elements = [| `A; `B; `C |]
end)

module I = Adapter.Adapt (Functional) (A)

module G = Group.Make (I)


let () =
  let p = G.of_list `Reverse 10 Group.Operation.[
    Transpose (`A, `B);
    Transpose (`B, `C);
  ] in
  let f = function
    | `A -> 'A'
    | `B -> 'B'
    | `C -> 'C'
  in
  Printf.printf "%c%c%c\n" (f (I.get p `A)) (f (I.get p `B)) (f (I.get p `C))
