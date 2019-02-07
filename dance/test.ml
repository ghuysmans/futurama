module F (I: sig val order: int end) = struct
  type t = X
  let x = X
end

module I = struct
  let order = 0
end
module M = F (I)
module N = F (I)
module O = F (struct let order = 0 end)

let _ =
  M.x = N.x && (* ok, applicative *)
  true (* N.x = O.x (* ... but the anonymous module is not the same as I *) *)

type 'a regular_nested =
  | List of 'a list
  | Nested of 'a regular_nested list

let _ = Nested[ List [1]; Nested [List[2;3]]; Nested[Nested[]] ]

let rec maximal_depth = function
  | List _ -> 1
  | Nested [] -> 0
  | Nested (a :: q) -> 1 + max (maximal_depth a) (maximal_depth (Nested q))

type 'a nested =
  | List of 'a list
  | Nested of 'a list nested

let rec depth: 'a. 'a nested -> _ = function
  | List _ -> 1
  | Nested n -> 1 + depth n

let _ = depth (Nested (List [[7]; [8]]))

let len nested =
  (* grâce à f, ça marche pour des listes de n'importe quel niveau donné *)
  let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0 in
  (* fonction qui fait le vrai boulot avec en argument le f... *)
  let rec len: 'a. ('a list -> int) -> 'a nested -> int = fun nested_len n ->
    match n with
    | List l -> nested_len l
    | Nested n -> len (map_and_sum nested_len) n
  in
  (* appel initial *)
  len List.length nested

let _ = len (Nested (List [[1]]))

let average_depth x y = (depth x + depth y) / 2
let average_len x y = (len x + len y) / 2
let _ = average_len (List [2]) (List [[]])

(* on l'applique deux fois mais le paramètre ne peut pas changer de type entre
   les appels : x et y doivent être de même type. *)
let average f x y = (f x + f y) / 2
(* moins générale, du coup *)
let average_depth' x y = average depth x y

(* essai perso, ça fonctionne !! *)
module Avg (S: sig
  (* juste 'a ne suffit pas, c'est trop pour depth ou len *)
  val f: 'a nested -> int
end) = struct
  let f x y = (S.f x + S.f y) / 2
end
module D = Avg (struct let f = depth end)
module L = Avg (struct let f = len end)
