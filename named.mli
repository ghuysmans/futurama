(* A permutation, represented extensively *)
type 'a t

type 'a a =
  (* An {x, y} transposition swaps elements {x} and {y}. *)
  | Transpose of 'a * 'a
  (* A rotation of {i} moves the last {i} elements to the front. *)
  | Rotate_right of int

(* {of_list l} creates a permutation from transpositions. *)
val of_list: 'a a list -> 'a t

(* {length permutation} returns the {permutation}'s length. *)
val length: 'a t -> int

(* {get permutation x} maps element {x} through the {permutation}. *)
val get: 'a t -> 'a -> 'a

(* {iter f permutation} iterates through the {permutation}. *)
val iter: ('a -> 'a -> unit) -> 'a t -> unit

(* {transform permutation s} maps {s} through the {permutation}. *)
val transform: 'a t -> 'a array -> 'a array

(* {explain permutation s} shows how the {permutation} maps each position. *)
val explain: 'a t -> 'a array -> 'a array


val of_disjoint_cycles: 'a list list -> 'a t
val to_disjoint_cycles: 'a t -> 'a list list
