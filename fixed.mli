(* A permutation, represented extensively *)
type t

type a =
  (* An {i, j} transposition swaps positions {i} and {j}. *)
  | Transpose of int * int
  (* A rotation of {i} moves the last {i} elements to the front. *)
  | Rotate_right of int

(* {of_list n l} creates a permutation over {n} elements from transpositions. *)
val of_list: int -> a list -> t

(* {a @ b} computes {a} ∘ {b}. *)
val (@): t -> t -> t

(* {length permutation} returns the {permutation}'s length. *)
val length: t -> int

(* {get permutation i} maps position {i} through the {permutation}. *)
val get: t -> int -> int

(* {iteri f permutation} iterates through the {permutation}. *)
val iteri: (int -> int -> unit) -> t -> unit

(* {transform permutation s} maps {s} through the {permutation}. *)
val transform: t -> string -> string

(* {explain permutation s} shows how the {permutation} maps each position. *)
val explain: t -> string -> string


val of_disjoint_cycles: int -> int list list -> t
val to_disjoint_cycles: t -> int list list
val disjoint: int list list -> bool
