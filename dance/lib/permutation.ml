module Operation = struct
  type 'a t =
    | Transpose of 'a * 'a
    | Rotate of int

  let inv order = function
    | Transpose _ as t -> t
    | Rotate n -> Rotate (order - n)
end

(** Finite permutation group *)
module type S = sig
  include Group.G
  val n: int
  type i
  val int_of_i: i -> int
  val get: t -> i -> i
  val iter: (i -> i -> unit) -> t -> unit
  val update: t -> i Operation.t -> unit
end

module Make (I: S) = struct
  open I

  let order t =
    let id = make () in
    let rec f u n =
      if equal u id then
        n
      else
        f (t @ u) (n + 1)
    in
    f t 0

  let of_list how l =
    let t = make () in
    match how with
    | `Invert_twice ->
      List.map (Operation.inv n) l |> List.iter (update t);
      inv t
    | `Reverse ->
      List.rev l |> List.iter (update t);
      t
end
