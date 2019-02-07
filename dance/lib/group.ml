(** Generic finite group *)
module type G = sig
  type t
  val make: int -> t
  val equal: t -> t -> bool
  val order: t -> int
  val inv: t -> t
end

module Operation = struct
  type 'a t =
    | Transpose of 'a * 'a
    | Rotate of int

  let inv order = function
    | Transpose _ as t -> t
    | Rotate n -> Rotate (order - n)
end

(** Permutation group *)
module type S = sig
  include G
  type i
  val get: t -> i -> i
  val map: (i -> i) -> t -> t
  val update: t -> i Operation.t -> unit
end

(* FIXME maybe we ask for too much *)
module Make (I: S) = struct
  open I

  let of_list how order l =
    let t = make order in
    match how with
    | `Invert_twice ->
      List.map (Operation.inv order) l |> List.iter (update t);
      inv t
    | `Reverse ->
      List.rev l |> List.iter (update t);
      t

  let (@) g f = map (get g) f

  let pow a b =
    let rec f a b acc =
      if b = 0 then
        acc
      else if b mod 2 = 0 then
        f (a @ a) (b / 2) acc
      else
        f (a @ a) (b / 2) (acc @ a)
    in
    f a b (make (order a))
end
