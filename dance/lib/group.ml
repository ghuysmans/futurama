(** Generic group *)
module type G = sig
  type t

  (** {make ()} creates an identity value. *)
  val make: unit -> t

  val equal: t -> t -> bool
  val inv: t -> t
  val (@): t -> t -> t
end

module Tools (G: G) = struct
  open G

  let pow a b =
    let rec f a b acc =
      if b = 0 then
        acc
      else if b mod 2 = 0 then
        (* acc * a^(2k) = acc * (a^2)^k *)
        f (a @ a) (b / 2) acc
      else
        (* acc * a^(2k+1) = acc * a * a^(2k) = acc * a * (a^2)^k *)
        f (a @ a) (b / 2) (acc @ a)
        (*                = acc * a^(2k) * a = acc * (a^2)^k * a [assoc] *)
        (* f (a @ a) (b / 2) (a @ acc) *)
    in
    f a b (make ())
end

module Direct_product (A: G) (B: G) = struct
  type t = A.t * B.t
  let make () = A.make (), B.make ()
  let equal (a, b) (a', b') = A.equal a a' && B.equal b b'
  let inv (a, b) = A.inv a, B.inv b
  let (@) (a, b) (a', b') = A.(a @ a'), B.(b @ b')
end
