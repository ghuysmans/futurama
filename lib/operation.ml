module type Minimal = sig
  type t

  (** maximal order *)
  val order : int

  (** inverts the effect of a given operation *)
  val inv : t -> t
end

module type S = sig
  (** to be substituted *)
  type item

  type t =
    | Transpose of item * item
    | Rotate of int

  include Minimal with type t := t
end

module Standard (P : sig type item val order: int end) :
       S with type item := P.item =
struct
  type t =
    | Transpose of P.item * P.item
    | Rotate of int

  (** maximal order *)
  let order = P.order

  (** inverts the effect of a given operation *)
  let inv = function
    | Transpose _ as t -> t
    | Rotate n -> Rotate (order - n)
end

module Direct_product = struct
  module type S = sig
    (** to be substituted *)
    module A : sig type t end
    module B : sig type t end

    type t =
      | Left of A.t
      | Right of B.t

    val order : int
    val inv : t -> t
  end

  module Make (A : Minimal) (B : Minimal) = struct
    type t =
      | Left of A.t
      | Right of B.t

    (** maximal order *)
    let order = Algebra.Euclid.lcm A.order B.order (* FIXME? *)

    let inv = function
      | Left l -> Left (A.inv l)
      | Right r -> Right (B.inv r)
  end
end

module Morph = struct
  module type M = sig
    include S

    type s
    val map : s -> t
    val unmap : t -> s
  end

  module Make (I : Algebra.Isomorphism.S) (S : S with type item := I.i) = struct
    type t =
      | Transpose of I.j * I.j
      | Rotate of int

    let order = S.order

    let map = function
      | S.Transpose (x, y) -> Transpose (I.f x, I.f y)
      | Rotate n -> Rotate n

    let unmap = function
      | Transpose (x, y) -> S.Transpose (I.f' x, I.f' y)
      | Rotate n -> S.Rotate n

    let inv t = unmap t |> S.inv |> map
  end
end

module Named (P : sig type t val elements: t array end) = struct
  module Iso = Algebra.Isomorphism.Reverse (Algebra.Isomorphism.Named (P))

  module Indexed = Standard (struct
    type item = int
    let order = Array.length P.elements
  end)

  include Morph.Make (Iso) (Indexed)
  let map, unmap = unmap, map
end
