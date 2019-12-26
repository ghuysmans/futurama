module type Enumerable = sig
  (** to be substituted *)
  type t
  type item

  val get: t -> item -> item
  val iter: (item -> item -> unit) -> t -> unit
end

module type S = sig
  type t
  val equal: t -> t -> bool
  val id : t

  module Op : Operation.Minimal
  val just : Op.t -> t
  val step : t -> Op.t -> t
end

module type I = sig
  type t
  val equal: t -> t -> bool
  val copy : t -> t
  val make : unit -> t

  module Op : Operation.Minimal
  val just : Op.t -> t
  val update : t -> Op.t -> unit
end

module Direct_product = struct
  module S (A : S) (B : S)
         (Op : Operation.Direct_product.S with
          module A := A.Op and module B := B.Op) = struct
    type t = A.t * B.t
    let equal (a, b) (a', b') = A.equal a a' && B.equal b b'
    let id = A.id, B.id

    module Op = Op

    let just = function
      | Op.Left l -> A.just l, B.id
      | Op.Right r -> A.id, B.just r

    let step (a, b) = function
      | Op.Left l -> A.step a l, b
      | Op.Right r -> a, B.step b r
  end

  module I (A : I) (B : I)
         (Op : Operation.Direct_product.S with
          module A := A.Op and module B := B.Op) = struct
    type t = A.t * B.t
    let equal (a, b) (a', b') = A.equal a a' && B.equal b b'
    let copy (a, b) = A.copy a, B.copy b
    let make () = A.make (), B.make ()

    module Op = Op

    let just = function
      | Op.Left l -> A.just l, B.make ()
      | Op.Right r -> A.make (), B.just r

    let update (a, b) = function
      | Op.Left l -> A.update a l
      | Op.Right r -> B.update b r
  end
end
