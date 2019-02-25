module type Isomorphism = sig
  type i
  type j
  val f: i -> j
  val f': j -> i
end

module Adapt (I: Permutation.S) (M: Isomorphism with type j = I.i) :
       Permutation.S with type i = M.i = struct
  let n = I.n

  type i = M.i
  let int_of_i i = I.int_of_i (M.f i)

  type t = I.t

  let make = I.make
  let equal = I.equal
  let get t i = M.f' (I.get t (M.f i))
  let iter f = I.iter (fun i i' -> f (M.f' i) (M.f' i'))
  let inv = I.inv
  let (@) = I.(@)

  let update t o =
    I.update t (
      match o with
      | Permutation.Operation.Transpose (x, y) -> Transpose (M.f x, M.f y)
      | Rotate _ as r -> r
    )
end


module Char (I: sig val from: char end) = struct
  type i = char
  type j = int
  let f i = Char.code i - Char.code I.from
  let f' j = Char.chr (j + Char.code I.from)
end

module Named (E: sig type t val elements: t array end) :
       Isomorphism with type i = E.t and type j = int = struct
  type i = E.t
  type j = int
  let h =
    let h = Hashtbl.create 100 in
    Array.iteri (fun i x -> Hashtbl.replace h x i) E.elements;
    h
  let f = Hashtbl.find h
  let f' = Array.get E.elements
end
