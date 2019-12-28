module Make : functor (Op: Operation.S with type item := int) -> sig
  open Model
  include S with module Op = Op
  include Enumerable with type t := t and type item := int
  include Algebra.Monoid.S with type t := t
end
