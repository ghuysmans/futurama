module Make : functor (Op: Operation.S with type item := int) -> sig
  open Model
  include I with type t = int array and module Op = Op
  include Enumerable.S with type t := t and type item := int
  include Algebra.Group.I with type t := t
end
