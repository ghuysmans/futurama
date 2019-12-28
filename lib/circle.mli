module Make : functor (Op: Operation.S with type item := int) -> sig
  open Model
  include I with module Op = Op
  include Enumerable.S with type t := t and type item := int
  include Algebra.Group.I with type t := t
  val of_array : int array -> t
  val to_array : t -> int array
end
