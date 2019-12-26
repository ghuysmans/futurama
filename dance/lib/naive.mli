module Make : functor (Op: Aoc17_day16.S) -> sig
  include Model.I with type t = bytes and module Op = Op
end
