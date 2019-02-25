module Make: functor (S: sig val n: int end) -> sig
  include Permutation.S with type i = int
end
