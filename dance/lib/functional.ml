module Make (Op : Operation.S with type item := int) = struct
  module Op = Op

  type t = int -> int

  let id = fun i -> i

  let (* extensionally *) equal a b =
    let rec f i =
      if i = Op.order then
        true
      else if a i = b i then
        f (i + 1)
      else
        false
    in
    f 0

  let get f i = f i

  let iter f t =
    for i = 0 to Op.order - 1 do
      f i (t i)
    done

  let just = function
    | Op.Transpose (a, b) ->
      fun i -> if i = a then b else if i = b then a else i
    | Rotate n ->
      fun i -> (i + n) mod Op.order

  let step t op =
    fun i -> t ((just op) i)

  let (@) g f = fun i -> g (f i)
end
