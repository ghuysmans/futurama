type i = int

type t = {
  mutable f: i -> i;
  order: int;
}

let make order = {order; f = fun i -> i}

let (* extensionally *) equal a b =
  let rec f i =
    if i = a.order then
      true
    else if a.f i = b.f i then
      f (i + 1)
    else
      false
  in
  a.order = b.order && f 0

let order {order; _} = order

let get {f; _} i = f i

let map g {f; order} = {order; f = fun i -> g (f i)}

let update t op =
  let old = t.f in (* this avoids infinite recursion... *)
  match op with
  | Group.Operation.Transpose (a, b) ->
    t.f <- fun i -> old (if i = a then b else if i = b then a else i)
  | Rotate n ->
    t.f <- fun i -> old ((i + n) mod t.order)

let inv _ = failwith "not implemented"
