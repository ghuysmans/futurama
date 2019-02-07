type t = int array
type i = int

let make order =
  Array.init order (fun i -> i)

let equal = (=)

let order = Array.length

let get = Array.get

let map = Array.map

let update a = function
  | Group.Operation.Transpose (i, j) ->
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  | Rotate n ->
    let len = Array.length a in
    let tmp = Array.init len (fun i -> a.((i + n) mod len)) in
    Array.blit tmp 0 a 0 len

let inv t =
  let arr = Array.make (Array.length t) (-1) in
  Array.iteri (fun i v -> arr.(v) <- i) t;
  arr

let to_array t = t
