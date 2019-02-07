(* beware, structural equality isn't enough anymore *)
type t = {
  arr: int array;
  mutable ofs: int;
}

type i = int

let make order = {
  arr = Array.init order (fun i -> i);
  ofs = 0;
}

let get {arr; ofs} i =
  arr.((i + ofs) mod Array.length arr)

let map f t = {
  arr = Array.init (Array.length t.arr) (fun i -> f (get t i));
  ofs = 0;
}

let to_array t =
  (map (fun x -> x) t).arr

let equal t u =
  to_array t = to_array u

let order {arr; _} = Array.length arr

let set {arr; ofs} i v =
  arr.((i + ofs) mod Array.length arr) <- v

let update t = function
  | Group.Operation.Transpose (i, j) ->
    let tmp = get t i in
    set t i (get t j);
    set t j tmp
  | Rotate n ->
    t.ofs <- t.ofs + n

let inv t =
  let arr = Array.make (Array.length t.arr) (-1) in
  Array.iteri (fun i _ -> arr.(get t i) <- i) t.arr;
  {arr; ofs = 0}
