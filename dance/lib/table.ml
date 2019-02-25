module Make (I: sig val n: int end) = struct
  let n = I.n

  type t = int array
  type i = int
  let int_of_i i = i

  let make () =
    Array.init I.n (fun i -> i)

  let equal = (=)

  let get = Array.get

  let iter = Array.iteri

  let update a = function
    | Permutation.Operation.Transpose (i, j) ->
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    | Rotate n ->
      let tmp = Array.init I.n (fun i -> a.((i + n) mod I.n)) in
      Array.blit tmp 0 a 0 I.n

  let inv t =
    let arr = Array.make I.n (-1) in
    Array.iteri (fun i v -> arr.(v) <- i) t;
    arr

  let to_array t = t

  let (@) g f = Array.map (Array.get g) f
end
