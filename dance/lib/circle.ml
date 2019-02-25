module Make (I: sig val n: int end) = struct
  let n = I.n

  (* beware, structural equality isn't enough anymore *)
  type t = {
    arr: int array;
    mutable ofs: int;
  }

  type i = int
  let int_of_i i = i

  let make () = {
    arr = Array.init I.n (fun i -> i);
    ofs = 0;
  }

  let get {arr; ofs} i =
    arr.((i + ofs) mod I.n)

  let iter f {arr; ofs} =
    (* FIXME test this *)
    Array.iteri (fun i i' -> f ((I.n + (i - ofs) mod I.n) mod I.n) i') arr

  let map f t = {
    arr = Array.init I.n (fun i -> f (get t i));
    ofs = 0;
  }

  let to_array t =
    (map (fun x -> x) t).arr

  let equal t u =
    to_array t = to_array u

  let set {arr; ofs} i v =
    arr.((i + ofs) mod I.n) <- v

  let update t = function
    | Permutation.Operation.Transpose (i, j) ->
      let tmp = get t i in
      set t i (get t j);
      set t j tmp
    | Rotate n ->
      t.ofs <- t.ofs + n

  let inv t =
    let arr = Array.make I.n (-1) in
    Array.iteri (fun i _ -> arr.(get t i) <- i) t.arr;
    {arr; ofs = 0}

  let (@) g f = map (get g) f
end
