module Make (Op : Operation.S with type item := int) = struct
  module Op = Op

  (* beware, structural equality isn't enough anymore *)
  type t = {
    arr: int array;
    mutable ofs: int;
  }

  let make () = {
    arr = Array.init Op.order (fun i -> i);
    ofs = 0;
  }

  let copy {arr; ofs} = {
    arr = Array.copy arr;
    ofs;
  }

  let get {arr; ofs} i =
    arr.((i + ofs) mod Op.order)

  let iter f {arr; ofs} =
    (* FIXME test this *)
    Array.iteri (fun i i' -> f ((Op.order + (i - ofs) mod Op.order) mod Op.order) i') arr

  let map f t = {
    arr = Array.init Op.order (fun i -> f (get t i));
    ofs = 0;
  }

  let of_array arr = {
    arr;
    ofs = 0;
  }

  let to_array t =
    (map (fun x -> x) t).arr

  let equal t u =
    to_array t = to_array u

  let update t op =
    let set {arr; ofs} i v =
      arr.((i + ofs) mod Op.order) <- v
    in
    match op with
    | Op.Transpose (i, j) ->
      let tmp = get t i in
      set t i (get t j);
      set t j tmp
    | Rotate n ->
      t.ofs <- t.ofs + n

  let just o =
    let t = make () in
    update t o;
    t

  let inv t =
    let arr = Array.make Op.order (-1) in
    Array.iteri (fun i _ -> arr.(get t i) <- i) t.arr;
    {arr; ofs = 0}

  let (@) g f = map (get g) f
end
