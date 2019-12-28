module Make (Op : Operation.S with type item := int) = struct
  module Op = Op

  type t = int array

  let make () =
    Array.init Op.order (fun i -> i)

  let copy = Array.copy

  let equal = (=)

  let get = Array.get

  let iter = Array.iteri

  let update a = function
    | Op.Transpose (i, j) ->
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    | Rotate n ->
      if n = 0 then
        ()
      else
        let tmp = Array.init Op.order (fun i -> a.((i + n) mod Op.order)) in
        Array.blit tmp 0 a 0 Op.order

  let just o =
    let a = make () in
    update a o;
    a

  let inv t =
    let arr = Array.make Op.order (-1) in
    Array.iteri (fun i v -> arr.(v) <- i) t;
    arr

  let (@) g f = Array.map (Array.get g) f
end
