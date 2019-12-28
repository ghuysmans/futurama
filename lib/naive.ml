module Make (Op: Aoc17_day16.S) = struct
  type t = bytes

  let equal = Bytes.equal
  let copy = Bytes.copy
  let make () = Bytes.init Op.order (fun i -> Char.chr (97 + i))

  module Op = Op

  let rec update a = function
    | Op.Spin i ->
      let l = Op.order - i in
      Bytes.(blit (cat (sub a l i) (sub a 0 l)) 0 a 0 Op.order)
    | Exchange (p, p') ->
      let t = Bytes.get a p in
      Bytes.(set a p (get a p'));
      Bytes.(set a p' t)
    | Partner (c, c') ->
      let p = Bytes.index a c in
      let p' = Bytes.index a c' in
      update a (Op.Exchange (p, p'))

  let just op =
    let t = make () in
    update t op;
    t
end
