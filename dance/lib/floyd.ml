type 'a t = {
  rem: int;
  length: int;
  from: 'a;
}

(* FIXME incorrect when n < length *)
let cycle init move (<=>) n =
  let rec f (tortoise, hare) rem = function
    | `Race ->
      if tortoise <=> hare then
        f (move tortoise, hare) rem (`Cycle 1)
      else
        f (move tortoise, move (move hare)) (rem - 1) `Race
    | `Cycle length ->
      if tortoise <=> hare then
        {rem; length; from = tortoise}
      else
        f (move tortoise, hare) rem (`Cycle (length + 1))
  in
  f (init, move (move init)) n `Race


(* Helper function *)

let pow (<=>) move init n =
  let {rem; length; from} = cycle init move (<=>) n in
  Algebra.Naive.pow move from (rem mod length)
