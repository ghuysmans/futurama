module Make (I: sig val n: int end) = struct
  let n = I.n

  type i = int
  let int_of_i i = i

  type t = (i -> i) ref

  let make () = ref (fun i -> i)

  let (* extensionally *) equal a b =
    let rec f i =
      if i = I.n then
        true
      else if !a i = !b i then
        f (i + 1)
      else
        false
    in
    f 0

  let get f i = !f i

  let iter f t =
    for i = 0 to n - 1 do
      f i (!t i)
    done

  let update t op =
    let old = !t in (* this avoids infinite recursion... *)
    match op with
    | Permutation.Operation.Transpose (a, b) ->
      t := fun i -> old (if i = a then b else if i = b then a else i)
    | Rotate n ->
      t := fun i -> old ((i + n) mod I.n)

  let inv _ = failwith "not implemented"

  let (@) g f = ref (fun i -> !g (!f i))
end
