type 'a t = {
  by_value: ('a, int) Hashtbl.t;
  by_index: 'a array;
  fixed: Fixed.t;
}

type 'a a =
  | Transpose of 'a * 'a
  | Rotate_right of int

let index values =
  let by_index = Array.of_list values in
  let by_value = Hashtbl.create (Array.length by_index) in
  Array.iteri (fun i x -> Hashtbl.add by_value x i) by_index;
  by_index, by_value

let of_list l =
  let by_index, by_value =
    List.fold_right (fun a acc ->
      match a with
      | Transpose (x, y) -> x :: y :: acc
      | Rotate_right _ -> acc (* no new value *)
    ) l [] |>
    List.sort_uniq compare |>
    index
  in
  {
    by_index;
    by_value;
    fixed =
      let map i = Hashtbl.find by_value i in
      l |> List.map (function
        | Transpose (x, y) -> Fixed.Transpose (map x, map y)
        | Rotate_right n -> Fixed.Rotate_right n
      ) |>
      Fixed.of_list (Hashtbl.length by_value)
  }

let length {by_index; _} = Array.length by_index

let get {by_value; by_index; fixed} x =
  by_index.(Fixed.get fixed (Hashtbl.find by_value x))

let iter f {fixed; by_index; _} =
  Fixed.iteri (fun i i' -> f by_index.(i) by_index.(i')) fixed

let explain {fixed; by_value; by_index} a =
  Array.map (fun x ->
    if Hashtbl.mem by_value x then
      by_index.(Fixed.get fixed (Hashtbl.find by_value x))
    else
      x
  ) a

let transform {fixed; by_value; _} a =
  let rev = Array.make (Fixed.length fixed) (-1) in
  Array.iteri (fun i x ->
    if Hashtbl.mem by_value x then
      rev.(Hashtbl.find by_value x) <- i
  ) a;
  let a' = Array.make (Array.length a) a.(0) in
  Fixed.iteri (fun i i' -> a'.(rev.(i')) <- a.(rev.(i))) fixed;
  a'


let to_disjoint_cycles {fixed; by_index; _} =
  Fixed.to_disjoint_cycles fixed |>
  List.map (List.map (Array.get by_index))

let of_disjoint_cycles l =
  let by_index, by_value = List.flatten l |> index in
  {
    by_index;
    by_value;
    fixed =
      List.map (List.map (Hashtbl.find by_value)) l |>
      Fixed.of_disjoint_cycles (Array.length by_index)
  }
