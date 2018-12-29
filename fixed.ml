type t = int array

type a =
  | Transpose of int * int
  | Rotate_right of int

let id n = Array.init n (fun i -> i)

let of_list n l =
  let permut = id n in
  let f = function
    (* {permut} becomes {permut ∘ a}. *)
    | Transpose (i, j) ->
      let tmp = permut.(i) in
      permut.(i) <- permut.(j);
      permut.(j) <- tmp
    | Rotate_right r ->
      let p' = Array.mapi (fun i _ -> permut.((i + r) mod n)) permut in
      Array.blit p' 0 permut 0 n
  in
  (* {l} describes the final composition in the reverse order *)
  List.iter f (List.rev l);
  permut

let (@) a =
  Array.map (fun i -> a.(i))

let length = Array.length
let get = Array.get
let iteri = Array.iteri

let explain a s =
  String.mapi (fun i _ -> s.[Array.get a i]) s

let transform a s =
  let s' = Bytes.make (String.length s) '\000' in
  Array.iteri (fun i i' -> Bytes.set s' i' s.[i]) a;
  Bytes.to_string s'


let disjoint l =
  let rec f = function
    | [] -> true
    | x :: y :: t when x = y -> false
    | _ :: t -> f t
  in
  f (List.flatten l |> List.sort compare)

let to_disjoint_cycles a =
  let visited = Array.make (Array.length a) false in
  let rec walk first cycle current =
    if current = first then
      List.rev cycle
    else (
      visited.(current) <- true;
      walk first (current :: cycle) a.(current)
    )
  in
  let rec f acc i =
    if i = Array.length a then
      List.rev acc
    else if visited.(i) then
      (* already in a previous cycle... *)
      f acc (i + 1)
    else
      match walk i [i] a.(i) with
      | [_] -> f acc (i + 1) (* useless *)
      | c -> f (c :: acc) (i + 1)
  in
  f [] 0

let of_disjoint_cycles n l =
  let a = id n in
  l |> List.iter (fun c ->
    let fst = List.hd c in
    (* create a cycle in the array (the accumulator becomes the last index) *)
    a.(List.fold_left (fun prev i -> a.(prev) <- i; i) fst (List.tl c)) <- fst
  );
  a
