type t = {
  arr: char array;
  ofs: int;
}

let make from to_ =
  let ofs = Char.code from in
  {
    ofs;
    arr = Array.init (Char.code to_ - Char.code from + 1) (fun i ->
      Char.chr (i + ofs)
    );
  }

let get {arr; ofs} i =
  arr.(Char.code i - ofs)
