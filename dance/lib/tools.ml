module S = struct
  module type Monoid = sig
    include Model.S
    include Algebra.Monoid.S with type t := t
  end

  let of_list_by_reversing
      (type t) (type o) (module M : Monoid with type t = t and type Op.t = o)
      (l : o list) =
    let open M in
    List.rev l |> List.fold_left step id

  module type Group = sig
    include Model.S
    include Algebra.Group.S with type t := t
  end

  let of_list_by_inverting_twice
      (type t) (type o) (module G : Group with type t = t and type Op.t = o)
      (l : o list) =
    let open G in
    List.map Op.inv l |>
    List.fold_left step id |>
    inv
end

module I = struct
  module type Monoid = sig
    include Model.I
    include Algebra.Monoid.I with type t := t
  end

  let of_list_by_reversing
      (type t o) (module M : Monoid with type t = t and type Op.t = o)
      (l : o list) =
    let open M in
    let t = make () in
    List.rev l |> List.iter (update t);
    t

  module type Group = sig
    include Model.I
    include Algebra.Group.I with type t := t
  end

  let of_list_by_inverting_twice
      (type t o) (module G : Group with type t = t and type Op.t = o)
      (l : o list) =
    let open G in
    let t = make () in
    List.map Op.inv l |>
    List.iter (update t);
    inv t
end
