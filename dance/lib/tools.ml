module S = struct
  module Monoid = struct
    module type S = sig
      include Model.S
      include Algebra.Monoid.S with type t := t
    end

    let of_list
        (type t) (type o) (module M : S with type t = t and type Op.t = o)
        (l : o list) =
      let open M in
      List.rev l |> List.fold_left step id
  end

  module Group = struct
    module type S = sig
      include Model.S
      include Algebra.Group.S with type t := t
    end

    let of_list
        (type t) (type o) (module G : S with type t = t and type Op.t = o)
        (l : o list) =
      let open G in
      List.map Op.inv l |>
      List.fold_left step id |>
      inv
  end
end

module I = struct
  module Monoid = struct
    module type I = sig
      include Model.I
      include Algebra.Monoid.I with type t := t
    end

    let of_list
        (type t o) (module M : I with type t = t and type Op.t = o)
        (l : o list) =
      let open M in
      let t = make () in
      List.rev l |> List.iter (update t);
      t
  end

  module Group = struct
    module type I = sig
      include Model.I
      include Algebra.Group.I with type t := t
    end

    let of_list
        (type t o) (module G : I with type t = t and type Op.t = o)
        (l : o list) =
      let open G in
      let t = make () in
      List.map Op.inv l |>
      List.iter (update t);
      inv t
  end
end
