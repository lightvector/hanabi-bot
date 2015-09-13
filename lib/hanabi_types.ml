open Core.Std
(* open Core_extended.Std *)

module Color = struct
  module T = struct
    type t =
    | Red
    | Yellow
    | Green
    | Blue
    | White
    | Rainbow
    with sexp, compare
  end
  include T
  include Comparable.Make(T)

  let default_5 = [ Red; Yellow; Green; Blue; White ]
  let rainbow_6 = [ Red; Yellow; Green; Blue; White; Rainbow ]
  let all = rainbow_6

  let to_string t =
    match t with
    | Red -> "R"
    | Yellow -> "Y"
    | Green -> "G"
    | Blue -> "B"
    | White -> "W"
    | Rainbow -> "S"
end

module Number = struct
  include Int
  let to_int = Fn.id
  let of_int = Fn.id
  let to_string = Int.to_string
  let min_value = 1
  let max_value = 5

  let first = 1
  let next t = t + 1
  let max = Int.max

  let is_first t = t = 1
  let is_after t ~after = t = after + 1
  let diff t0 t1 = t0 - t1

  let all ~num_numbers = List.init num_numbers ~f:(fun x -> x + 1)
  let between ~min ~max = if min > max then [] else List.init (max-min+1) ~f:(fun x -> x + min)
end

module Card = struct
  module T = struct
    type t =
      { color : Color.t
      ; number : Number.t
      }
    with sexp, compare
  end
  include T
  include Comparable.Make(T)

  let (=) a b =
    Color.(=) a.color b.color
    && Number.(=) a.number b.number

  let to_string t =
    Color.to_string t.color ^ Number.to_string t.number

  let to_ansicolor_string t =
    let s = Number.to_string t.number in
    let code =
      match t.color with
      | Color.Red -> "\x1b[31m"
      | Color.Green -> "\x1b[32m"
      | Color.Blue -> "\x1b[34m"
      | Color.Yellow -> "\x1b[33m"
      | Color.White -> ""
      | Color.Rainbow -> "\x1b[35m"
    in
    code ^ s ^ "\x1b[0m"
end

module Player_id = struct
  include Int
  let to_int = Fn.id
  let of_int = Fn.id

  let first = 0
  let next t ~player_count = (t + 1) mod player_count
  let all ~player_count = List.init player_count ~f:Fn.id
  let is_legal t ~player_count = t >= 0 && t < player_count
end

module Card_id = struct
  include Int
  let to_int = Fn.id
  let of_int = Fn.id
  let all ~deck = List.mapi deck ~f:(fun i _ -> i)
end

module Hint = struct
  type hint =
  | Color of Color.t
  | Number of Number.t
  with sexp

  type t =
    { target: Player_id.t
    ; hint: hint
    ; hand_indices: Int.Set.t
    }
  with sexp
end

module Action = struct
  type t =
  | Hint of Hint.t
  | Discard of int
  | Play of int
  with sexp
end
