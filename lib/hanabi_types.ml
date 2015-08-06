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

  let min_value = 1
  let max_value = 5
  let all = [ 1; 2; 3; 4; 5 ]
  let first = 1
  let next t = t + 1

  let to_int = Fn.id
  let of_int = Fn.id
  let to_string = Int.to_string
end

module Card = struct
  type t =
    { color : Color.t
    ; number : Number.t
    }
  with sexp

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
end

module Card_id = struct
  include Int
  let to_int = Fn.id
  let of_int = Fn.id
end

module Hint = struct
  type hint =
  | Color of Color.t
  | Number of Number.t
  with sexp

  type t =
    { target: Player_id.t
    ; hint: hint
    ; hand_indices: int list
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
