open Core.Std

module Color = struct
  type t =
  | White
  | Blue
  | Yellow
  | Red
  | Green
  with sexp

  let all = [ White; Blue; Yellow; Red; Green ]

  let to_string t =
    match t with
    | White -> "W"
    | Blue -> "B"
    | Yellow -> "Y"
    | Red -> "R"
    | Green -> "G"
end

module Number = struct
  type t = int
  with sexp

  let all = [ 1; 2; 3; 4; 5 ]
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
end

module Hint = struct
  type t =
  | Color of Color.t
  | Number of Number.t
  with sexp
end

module Player_id = struct
  type t = int
  with sexp
end

module Action = struct
  type t =
  | Hint of Player_id.t * Hint.t * (int List.t)
  | Discard of int
  | Play of int
  with sexp
end
