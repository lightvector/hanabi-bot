open Core.Std

module Color : sig
  type t =
  | White
  | Blue
  | Yellow
  | Red
  | Green
  with sexp

  val all: t list
  val to_string: t -> string
end

module Number : sig
  type t = int
  with sexp

  val all: t list
  val to_string: t -> string
end

module Card : sig
  type t =
    { color : Color.t
    ; number : int
    }
  with sexp

  val to_string: t -> string
end

module Player_id : sig
  type t = int
  with sexp
end

module Hint : sig
  type t =
  | Color of Color.t
  | Number of int
  with sexp
end

module Action : sig
  type t =
  | Hint of Player_id.t * Hint.t * (int List.t)
  | Discard of int
  | Play of int
  with sexp
end
