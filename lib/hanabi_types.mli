open Core.Std

module Color : sig
  type t =
  | Red
  | Yellow
  | Green
  | Blue
  | White
  | Rainbow
  with sexp, compare
  include Comparable.S with type t := t

  val default_5: t list
  val rainbow_6: t list

  val to_string: t -> string
end

module Number : sig
  type t
  with sexp

  val to_int: t -> int
  val of_int: int -> t

  val first: t
  val next: t -> t

  val all: t list
  val to_string: t -> string
end

module Card : sig
  type t =
    { color : Color.t
    ; number : Number.t
    }
  with sexp

  val to_string: t -> string
end

(* An id for a player, unique per game *)
module Player_id : sig
  type t = int
  with sexp, compare
  include Comparable.S with type t := t

  val to_int: t -> int
  val of_int: int -> t
end

(* An id for a card, unique per game *)
module Card_id : sig
  type t = int
  with sexp, compare
  include Comparable.S with type t := t

  val to_int: t -> int
  val of_int: int -> t
end

module Hint : sig
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

module Action : sig
  type t =
  | Hint of Hint.t
  | Discard of int
  | Play of int
  with sexp
end

