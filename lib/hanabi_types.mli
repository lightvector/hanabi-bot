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

  val all: t list

  val to_string: t -> string
end

module Number : sig
  type t
  with sexp, compare
  include Comparable.S with type t := t

  val to_int: t -> int
  val of_int: int -> t
  val to_string: t -> string

  val first: t
  val next: t -> t
  val max: t -> t -> t

  val is_first: t -> bool
  val is_after: t -> after:t -> bool
  val diff: t -> t -> int

  val all: num_numbers:int -> t list
  val between: min:t -> max:t -> t list
end

module Card : sig
  type t =
    { color : Color.t
    ; number : Number.t
    }
  with sexp, compare
  include Comparable.S with type t := t

  val (=): t -> t -> bool

  val to_string: t -> string
  val to_ansicolor_string: t -> string
end

(* An id for a player, unique per game *)
module Player_id : sig
  type t
  with sexp, compare
  include Comparable.S with type t := t

  val to_int: t -> int
  val of_int: int -> t

  val first: t
  val next: t -> player_count:int -> t
  val all: player_count:int -> t list
  val is_legal: t -> player_count:int -> bool
end

(* An id for a card, unique per game *)
module Card_id : sig
  type t
  with sexp, compare
  include Comparable.S with type t := t

  val to_int: t -> int
  val of_int: int -> t
  val all: deck: _ list -> t list
end

module Hint : sig
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

module Action : sig
  type t =
  | Hint of Hint.t option (* None used in search when a hint is given but we don't know what exactly it is *)
  | Discard of int
  | Play of int
  with sexp
  val (=): t -> t -> bool
end

(* This is syntactic sugar so that you can write:
   map.{key}
   instead of:
   Map.find_exn map key *)
module Bigarray : sig
  module Array1 : sig
    val get: ('key,'value,'cmp) Map.t -> 'key -> 'value
  end
end
