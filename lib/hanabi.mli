open Core.Std

module Color : sig
  type t =
  | White
  | Blue
  | Yellow
  | Red
  | Green
end

module Card : sig
  type t =
    { color : Color.t
    ; number : int
    }
end

module Player_id : sig
  type t = int
end

module State : sig
  type t =
    { hint_token : int
    ; bombs : int
    ; player_cards : Card.t List.t Int.Map.t
    ; top_played_cards : Card.t List.t
    ; remaining_cards : Card.t List.t
    }
end

module Hint : sig
  type t =
  | Color of Color.t
  | Number of int
end

module Action : sig
  type t =
  | Hint of Player_id.t * Hint.t * (int List.t)
  | Discard of int
  | Play of int
  | Bomb of int
end

val standard_init_state : int -> State.t

module Player : sig
  type t =
  | Player of (State.t -> Action.t Int.Map.t -> (t * Action.t))
end

val hanabi :
  State.t
  -> Player_id.t
  -> Player.t Int.Map.t
  -> bool

val test_bot :
  int -> int -> Player.t -> int
