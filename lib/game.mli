open Core.Std
open Hanabi

module State : sig
  type t =
    { hint_token : int
    ; bombs : int
    ; player_cards : Card.t List.t Int.Map.t
    ; top_played_cards : Card.t List.t
    ; remaining_cards : Card.t List.t
    }
  with sexp
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
