open Core.Std

type color =
    White 
  | Blue
  | Yellow
  | Red
  | Green

type card = {
  color : color
; number : int
}

type player_name = int

type state = {
  hint_token : int
; bombs : int
; player_cards : card List.t Int.Map.t
; top_played_cards : card List.t
; remaining_cards : card List.t
} 

type hint =
    Color of color
  | Number of int

type play = 
    Hint of player_name * hint * (int List.t)
  | Discard of int
  | Play of int
  | Bomb of int

val standard_init_state : int -> state

type player =
    Player of (state -> play Int.Map.t -> (player * play))

val hanabi :
  state
  -> player_name
  -> (player Int.Map.t)
  -> bool

val test_bot :
  int -> int -> player -> int
