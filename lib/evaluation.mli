open Core.Std
open Hanabi_types
open Game

val loss: float

val evaluate :
  state:State.t
  -> knowledge:Knowledge.t
  -> belief:Belief.t
  -> extra_hint_usefulness:float
  -> main_player:Player_id.t
  -> trace:bool
  -> float
