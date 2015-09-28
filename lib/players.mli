open Core.Std
open Hanabi_types
open Game

val all:
  trace: (int * [`Eval of Action.t | `Pred of Action.t] list option)
  -> Game.Player.Intf.wrapped String.Map.t

val always_play: Game.Player.Intf.wrapped
val base_player: Game.Player.Intf.wrapped

val search_player:
  trace: (int * [`Eval of Action.t | `Pred of Action.t] list option)
  -> Game.Player.Intf.wrapped
