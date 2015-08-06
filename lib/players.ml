open Core.Std
open Hanabi_types
open Game

let always_play =
  let create _player_id ~seed:_ = () in
  let act () _state = Action.Play 0 in
  Player.Intf.T { Player.Intf.create; act }

let all =
  ["always_play", always_play
  ] |> String.Map.of_alist_exn
