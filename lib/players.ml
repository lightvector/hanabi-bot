open Core.Std
open Hanabi_types
open Game

let always_play =
  let create _player_id ~params:_ ~state:_ ~pseed:_ = () in
  let update () ~old_state:_ ~new_state:_ ~turn:_ = () in
  let act () _state = Action.Play 0 in
  Player.Intf.T { Player.Intf.create; update; act }

let base_player =
  let create = Base_player.create in
  let update = Base_player.update in
  let act = Base_player.act in
  Player.Intf.T { Player.Intf.create; update; act }

let search_player =
  let create = Search_player.create in
  let update = Search_player.update in
  let act = Search_player.act in
  Player.Intf.T { Player.Intf.create; update; act }

let all =
  ["always_play", always_play
  ;"base_player", base_player
  ;"search_player", search_player
  ] |> String.Map.of_alist_exn
