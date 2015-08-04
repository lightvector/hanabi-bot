open Core.Std


(* play one game with a known seed value with pretty output *)

  (* play a seed range and collect the stats and show a histogram *)

        let state =
        Game.play (Game.Game_params.standard ~player_count:2) ~seed
          [ Game.Player.Intf.auto_player
          ; Game.Player.Intf.auto_player ]
      in
