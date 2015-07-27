open Core.Std

let () = Random.self_init ()
;;

let sandbox_command =
  Command.basic
    ~summary:"A nice patch of sand"
    Command.Spec.(
      empty
      ++ step (fun k seed -> k ~seed)
      +> flag "-seed" (optional int) ~doc:"SEED defaults to random value"
    )
    (fun ~seed () ->
      let seed =
        match seed with
        | None -> Random.int 1000000000
        | Some seed -> seed
      in
      let state =
        Game.play (Game.Game_params.standard ~player_count:2) ~seed
          [ Game.Player.Intf.auto_player
          ; Game.Player.Intf.auto_player ]
      in
      printf "%s\n%!" (Sexp.to_string (Game.State.sexp_of_t (fun _ -> Sexp.unit) state))

        (*
 *   let state =
 *     State.create (Game_params.standard ~player_count:2)
 *     |> fun t -> State.eval_action_exn t (Action.Discard 2)
 *     |> fun (t, _) -> State.eval_action_exn t (Action.Play 4)
 *     |> fun (t, _) ->
 *       let target = Player_id.of_int 1 in
 *       let hint, hand_indices =
 *         List.hd_exn (State.all_legal_hints t (Map.find_exn t.State.hands target))
 *       in
 *       State.eval_action_exn t (Action.Hint { Hint. target; hint; hand_indices })
 *     |> fun (t, _) -> State.specialize t (Player_id.of_int 0)
 *   in
 *   printf "%s\n%!" (Sexp.to_string (State.sexp_of_t (fun _ -> Sexp.unit) state)) *)


    )
;;


let command =
  Command.group ~summary:"Hanabi!"
    [ "sandbox", sandbox_command ]

;;

Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
;;
