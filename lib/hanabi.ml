open Core.Std
open Hanabi_types
open Game

let () = Random.self_init ()
;;

let seedvalue seed =
  match seed with
  | None -> Random.int 1000000000
  | Some seed -> seed

let sandbox_command =
  Command.basic
    ~summary:"A nice patch of sand"
    Command.Spec.(
      empty
      ++ step (fun k seed -> k ~seed)
      +> flag "-seed" (optional int) ~doc:"SEED defaults to random value"
    )
    (fun ~seed () ->
      let seed = seedvalue seed in
      printf "Seed: %d\n" seed;
      let state =
        Game.play (Params.standard ~player_count:2) ~seed
          [ Players.always_play
          ; Players.always_play ]
      in
      printf "%s\n%!" (Sexp.to_string (Game.State.sexp_of_t state))

(* let () =
 *   let state =
 *     State.create (Params.standard ~player_count:2)
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

let hash seed i =
  if i = 0
  then seed
  else
    let digest =
      Digest.to_hex (Digest.string (sprintf "%d,%d" seed i))
    in
    Int.of_string ("0x" ^ String.sub digest ~pos:0 ~len:15)
;;

let simulate_command =
  Command.basic
    ~summary:"Run one or more games with the specified players"
    Command.Spec.(
      empty
      ++ step (fun k seed -> k ~seed)
      +> flag "-seed" (optional int) ~doc:"SEED defaults to random value"
      ++ step (fun k players -> k ~players)
      +> flag "-players" (required string) ~doc:"STRING comma-separated names of players"
      ++ step (fun k num_games -> k ~num_games)
      +> flag "-num-games" (optional_with_default 1 int) ~doc:"INT number of games, default 1"
      ++ step (fun k print_games -> k ~print_games)
      +> flag "-print-games" no_arg ~doc:" Display the detailed events of each game"
      ++ step (fun k use_ansi_colors -> k ~use_ansi_colors)
      +> flag "-ansi-colors" no_arg ~doc:" Use ansi colors to display"
    )
    (fun ~seed ~players ~num_games ~print_games ~use_ansi_colors () ->
      let seed = seedvalue seed in
      printf "Seed: %d\n" seed;
      let player_queue =
        String.split players ~on:','
        |> List.map ~f:(fun name ->
          match Map.find Players.all name with
          | None -> failwithf "Could not find player with name %s" name ()
          | Some player -> (name,player))
        |> Queue.of_list
      in
      let game_params = Params.standard ~player_count:(Queue.length player_queue) in
      let max_score = Params.max_score game_params in
      let score_freqs = Array.create ~len:(max_score+1) 0 in
      let played_freqs = Array.create ~len:(max_score+1) 0 in
      for i = 0 to num_games - 1 do
        if print_games
        then printf "\nGame %d\n" i
        else if i mod 100 = 0
        then printf "Game %d\n" i;

        let players = Queue.to_list player_queue in

        if print_games
        then begin
          List.iteri players ~f:(fun i (name,_) -> printf "P%d = %s  " i name);
          printf "\n";
        end;

        let players = List.map players ~f:snd in
        let seed = hash seed i in
        let final_state = Game.play game_params ~seed players in
        let history = List.rev final_state.State.rev_history in

        let state = ref (State.create game_params ~seed) in
        if print_games
        then begin
          List.iteri history ~f:(fun i turn ->
            (* Exclude the initial card draws that occur when the state is created fresh *)
            if i >= List.length !state.State.rev_history
            then begin
              printf "%s\n" (State.display_string !state ~use_ansi_colors);
              printf "%s\n" (Turn.to_string turn);
              state := State.eval_turn_exn !state turn
            end
          );
          printf "%s\n%!" (State.display_string !state ~use_ansi_colors);
        end;
        let score = State.score final_state in
        let num_played = State.num_played final_state in
        score_freqs.(score) <- score_freqs.(score) + 1;
        played_freqs.(num_played) <- played_freqs.(num_played) + 1;
      done;
      printf "Num games: %d\n" num_games;
      printf "Score and num_played frequency table: \n";
      Array.iteri score_freqs ~f:(fun score freq ->
        let played_freq = played_freqs.(score) in
        printf "%d: %d (%.2f%%)  %d (%.2f%%)\n"
          score
          freq (Float.of_int freq /. Float.of_int num_games *. 100.)
          played_freq (Float.of_int played_freq /. Float.of_int num_games *. 100.)
      );
      printf "\n%!";
      let sum_score = Array.foldi score_freqs ~init:0 ~f:(fun i acc x -> acc + i * x) in
      let sum_played = Array.foldi played_freqs ~init:0 ~f:(fun i acc x -> acc + i * x) in
      printf "Avg score = %.3f\n" (Float.of_int sum_score /. Float.of_int num_games);
      printf "Avg played = %.3f\n" (Float.of_int sum_played /. Float.of_int num_games);
    )



let command =
  Command.group ~summary:"Hanabi!"
    [ "sandbox", sandbox_command
    ; "simulate", simulate_command ]
;;

Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
;;
