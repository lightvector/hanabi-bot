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
      let pseed = 0 in
      printf "Seed: %d\n" seed;
      printf "PSeed: %d\n" seed;
      let state =
        Game.play (Params.standard ~player_count:2) ~seed ~pseed
          ~f:(fun ~old_state:_ ~new_state:_ ~turn:_ -> ())
          [ Players.base_player
          ; Players.base_player ]
      in
      printf "%s\n%!" (Sexp.to_string (Game.State.sexp_of_t state))
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
      let pseed = 0 in
      printf "Seed: %d\n" seed;
      printf "PSeed: %d\n" seed;
      let player_queue =
        String.split players ~on:','
        |> List.map ~f:(fun name ->
          match Map.find Players.all name with
          | None -> failwithf "Could not find player with name %s" name ()
          | Some player -> (name,player))
        |> Queue.of_list
      in
      let game_params = Params.standard ~player_count:(Queue.length player_queue) in
      let max_score = game_params.Params.max_score in
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
        let final_state =
          Game.play game_params ~seed ~pseed players
            ~f:(fun ~old_state:_ ~new_state ~turn ->
              if print_games
              then printf "%s  %s\n"
                (State.display_string new_state ~use_ansi_colors)
                (State.turn_display_string new_state turn ~use_ansi_colors);
            )
        in
        if print_games
        then printf "%s\n%!" (State.display_string final_state ~use_ansi_colors);

        let score = State.score final_state in
        let num_played = final_state.State.num_played in
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
