open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

let loss = 0.

(* CR stabony: arbitrary choice, perhaps not necessary.  Used to eval beginning states *)
(* let average_score = 15. *)

let logistic x ~scale =
  let x = x /. scale in
  1. /. (1. +. exp (-. x))

let evaluate ~state ~knowledge ~belief ~extra_hint_usefulness ~main_player ~trace =
  let { State. params
      ; deck
      ; bombs_left
      ; hints_left
      ; final_turns_left
      ; num_played
      ; played_cards
      ; playable_numbers
      ; handdeck_count
      ; unknown_count
      ; dead_cards
      ; discarded_cards
      ; known_cards
      ; hands
      ; rev_history
      ; cur_player
      } = state
  in
  let { Params. deck_params
      ; colors
      ; max_number
      ; initial_hints
      ; max_hints
      ; bombs_before_loss
      ; rainbow_colors
      ; rainbow_numbers
      ; hintable_colors
      ; hintable_numbers
      ; possible_hints
      ; player_count
      ; hand_size
      ; max_score
      ; max_discards
      } = params
  in
  if trace
  then printf "%s\n%!" (Sexp.to_string_hum (Game.State.sexp_of_t state));

  begin
    let playability cid =
      match State.card state cid with
      | Some card ->
        if State.is_playable state card then 1.0 else 0.0
      | None ->
        let definitely_playable =
          Knowledge.Of_card.definitely
            (Knowledge.card knowledge main_player cid)
            (Knowledge.Cond.playable state)
        in
        if definitely_playable
        then 1.0
        else begin
          let prob =
            Knowledge.Of_card.prob
              (Knowledge.card knowledge main_player cid)
              (Knowledge.Cond.playable state)
          in
          if Belief.is_probably_playable belief main_player cid
          then 0.8 +. 0.2 *. prob
          else 0.2 *. prob
        end
    in
    (* CR stabony: Need to refine this for discounting multiple equal playables,
       and also for when we get hint future plays that aren't currently playable. *)
    let good_playable_count, bad_playable_count =
      Map.fold ~init:(0., 0.) hands ~f:(fun ~key:pid ~data:hand init ->
        List.fold hand ~init ~f:(fun (good, bad) cid ->
          (* Player holding it believes it's playable *)
          if Belief.is_probably_playable belief pid cid
          then
            let playability = playability cid in
            (good +. playability, bad +. (1. -. playability))
          else
            (good, bad)
        ))
    in
    let good_played_count, bad_played_count =
      List.fold ~init:(0., 0.) played_cards ~f:(fun (good, bad) cid ->
        match State.card state cid with
        | Some card ->
          (good +. 1., bad)
        | None ->
          let playability = playability cid in
          (good +. playability, bad +. (1. -. playability))
      )
    in
    (* CR dwu: Need belief to tell us how likely these are to be dangers or kill us *)
    let unknown_discards =
      List.fold ~init:0. discarded_cards ~f:(fun count cid ->
        match State.card state cid with
        | Some card -> count (* CR dwu: Penalize for making cards dangers *)
        | None -> count +. 1.
      )
    in

    if trace
    then printf "GBC: %f, BBC: %f\n%!" good_playable_count bad_playable_count;
    if trace
    then printf "GPC: %f, BPC: %f\n%!" good_played_count bad_played_count;

    let score =
      Float.min (good_playable_count +. good_played_count)
        (float max_score)
    in
    let hint_score =
      float hints_left -. float initial_hints -. bad_playable_count -. bad_played_count
      -. float (List.length discarded_cards)
    in
    let loss_score =
      if not (State.perfect_score_possible state)
      then -20.
      else 0.
    in
    if trace
    then printf "Score: %f, Hint score %f, ExtraHintUseful %f, UnknownDiscards %f\n%!"
      score hint_score extra_hint_usefulness unknown_discards;

    logistic (score +. hint_score +. extra_hint_usefulness -. unknown_discards *. 0.5 +. loss_score) ~scale:3.0
  end
