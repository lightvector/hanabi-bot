open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

let loss = 0.

(* CR stabony: arbitrary choice, perhaps not necessary.  Used to eval beginning states *)
let average_score = 15.

let evaluate ~state ~knowledge ~belief ~extra_hint_usefulness:_ =
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
  let initial_deck_length =
    List.length (Deck_params.to_deck deck_params)
    - (player_count * hand_size)
  in
  let deck_length = List.length deck in
  (* CR stabony: Need to refine this for discounting multiple equal playables,
     and also for when we get hint future plays that aren't currently playable. *)
  let good_playable_count, bad_playable_count =
    Map.fold ~init:(0, 0) hands ~f:(fun ~key:pid ~data:hand init ->
      List.fold hand ~init ~f:(fun (good, bad) cid ->
        if Belief.is_probably_playable belief pid cid
        then
          let is_probably_playable =
            Option.value_map ~default:true (State.card state cid)
              ~f:(State.is_playable state)
          in
          if is_probably_playable
          then (good + 1, bad)
          else (good, bad + 1)
        else (good, bad)))
  in
  let fraction_of_game_left =
    (hints_left + deck_length) // (initial_hints + initial_deck_length)
  in
  if bombs_left = 0
  then loss
  else if fraction_of_game_left >=. 1.
  then average_score
  else
    let score =
      max (List.length played_cards + good_playable_count)
        max_score
    in
    let hint_score = hints_left - initial_hints - bad_playable_count in
    float (score - hint_score)
    /. (1. -. fraction_of_game_left)
    -. (float (Set.length dead_cards))
