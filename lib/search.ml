open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

module Search_params = struct
  type t = {
    eval_action_logistic_scale: float;
  } with sexp
end

let logistic x ~scale =
  let x = x /. scale in
  1. /. (1. +. exp (-. x))

let all_actions ~state ~belief:_ =
  let hand = state.State.hands.{state.State.cur_player} in
  let handlen = List.length hand in
  let plays = List.init handlen ~f:(fun i -> `Real (Action.Play i)) in
  let discards = List.init handlen ~f:(fun i -> `Real (Action.Discard i)) in

  let all_players = Player_id.all ~player_count:state.State.params.Params.player_count in
  let hints = List.concat_map all_players ~f:(fun player ->
    if Player_id.(=) player state.State.cur_player
    then []
    else if State.all_cards_in_hand_known state player
    then State.all_legal_hints_exn state player |> List.map ~f:(fun x -> `Real (Action.Hint x))
    else ((State.maybe_legal_hints state player |> List.map ~f:(fun x -> `Real (Action.Hint x)))
          @ [ `Unknown_hint_to player ])
  )
  in
  plays @ discards @ hints

let play_action ~state ~knowledge ~belief ~action =
  let handle_normal_action action =
    let new_state, turn =
      State.eval_action_exn
        ~allow_unknown_unhinted:true
        state
        action
    in
    let new_knowledge = Knowledge.update knowledge ~old:state new_state turn in
    let new_belief =
      Belief.update belief ~old_state:state ~old_knowledge:knowledge state knowledge turn
    in
    (new_state, new_knowledge, new_belief)
  in
  match action with
  | `Real action -> begin
    match action with
    | Action.Hint _ | Action.Discard _ -> handle_normal_action action
    | Action.Play i ->
      let id = State.player_card_exn state state.State.cur_player i in
      match State.card state id with
      | Some _ -> handle_normal_action action
      | None ->
        (* CR lightvector: Think about this *)
        let new_state, turn =
          State.eval_action_exn
            ~playable_if_unknown:(Belief.probably_playable belief id)
            ~allow_unknown_unhinted:true
            state
            action
        in
        let new_knowledge = Knowledge.update knowledge ~old:state new_state turn in
        let new_belief =
          Belief.update belief ~old_state:state ~old_knowledge:knowledge state knowledge turn
        in
        (new_state, new_knowledge, new_belief)
  end
  | `Unknown_hint_to player ->
    assert false (* CR lightvector: TODO *)

(* Returns a distribution over possible actions that might be taken by the player-to-move
   from the given state with that player's knowledge and beliefs *)
let rec search_predict ~params ~state ~depth ~knowledge ~belief =
  if depth <= 0
  then failwith "search_predict called with depth <= 0"
  else begin
    let actions = all_actions ~state ~belief in
    let action_evals = List.map actions ~f:(fun action ->
      let state, knowledge, belief = play_action ~state ~knowledge ~belief ~action in
      let eval = search_evaluate ~params ~state ~depth:(depth-1) ~knowledge ~belief in
      (action,eval)
    )
    in
    let max_eval =
      List.fold actions_and_evals ~init:Float.neg_infinity ~f:(fun max (_,eval) ->
        Float.max max eval
      )
    in
    let action_odds = List.map action_evals ~f:(fun (action,eval) ->
      let odds = logistic (eval -. max_eval) ~scale:params.Search_params.eval_action_logistic_scale in
      (action,eval,odds)
    )
    in
    let sum_odds =
      List.fold action_odds ~init:0. ~f:(fun sum (_,odds) ->
        sum +. odds
      )
    in
    let action_probs = List.map action_odds ~f:(fun (action,odds) ->
      (action,odds /. sum_odds)
    )
    in
    action_probs
  end

(* Returns the player's judgment of the goodness of the state and knowledge and beliefs *)
and search_evaluate ~params ~state ~depth ~knowledge ~belief =
  if depth <= 0
  then Evaluation.evaluate ~state ~knowledge ~belief ~extra_hint_usefulness
  else begin
    (* Predict what the current player will do and take the weighted sum *)
    let action_probs =
      let player = state.State.cur_player in
      let state = State.specialize state player in
      let knowledge = Knowledge.descend knowledge player in
      let belief = Belief.descend belief player in
      search_predict ~params ~state ~depth ~knowledge ~belief
    in
    List.fold action_probs ~init:0. ~f:(fun sum (action,prob) ->
      let state, knowledge, belief = play_action ~state ~knowledge ~belief ~action in
      let eval = search_evaluate ~params ~state ~depth:(depth-1) ~knowledge ~belief in
      sum +. prob *. eval
    )
  end
