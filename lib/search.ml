open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

module Search_params = struct
  type t = {
    eval_action_exp_scale: float;
  } with sexp
end

type all_actions = {
  actions: Action.t list;
  good_unknown_hint_prob: float;
}

let all_actions ~state ~belief =
  let hand = state.State.hands.{state.State.cur_player} in
  let handlen = List.length hand in
  let plays = List.init handlen ~f:(fun i -> Action.Play i) in
  let discards = List.init handlen ~f:(fun i -> Action.Discard i) in

  let all_players = Player_id.all ~player_count:state.State.params.Params.player_count in
  let prob_of_no_good_unknown_hint = ref 1. in
  let hints =
    if state.State.hints_left <= 0
    then []
    else
      List.concat_map all_players ~f:(fun pid ->
        if Player_id.(=) pid state.State.cur_player
        then []
        else if State.all_cards_in_hand_known state pid
        then State.all_legal_hints_exn state pid |> List.map ~f:(fun x -> Action.Hint (Some x))
        else begin
          prob_of_no_good_unknown_hint :=
            !prob_of_no_good_unknown_hint *. (1. -. Belief.prob_of_good_hint belief pid);
          State.maybe_legal_hints state pid |> List.map ~f:(fun x -> Action.Hint (Some x))
        end
      )
  in
  let actions = plays @ discards @ hints in
  {actions;
   good_unknown_hint_prob = 1. -. !prob_of_no_good_unknown_hint;
  }

let act ~state ~knowledge ~belief ~action ~extra_hint_usefulness =
  let extra_hint_usefulness =
    if Action.(=) action (Action.Hint None)
    then extra_hint_usefulness +. 1.
    else extra_hint_usefulness
  in
  let handle_normal_action action =
    let new_state, turn =
      State.eval_action_exn
        ~allow_unknown_unhinted:true
        ~allow_unknown_hint:true
        state
        action
    in
    let new_knowledge = Knowledge.update knowledge ~old_state:state ~new_state ~turn in
    let new_belief =
      Belief.update belief ~old_state:state ~old_knowledge:knowledge
        ~new_state ~new_knowledge turn
    in
    (new_state, new_knowledge, new_belief, extra_hint_usefulness)
  in
  match action with
  | Action.Hint _ | Action.Discard _ -> handle_normal_action action
  | Action.Play i ->
    let cid = State.player_card_exn state state.State.cur_player i in
    match State.card state cid with
    | Some _ -> handle_normal_action action
    | None ->
      let new_state, turn =
        State.eval_action_exn
          (* CR lightvector: Think about this *)
          ~playable_if_unknown:(Belief.is_probably_playable belief state.State.cur_player cid)
          ~allow_unknown_unhinted:true
          ~allow_unknown_hint:true
          state
          action
      in
      let new_knowledge = Knowledge.update knowledge ~old_state:state ~new_state ~turn in
      let new_belief =
        Belief.update belief ~old_state:state ~old_knowledge:knowledge
          ~new_state ~new_knowledge turn
      in
      (new_state, new_knowledge, new_belief, extra_hint_usefulness)

let step_trace_pred trace action =
  match trace with
  | None -> None
  | Some [] -> None
  | Some (`Eval _ :: _) -> None
  | Some (`Pred hd :: tl) ->
    if Action.(=) action hd
    then Some tl
    else None

let step_trace_eval trace action =
  match trace with
  | None -> None
  | Some [] -> None
  | Some (`Pred _ :: _) -> None
  | Some (`Eval hd :: tl) ->
    if Action.(=) action hd
    then Some tl
    else None

(* Returns a distribution over possible actions that might be taken by the player-to-move
   from the given state with that player's knowledge and beliefs *)
let rec search_predict ~params ~state ~depth ~knowledge ~belief
    ~extra_hint_usefulness ~trace =
  if depth <= 0
  then failwith "search_predict called with depth <= 0"
  else begin
    let { actions; good_unknown_hint_prob } = all_actions ~state ~belief in
    let eval_action action =
      let state, knowledge, belief, extra_hint_usefulness =
        act ~state ~knowledge ~belief ~action ~extra_hint_usefulness
      in
      let eval =
        search_evaluate ~params ~state ~depth:(depth-1)
          ~knowledge ~belief ~extra_hint_usefulness
          ~main_player:state.State.cur_player
          ~trace:(step_trace_pred trace action)
      in
      begin match trace with
      | Some [] -> printf "Eval: %f  Action: %s\n%!" eval (Sexp.to_string (Action.sexp_of_t action));
      | _ -> ()
      end;
      (action,eval)
    in
    let probs action_evals =
      let max_eval =
        List.fold action_evals ~init:Float.neg_infinity ~f:(fun max (_,eval) ->
          Float.max max eval
        )
      in
      let action_odds = List.map action_evals ~f:(fun (action,eval) ->
      (* CR lightvector: We should figure out a better model here *)
        let odds = exp ((eval -. max_eval) /. params.Search_params.eval_action_exp_scale) in
        (action,eval,odds)
      )
      in
      let sum_odds =
        List.fold action_odds ~init:0. ~f:(fun sum (_,_,odds) ->
          sum +. odds
        )
      in
      List.map action_odds ~f:(fun (action,_,odds) ->
        (action, odds /. sum_odds)
      )
    in

    let action_evals = List.map actions ~f:eval_action in
    let action_probs = probs action_evals in
    if Float.(<=) good_unknown_hint_prob 0.
    then action_probs
    else begin
      let action_probs = (Action.Hint None, 0.) :: action_probs in
      let good_hint_action_eval = eval_action (Action.Hint None) in
      let action_probs_if_good_hint = probs (good_hint_action_eval :: action_evals) in
      List.map2_exn action_probs action_probs_if_good_hint
        ~f:(fun (action,prob) (_action2,prob2) ->
          (* assert (Action.(=) action action2) *)
          (action, prob *. (1. -. good_unknown_hint_prob) +. prob2 *. good_unknown_hint_prob)
        )
    end
  end

(* Returns the player's judgment of the goodness of the state and knowledge and beliefs *)
and search_evaluate ~params ~state ~depth ~knowledge ~belief
    ~extra_hint_usefulness ~main_player ~trace =
  if depth <= 0
  then begin
    let trace =
      match trace with
      | Some [] -> true
      | _ -> false
    in
    let eval =
      Evaluation.evaluate ~state ~knowledge ~belief
        ~extra_hint_usefulness ~main_player ~trace
    in
    if trace
    then printf "Eval: %f\n%!" eval;
    eval
  end
  else begin
    (* Predict what the current player will do and take the weighted sum *)
    let action_probs =
      let pid = state.State.cur_player in
      let state = State.specialize state (View.Pid pid) in
      let knowledge = Knowledge.descend knowledge (View.Pid pid) in
      let belief = Belief.descend belief pid in
      search_predict ~params ~state ~depth ~knowledge ~belief
        ~extra_hint_usefulness ~trace
    in
    List.fold action_probs ~init:0. ~f:(fun sum (action,prob) ->
      let state, knowledge, belief, extra_hint_usefulness =
        act ~state ~knowledge ~belief ~extra_hint_usefulness ~action
      in
      let eval =
        search_evaluate ~params ~state ~depth:(depth-1)
          ~knowledge ~belief ~extra_hint_usefulness
          ~main_player ~trace:(step_trace_eval trace action)
      in
      begin match trace with
      | Some [] -> printf "Eval: %f  Prob: %f  Action:%s \n%!" eval prob (Sexp.to_string (Action.sexp_of_t action));
      | _ -> ()
      end;
      sum +. prob *. eval
    )
  end
