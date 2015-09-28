open Core.Std
open Hanabi_types
open Game

open Int.Replace_polymorphic_compare

type t = {
  pid: Player_id.t;
  params: Params.t;
  search_params: Search.Search_params.t;
  mutable knowledge: Knowledge.t;
  mutable belief: Belief.t;
  pseed: int;
  depth: int;
  trace: int * [`Eval of Action.t | `Pred of Action.t] list option
}

let create pid ~params ~state:_ ~pseed ~trace ~depth =
  let knowledge = Knowledge.empty params in
  let belief = Belief.empty params in
  let search_params = {
    Search.Search_params.
    eval_action_exp_scale = 4.0;
  }
  in
  { pid; params; search_params; knowledge; belief; pseed; depth; trace }

let update t ~old_state ~new_state ~turn =
  let old_knowledge = t.knowledge in
  let new_knowledge = Knowledge.update old_knowledge ~old_state ~new_state ~turn in
  let old_belief = t.belief in
  let new_belief = Belief.update old_belief ~old_knowledge ~new_knowledge ~old_state ~new_state turn in
  t.knowledge <- new_knowledge;
  t.belief <- new_belief

let act t state =
  let action_probs =
    Search.search_predict
      ~params:t.search_params
      ~state
      ~depth:t.depth
      ~knowledge:t.knowledge
      ~belief:t.belief
      ~extra_hint_usefulness:0.
      ~trace:(if State.turn_number state = fst t.trace then snd t.trace else None)
    |> List.sort ~cmp:(fun (_a1,p1) (_a2,p2) -> Float.compare p2 p1)
  in
  let best_action = List.hd_exn action_probs |> fst in
  best_action
