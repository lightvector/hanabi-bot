open Core.Std
open Hanabi_types
open Game

module Search_params : sig
  type t = {
    eval_action_exp_scale: float;
  } with sexp
end

val search_predict:
  params:Search_params.t
  -> state:State.t
  -> depth:int
  -> knowledge:Knowledge.t
  -> belief:Belief.t
  -> extra_hint_usefulness:float
  -> trace:[`Eval of Action.t | `Pred of Action.t] list option
  -> (Action.t * float) list
