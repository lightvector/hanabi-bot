open Core.Std
open Hanabi_types
open Game

type t with sexp_of

val empty : Params.t -> t

val update :
  t
  -> old_state:State.t
  -> new_state:State.t
  -> old_knowledge:Knowledge.t
  -> new_knowledge:Knowledge.t
  -> Turn.t
  -> t

val descend : t -> Player_id.t -> t

(* CR lightvector: Document what this does for a card not in that player's hand *)
val is_probably_playable : t -> Player_id.t -> Card_id.t -> bool
val prob_of_good_hint : t -> Player_id.t -> float
