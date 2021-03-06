open Core.Std
open Hanabi_types
open Game

(* module Tag : sig
 *   type t =
 *   (\* At the time of a turn, hints or signals will be assigned exactly one of these
 *      qualities.
 *      For hints, maybe based on public information, so it cannot be mis-assigned? *\)
 *   | Playable
 *   | Danger
 *   | Discardable
 *   | Hinted
 *   with sexp
 * end
 *
 * module Beliefs : sig
 *   type t =
 *     { player : Player_id.t
 *     ; tags   : Tag.t list Card_id.Map.t
 *     }
 * end *)

type t with sexp_of

val empty : State.t -> t

val update :
  t
  -> old_state:State.t
  -> new_state:State.t
  -> old_knowledge:Knowledge.t
  -> new_knowledge:Knowledge.t
  -> Turn.t
  -> t

val descend : t -> Player_id.t -> t

val is_probably_playable : t -> Player_id.t -> Card_id.t -> bool
val prob_of_good_hint : t -> Player_id.t -> float
