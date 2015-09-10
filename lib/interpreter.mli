open Core.Std
open Hanabi_types
open Game

module Tag : sig
  type t =
  (* At the time of a turn, hints or signals will be assigned exactly one of these
     qualities.
     For hints, maybe based on public information, so it cannot be mis-assigned? *)
  | Playable
  | Danger
  | Discardable
  | Hinted
  with sexp
end

module Beliefs : sig
  type t =
    { player : Player_id.t
    ; tags   : Tag.t list Card_id.Map.t
    }
end

type t = Beliefs.t Player_id.Map.t

val interpret : t -> State.t -> Turn.t -> t
