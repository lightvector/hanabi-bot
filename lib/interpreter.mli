open Core.Std
open Hanabi_types
open Game

module Tag = struct
  type quality =
  (* At the time of a turn, hints or signals will be assigned exactly one of these
     qualities.
     For hints, maybe based on public information, so it cannot be mis-assigned? *)
  | Playable
  | Danger
  | Discardable
  (* dunno about this one *)
  | Valuable
  with sexp

  type t =
  | Hinted of quality * State.t * Player_id.t * Hint.hint * int
  (* Examples:
     Playable - Crossovers and such
     Dangerous - Say when bombing, assign to everything behind bomb
     Discardable - Most discardable card in hand,
     rightmost opponent hints playable of person beyond you when you have no play *)
  | Signaled of quality * State.t * Player_id.t
  (* Unsure if State.t, Player_id.t here is necessary *)
  | Anti_hinted of Hint.hint
  with sexp
end
