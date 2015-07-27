open Core.Std
open Hanabi_types

(* A card that may or may not be visible, with an arbitrary annotation on it to record
   information about that card. *)
module Annotated_card : sig
  type t =
    { id: Card_id.t
    ; card: Card.t option
    ; annot: Univ.t
    }
end

(* An action with an arbitrary annotation on it to record information about that action *)
module Annotated_action : sig
  type t =
    { action: Action.t
    ; annot: Univ.t
    }
end

(* Represents the parameters for a given game.
   Some of these fields are redundant, but convenient *)
module GameParams : sig
  type t =
    { deck: Card.t list
    (* The highest card in each color *)
    ; target_numbers: Number.t Color.Map.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    (* Colors that must be hinted as every color *)
    ; rainbow_colors: Color.t list
    (* Numbers that must be hinted as every number *)
    ; rainbow_numbers: Number.t list
    }
  with sexp
end

(* An instance of the game state.
   Capable of representing game states that are globally known, as well as game states
   as seen by one player (or seen by one player as envisioned by another), based on
   whether the [card] field in the various [Annotated_card.t] are Some or None. *)
module State : sig
  type t =
    { deck: Annotated_card.t list
    ; bombs_left: int
    ; hints_left: int
    (* The number of turns left in the game when the deck is empty *)
    ; final_turns_left: int
    ; played_cards: Annotated_card.t list Color.Map.t
    ; discarded_cards: Annotated_card.t list
    ; current_player: Player_id.t
    ; hands: Annotated_card.t list Player_id.Map.t
    ; history: Annotated_action.t list
    }

  (* True if an action is definitely legal. Fails if any cards hinted are unknown. *)
  val is_legal_exn: t -> Action.t -> bool

  (* Return all definitely-legal hints *)
  val legal_hints: t -> Hint.t list

  (* Perform an action *)
  val act: t -> Action.t -> t Or_error.t
  val act_exn: t -> Action.t -> t

  (* Return a [t] where all cards not visible to the specified player are hidden *)
  val specialize: t -> Player_id.t -> t
  (* Apply the given map function to all annotations on all cards and actions *)
  val map_annots: t -> cards:(Univ.t -> Univ.t) -> actions:(Univ.t -> Univ.t) -> t

end


module type Player = sig
  type t
  val update: t -> Action.t -> unit
  val act: t -> State.t -> Action.t
end
