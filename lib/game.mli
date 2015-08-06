open Core.Std
open Hanabi_types

(* A card that may or may not be visible, with an arbitrary annotation on it to record
   information about that card. *)
module Card_info : sig
  type 'annot t =
    { id: Card_id.t
(* Some implies the card is visible *)
    ; card: Card.t option
    ; annot: 'annot
    }
end

module Turn : sig
  type event =
    | Hint of Hint.t
    | Discard of int * Card_id.t * Card.t
    | Play of int * Card_id.t * Card.t
    | Draw of Card_id.t * Card.t option
  with sexp

  type t =
      { who : Player_id.t
      ; events : event list
      }
  with sexp

  val to_string: t -> string
end

module Deck_params : sig
  type t =
    | Symmetric of int Number.Map.t * Color.t list
    | Explicit  of Card.t list

  val standard: t

  val to_deck: t -> Card.t list

end

(* Represents the parameters for a given game.
   Some of these fields are redundant, but convenient *)
module Game_params : sig
  type t =
    (* we could remove this and use number_distribution below if we want all colors to have the same distribution *)
    { deck_params: Deck_params.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    (* Colors that must be hinted as every color *)
    ; rainbow_colors: Color.t list
    (* Numbers that must be hinted as every number *)
    ; rainbow_numbers: Number.t list
    ; player_count: int
    ; hand_size: int
    }
  with sexp

  val standard: player_count:int -> t
  val max_score: t -> int
end

(* An instance of the game state.
   Capable of representing game states that are globally known, as well as game states
   as seen by one player (or seen by one player as envisioned by another), based on
   whether the [card] field in the various [Annotated_card.t] are Some or None. *)
(* CR stabony: should this contain Game_params.t? *)
module State : sig
  type 'annot t =
    { game_params: Game_params.t
    ; deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    (* The number of turns left in the game when the deck is empty *)
    ; final_turns_left: int
    ; played_cards: Card_id.t list Color.Map.t
    ; discarded_cards: Card_id.t list
    ; hands: Card_id.t list Player_id.Map.t
    ; card_infos: 'annot Card_info.t Card_id.Map.t
    ; rev_history: Turn.t list
    } with sexp_of

  val create : Game_params.t -> seed:int -> unit t

  val eval_turn_exn : 'a t -> Turn.t -> 'a t
  val eval_action_exn : 'a t -> Action.t -> 'a t * Turn.t

  val next_player: 'a t -> Player_id.t
  val score : 'a t -> int
  val num_played : 'a t -> int

  val display_string :
    ?use_ansi_colors:bool -> 'a t -> string

  (* True if an action is definitely legal. Fails if any cards hinted are unknown. *)
  (* val is_definitely_legal_exn: 'a t -> Action.t -> bool
   *
   * (\* Return all definitely-legal hints *\)
   * val legal_hints: 'a t -> Hint.t list
   *
   * (\* Perform an action *\)
   * val act: 'a t -> Action.t -> 'a t Or_error.t
   * val act_exn: 'a t -> Action.t -> 'a t
   *
   * (\* Return a [t] where all cards not visible to the specified player are hidden *\)
   * val specialize: 'a t -> Player_id.t -> 'a t
   * (\* Apply the given map function to all annotations on all cards and actions *\)
   * val map_annots: t -> cards:(Univ.t -> Univ.t) -> actions:(Univ.t -> Univ.t) -> t *)

end

module Player : sig
  module Intf : sig
    type 'a t =
      { create : (Player_id.t -> seed:int -> 'a)
      ; act : ('a -> unit State.t -> Action.t)
      }

    type wrapped = T:'a t -> wrapped
  end

  type 'a t = Player_id.t * 'a * 'a Intf.t

  type wrapped = T:'a t -> wrapped
end

val play : Game_params.t -> Player.Intf.wrapped list -> seed:int -> unit State.t
