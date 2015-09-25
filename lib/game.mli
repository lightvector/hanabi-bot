open Core.Std
open Hanabi_types

module Turn : sig
  type event =
    | Hint of Hint.t option
    | Discard of int * Card_id.t (* int = hand idx *)
    | Play of int * Card_id.t * bool (* int = hand_idx, bool = was_playable *)
    | Draw of Card_id.t
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
module Params : sig
  type t = private
    (* we could remove this and use number_distribution below if we want all colors to have the same distribution *)
    { deck_params: Deck_params.t
    ; colors : Color.Set.t
    ; max_number: Number.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    ; rainbow_colors: Color.Set.t (* Colors that must be hinted as every color *)
    ; rainbow_numbers: Number.Set.t (* Numbers that must be hinted as every number *)
    ; hintable_colors: Color.Set.t
    ; hintable_numbers: Number.Set.t
    ; possible_hints: Hint.hint list
    ; player_count: int
    ; hand_size: int
    ; max_score: int
    ; max_discards: int
    }
  with sexp

  val standard: player_count:int -> t

  val hint_matches_card: t -> Hint.hint -> Card.t -> bool
end

module View : sig
  type t =
  | Common
  | Pid of Player_id.t
  with sexp, compare
  include Comparable.S with type t := t
end

(* An instance of the game state.
   Capable of representing game states that are globally known, as well as game states
   as seen by one player (or seen by one player as envisioned by another), based on
   whether the [card] field in the various [Annotated_card.t] are Some or None. *)
(* XCR stabony: should this contain Params.t?
   lightvector: Sure. Why not? *)
module State : sig
  type t =
    { params: Params.t
    ; deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int (* The # of turns left in the game when the deck is empty *)
    ; num_played: int
    ; played_cards: Card_id.t list
    ; playable_numbers: Number.t Color.Map.t   (* keys have full domain *)
    ; handdeck_count: int Card.Map.t           (* keys have full domain *)
    ; unknown_count: int Card.Map.t            (* keys have full domain *)
    ; dead_cards: Card.Set.t
    ; discarded_cards: Card_id.t list
    ; known_cards: Card.t Card_id.Map.t
    ; hands: Card_id.t list Player_id.Map.t    (* keys have full domain *)
    ; rev_history: Turn.t list
    ; cur_player: Player_id.t
    } with sexp

  val create : Params.t -> seed:int -> t

  (* State updating -------------------------------------------------- *)

  (* Does not check legality, just plays the effect of the turn. Raises an exception
     if the turn is invalid (should never raise for turns produced by [eval_action_exn] *)
  val eval_turn_exn : t -> Turn.t -> t
  (* Raises exception if illegal.
     [playable_if_unknown] - if specified, playing an unknown card is allowed, but doing so
     won't update, handdeck_count, dead_cards].
     [allow_unknown_unhinted] - if specified, allows hints of hands that contain unknown
     cards as long as all hinted cards are known
     [allow_unknown_hint] - if specified, allow [Action.Hint None]
  *)
  val eval_action_exn :
    ?playable_if_unknown:bool
    -> ?allow_unknown_unhinted:bool
    -> ?allow_unknown_hint:bool
    -> t
    -> Action.t
    -> t * Turn.t

  (* Set the given card as the known card for this id. Fails if this card id is already
     known or if the setting is impossible given the counts of unknown cards *)
  val reveal_exn : t -> Card_id.t -> Card.t -> t

  (* Hides all the cards that the specified player can't see
     The common view hides all cards that at least one player can't see *)
  val specialize : t -> View.t -> t

  (* Utility functions -------------------------------------------------- *)

  (* Known card lookup *)
  val card : t -> Card_id.t -> Card.t option
  val card_exn : t -> Card_id.t -> Card.t

  (* Card in a given spot in player's hand *)
  val player_card_exn : t -> Player_id.t -> int -> Card_id.t

  (* Current game stats *)
  val score : t -> int
  val discards_left : t -> int (* discards left for perfect score *)

  (* Higher-level card properties *)
  val is_playable : t -> Card.t -> bool
  val is_useless: t -> Card.t -> bool (* provably nonplayable *)
  val is_dangerous: t -> Card.t -> bool (* useful and one card left in deck or hand *)

  val all_legal_hints_of_hand_exn : t -> Card_id.t list -> target:Player_id.t -> Hint.t list (* exn if hand unknown *)
  val all_legal_hints_exn : t -> Player_id.t -> Hint.t list (* exn if hand unknown *)
  val all_cards_in_hand_known : t -> Player_id.t -> bool

  (* no exn if hand unknown if hand unknown, just assumes unknown cards won't interfere *)
  val maybe_legal_hints : t -> Player_id.t -> Hint.t list

  val display_string : ?use_ansi_colors:bool -> t -> string
  val turn_display_string : ?use_ansi_colors:bool -> t -> Turn.t -> string

end

module Player : sig
  module Intf : sig
    type 'a t =
      { create : (Player_id.t -> params:Params.t -> seed:int -> 'a)
      ; act : ('a -> State.t -> Action.t)
      }

    type wrapped = T:'a t -> wrapped
  end

  type 'a t = Player_id.t * 'a * 'a Intf.t

  type wrapped = T:'a t -> wrapped
end

val play :
  Params.t
  -> Player.Intf.wrapped list
  -> seed:int
  -> f:(old:State.t -> State.t -> Turn.t -> unit)
  -> State.t
