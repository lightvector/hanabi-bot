open Core.Std
open Hanabi_types

module Cond : sig
  type t

  val create: (Card.t -> bool) -> t

  val not: t -> t
  val (||): t -> t -> t
  val (&&): t -> t -> t

  val always: t
  val never: t

  val color: Color.t -> t
  val number: Number.t -> t
  val card: Card.t -> t
  val hint: Game.Params.t -> Hint.hint -> t

  val playable: Game.State.t -> t  (* card is playable right now *)
  val useless: Game.State.t -> t   (* card provably can never be playable *)
  val dangerous: Game.State.t -> t (* only one in deck and not yet played *)
end

module Of_card : sig
  type t
  with sexp_of

  val contradiction: t -> bool
  val maybe: t -> Cond.t -> bool
  val definitely: t -> Cond.t -> bool
  val prob: t -> Cond.t -> float

  val condition_on: t -> Cond.t -> t
  val condition_on_not: t -> Cond.t -> t
  val add_evidence: t -> Cond.t -> likelihood:float -> t

  val known: t -> Card.t option
  val known_color: t -> Color.t option
  val known_number: t -> Number.t option
end

(* A state of knowledge about all the cards in the game *)
module View : sig
  type t = {
    of_cards: Of_card.t Card_id.Map.t;
    unknown_count: int Card.Map.t;
  }
  with sexp_of

  val card: t -> Card_id.t -> Of_card.t

  (* Make various changes to the player's knowledge state *)
  val reveal: t -> Card_id.t -> Card.t -> t
  val inform: t -> Card_id.t -> f:(Of_card.t -> Of_card.t) -> t
  val inform_cond: t -> Card_id.t -> Cond.t ->  t
end

type t
with sexp_of

val empty: Game.Params.t -> t

val view: t -> Player_id.t -> View.t
val view2: t -> Player_id.t -> Player_id.t -> View.t

val card: t -> Player_id.t -> Card_id.t -> Of_card.t
val card2: t -> Player_id.t -> Player_id.t -> Card_id.t -> Of_card.t

(* Make changes to the global knowledge state, affecting all players at all metalevels *)
val update: t -> old_state:Game.State.t -> new_state:Game.State.t -> turn:Game.Turn.t -> t
val reveal: t -> Card_id.t -> Card.t -> t
val inform: t -> Card_id.t -> f:(Of_card.t -> Of_card.t) -> t
val inform_cond: t -> Card_id.t -> Cond.t ->  t

(* Step inside [view]'s brain *)
val descend: t -> Game.View.t -> t
