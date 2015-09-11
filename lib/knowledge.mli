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

module Per_card : sig
  type t

  val maybe: t -> Cond.t -> bool
  val definitely: t -> Cond.t -> bool
  val prob: t -> Cond.t -> float

  val condition_on: t -> Cond.t -> t
  val add_evidence: t -> Cond.t -> odds:float -> t
end

type t

val create: Game.Params.t -> t
val update: t -> old:Game.State.t -> Game.State.t -> Game.Turn.t -> t

val player: t -> Player_id.t -> Per_card.t Card_id.Map.t
val card: t -> Player_id.t -> Card_id.t -> Per_card.t
