open Core.Std
open Hanabi_types

module Annotated_card = struct
  type t =
    { id: Card_id.t
    ; card: Card.t option
    ; annot: Univ.t
    }
end

module Annotated_action = struct
  type t =
    { action: Action.t
    ; annot: Univ.t
    }
end

module GameParams = struct
  type t =
    { deck: Card.t list
    ; target_numbers: Number.t Color.Map.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    ; rainbow_colors: Color.t list
    ; rainbow_numbers: Number.t list
    }
  with sexp
end

module State = struct
  type t =
    { deck: Annotated_card.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int
    ; played_cards: Annotated_card.t list Color.Map.t
    ; discarded_cards: Annotated_card.t list
    ; current_player: Player_id.t
    ; hands: Annotated_card.t list Player_id.Map.t
    ; history: Annotated_action.t list
    }

  let next_player t =
    (Player_id.to_int t.current_player + 1) mod Map.length t.hands
    |> Player_id.of_int

  let is_legal_exn t action =
    (* CR dwu: TODO *)
    assert false

  let legal_hints t =
    (* CR dwu: TODO *)
    assert false

  let act t action =
    (* CR dwu: TODO *)
    assert false

  let act_exn t action =
    (* CR dwu: TODO *)
    assert false

  let specialize t player =
    (* CR dwu: TODO *)
    assert false

  let map_annots t ~cards ~actions =
    (* CR dwu: TODO *)
    assert false
end


module type Player = sig
  type t
  val update: t -> Action.t -> unit
  val act: t -> State.t -> Action.t
end
