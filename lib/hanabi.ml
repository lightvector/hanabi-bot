open Core.Std
open Hanabi_types

module Card_info = struct
  type 'annot t =
    { id: Card_id.t
    ; card: Card.t option
    ; annot: 'annot
    }
end

module Turn = struct
  type event =
  | Hint of Hint.t
  | Discard of int * Card_id.t * Card.t
  | Play of int * Card_id.t * Card.t
  | Draw of Card_id.t * Card.t
  type t =
    { who : Player_id.t
    ; events : event list
    }
end

module Deck_params = struct
  type t =
  | Symmetric of int Number.Map.t * Color.t list
  | Explicit  of Card.t list
  with sexp

  let standard =
    let number_distribution =
      Number.Map.of_alist
	[ (Number.of_int 1, 3)
	; (Number.of_int 2, 2)
	; (Number.of_int 3, 2)
	; (Number.of_int 4, 2)
	; (Number.of_int 5, 1)
	]
    in
    Symmetric (number_distribution, Color.default_5)

  let to_deck = function
    | Explicit cards -> cards
    | Symmetric (number_dist, colors) ->
      let deck = ref [] in
      Number.Map.iter number_dist
	~f:(fun ~key ~data ->
	  let cards_to_add =
	    List.concat_map colors
	      ~f:(fun color ->
		List.init data
		  ~f:(fun _ -> { Card. color; number = key }))
	  in
	  deck := (cards_to_add @ !deck));
      !deck
end

module Game_params = struct
  type t =
    { deck_params: Deck_params.t
    ; target_numbers: Number.t Color.Map.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    ; rainbow_colors: Color.t list
    ; rainbow_numbers: Number.t list
    ; players: int
    ; hand_size: int
    }
  with sexp
end

module State = struct
  type 'annot t =
    { deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int
    ; played_cards: Card_id.t list Color.Map.t
    ; discarded_cards: Card_id.t list
    ; hands: Card_id.t list Player_id.Map.t
    ; card_infos: 'annot Card_info.t Card_id.Map.t
    ; history: Turn.t list
    }
  with sexp

  let next_player t =
    let player_count = Map.length t.hands in
    match t.history with
    | [] -> Player_id.of_int 0
    | last_turn :: _ ->
      let last_player = last_turn.who in
      (Player_id.to_int last_player + 1) mod player_count
      |> Player_id.of_int

  let turn_of_action_exn t action =
    assert is_definitely_legal_exn t action;
    let active_player = next_player t in
    let card_details_of_index i =
      let card_id = List.nth_exn (Player_id.Map.get active_player t.hands) i in
      let card_info = Card_id.Map.get t.card_infos card_id in
      let card = Option.value_exn card_info.Card_info.card in
      (i, card_id, card)
    in
    let events =
      let non_draw_event =
        match action with
        | Hint hint ->
          assert not (Player_id.(=) active_player hint.Hint.target);
          [ Hint hint ]
        | Discard i -> Discard (card_details_of_index i)
        | Play i -> Play (card_details_of_index i)
      in
      let draws =
        match action with
        | Hint hint -> []
        | Discard _ | Play _ ->
          match t.deck with
          | [] -> []
          | next_card_id :: _ ->
            let next_card =
              Option.value_exn (Card_id.Map.get t.card_infos card_id).Card_info.card
            in
            [ Draw (next_card_id, Some next_card) ]
      in
      non_draw_event :: draws
    in
    { Turn. who = active_player; events }


  let eval_turn_exn t turn =
    { t with final_turns_left = t.final_turns_left - 1; discarded

  let eval_action_exn t action =
    match action with
    | Hint hint ->
    | Discard i ->
    | Play i ->

  let create game_params =
    let { Game_params. deck_params; _target_numbers; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; players; hand_size } = game_params
    in
    let deck =
      random_permutation (Deck_params.to_deck deck_params)
    in
    let bombs_left = bombs_before_loss in
    let hints_left = initial_hints in
    let final_turns_left = players in
    let played_cards = Color.Map.create () in
    let discarded_cards = [] in
    let hands =
      let hands = Player_id.Map.create () in
      let players = List.init players ~f:Player_id.of_int in
      List.iter players ~f:(fun p -> Player_id.Map.add ~key:p ~data:[] hands)
    in
    (* continue *)
    assert false

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
