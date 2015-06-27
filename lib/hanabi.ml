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
    ; rev_history: Turn.t list
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
        | Hint hint -> [ Hint hint ]
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

  let identify_card_exn t card_id =
    match Map.find t.card_infos card_id with
    | None -> assert false
    | Some card_info ->
      match card_info.Card_info.card with
      | None -> assert false
      | Some card -> card

  let is_playable_exn t card =
    let { Card. color; number } = card in
    match Map.find t.played_cards color with
    | [] -> Number.first = number
    | last_played :: _ ->
      Number.next (identify_card_exn last_played).Card.number = number

  let eval_turn_exn t turn =
    let { who; events } = turn in
    let eval_event_exn t event =
      let bombs_used, hints_used, play, discard, draw =
        match event with
        | Discard (_, card_id, _) ->
          0, (-1), None, Some card_id, None
        | Play (i, card_id, card) ->
          if is_playable_exn t card
          then 0, 0, Some card_id, None, None
          else 1, 0, None, Some card_id, None
        | Hint _ ->
          0, 1, None, None, None
        | Draw (card_id, _) ->
          0, 0, None, None, Some card_id
      in
      let deck =
        match turn with
        | Draw _ -> List.tl_exn t.deck
        | _ -> t.deck
      in
      let remove_from_hand card_id =
        let hand = Map.get t.hands who in
        Map.set t.hands (List.filter hand ~f:(fun c -> not (c = card_id)))
      in
      Option.iter play ~f:(fun play ->
        let color = (identify_exn t play).Card.color in
        Map.add_multi t.played_cards ~key:color ~data:play;
        remove_from_hand play);
      Option.iter discard ~f:remove_from_hand;
      { t with deck
        ; bombs_left = t.bombs_left - bombs_used
        ; hints_left = min (t.hints_left - hints_used) t.game_params.Game_params.max_hints
        ; final_turns_left = t.final_turns_left - final_turns_used
        ; discarded_cards = begin
          match discard with
          | None -> t.discarded_cards
          | Some card_id -> card_id :: t.discarded_cards
        end
        ; rev_history = turn :: t.rev_history
      }
    in
    let final_turns_used =
      if t.deck = []
      then 1
      else 0
    in
    let new_t =
      List.fold turn.events ~init:t ~f:(fun eval_event_exn t)
    in
    { new_t with final_turns_left = new_t.final_turns_left - final_turns_used }

  let eval_action_exn t action =
    let turn = turn_of_action_exn t action in
    eval_turn_exn t turn

  (* creates the all-known-cards initial state *)
  let create game_params =
    let { Game_params. deck_params; _target_numbers; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; player_count; hand_size } = game_params
    in
    let cards = random_permutation (Deck_params.to_deck deck_params) in
    let deck = List.init (List.length cards) ~f:Card_id.of_int in
    let bombs_left = bombs_before_loss in
    let hints_left = initial_hints in
    let final_turns_left = player_count in
    let played_cards = Color.Map.create () in
    let discarded_cards = [] in
    let hands = Player_id.Map.create () in
    let players = List.init player_count ~f:Player_id.of_int in
    List.iter players ~f:(fun p -> Player_id.Map.add hands ~key:p ~data:[]);
    let card_infos = Card_id.Map.create () in
    List.zip_exn deck cards
    |> List.iter ~f:(fun (id, card) ->
      let data = { id; card; annot = () } in
      Card_id.Map.add card_infos ~key:id ~data);
    let rev_history = [] in
    let init =
      { State. game_params; deck; bombs_left; hints_left; final_turns_left
      ; played_cards; discarded_cards; hands; card_infos; rev_history }
    in
    let initial_turns =
      List.init (hand_size * player_count)
        ~f:(fun i ->
          { who = Player_id.of_int (i mod player_count)
          ; events = [ Draw (Card_id.of_int i, None) ] }
        )
    in
    List.fold initial_turns ~init ~f:eval_turn_exn

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
