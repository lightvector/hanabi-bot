open Core.Std
open Hanabi_types

module Card_info = struct
  type 'annot t =
    { id: Card_id.t
    ; card: Card.t option
    ; annot: 'annot
    } with sexp
end

module Turn = struct
  type event =
  | Hint of Hint.t
  | Discard of int * Card_id.t * Card.t
  | Play of int * Card_id.t * Card.t
  | Draw of Card_id.t * Card.t option
  with sexp
  type t =
    { who : Player_id.t
    ; events : event list
    } with sexp
end

module Deck_params = struct
  type t =
  | Symmetric of int Number.Map.t * Color.t list
  | Explicit  of Card.t list
  with sexp

  let standard =
    let number_distribution =
      Number.Map.of_alist_exn
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
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    ; rainbow_colors: Color.t list
    ; rainbow_numbers: Number.t list
    ; player_count: int
    ; hand_size: int
    }
  with sexp

  let standard ~player_count =
    let hand_size =
      match player_count with
      | 2 | 3 -> 5
      | 4 | 5 -> 4
      | _ -> failwithf "no standard game params with player_count = %d" player_count ()
    in
    { deck_params = Deck_params.standard;
      initial_hints = 8;
      max_hints = 8;
      bombs_before_loss = 3;
      rainbow_colors = [];
      rainbow_numbers = [];
      player_count;
      hand_size;
    }

end

module State = struct
  type 'annot t =
    { game_params: Game_params.t
    ; deck: Card_id.t list
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

  (* BASIC UTILITIES *)

  let next_player t =
    let player_count = Map.length t.hands in
    match t.rev_history with
    | [] -> Player_id.of_int 0
    | last_turn :: _ ->
      let last_player = last_turn.Turn.who in
      (Player_id.to_int last_player + 1) mod player_count
      |> Player_id.of_int

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
    | None | Some [] -> Number.first = number
    | Some (last_played :: _) ->
      Number.next (identify_card_exn t last_played).Card.number = number

  let hint_matches_card hint card =
    match hint with
    | Hint.Number n -> Number.(=) card.Card.number n
    | Hint.Color c -> Color.(=) card.Card.color c

  let is_legal_hint_exn t hint =
    let { Hint. target; hint; hand_indices } = hint in
    not (target = next_player t)
    && t.hints_left > 0
    && hand_indices <> []
    && match Map.find t.hands target with
    | None -> false
    | Some hand ->
      let rev_matching_indicies, _ =
        List.fold hand ~init:([], 0)
          ~f:(fun (matching_indicies, i) card_id ->
            let card = identify_card_exn t card_id in
            if hint_matches_card hint card
            then (i :: matching_indicies, (i + 1))
            else matching_indicies, (i + 1))
      in
      (List.rev rev_matching_indicies) = hand_indices

  let is_definitely_legal_exn t action =
    match action with
    | Action.Hint hint -> is_legal_hint_exn t hint
    | Action.Discard i | Action.Play i ->
      0 <= i
      && i < t.game_params.Game_params.hand_size

  let rec random_permutation l =
    Random.self_init ();
    let l_with_floats =
      List.map l ~f:(fun a -> a, Random.float 1.)
    in
    try
      List.sort l_with_floats ~cmp:(fun (_,x) (_,y) ->
        if x > y
        then 1
        else if x < y
        then -1
        else failwith "picked same floats")
      |> List.map ~f:fst
    with _ -> random_permutation l

  (* GAMEPLAY *)

  let turn_of_action_exn t action =
    assert (is_definitely_legal_exn t action);
    let active_player = next_player t in
    let card_details_of_index i =
      let card_id = List.nth_exn (Player_id.Map.find_exn t.hands active_player) i in
      let card_info = Card_id.Map.find_exn t.card_infos card_id in
      let card = Option.value_exn card_info.Card_info.card in
      (i, card_id, card)
    in
    let events =
      let non_draw_event =
        match action with
        | Action.Hint hint -> Turn.Hint hint
        | Action.Discard i ->
          let i, card_id, card = card_details_of_index i in
          Turn.Discard (i, card_id, card)
        | Action.Play i ->
          let i, card_id, card = card_details_of_index i in
          Turn.Play (i, card_id, card)
      in
      let draws =
        match action with
        | Action.Hint hint -> []
        | Action.Discard _ | Action.Play _ ->
          match t.deck with
          | [] -> []
          | next_card_id :: _ ->
            let next_card =
              Option.value_exn (Card_id.Map.find_exn t.card_infos next_card_id).Card_info.card
            in
            [ Turn.Draw (next_card_id, Some next_card) ]
      in
      non_draw_event :: draws
    in
    { Turn. who = active_player; events }

  let eval_turn_exn t turn =
    let { Turn. who; events } = turn in
    let eval_event_exn t event =
      let bombs_used, hints_used, play, discard, draw =
        match event with
        | Turn.Discard (_, card_id, _) ->
          0, (-1), None, Some card_id, None
        | Turn.Play (i, card_id, card) ->
          if is_playable_exn t card
          then 0, 0, Some card_id, None, None
          else 1, 0, None, Some card_id, None
        | Turn.Hint _ ->
          0, 1, None, None, None
        | Turn.Draw (card_id, _) ->
          0, 0, None, None, Some card_id
      in
      let deck =
        match event with
        | Turn.Draw _ -> List.tl_exn t.deck
        | _ -> t.deck
      in
      let remove_from_hand hands card_id =
        let hand = Player_id.Map.find_exn hands who in
        Map.add hands ~key:who
          ~data:(List.filter hand ~f:(fun c -> not (c = card_id)))
      in
      let add_to_hand hands card_id =
        let hand = Player_id.Map.find_exn hands who in
        Map.add hands ~key:who
          ~data:(card_id :: hand)
      in
      let played_cards, hands =
        Option.fold play ~init:(t.played_cards, t.hands)
          ~f:(fun (played_cards, hands) play ->
            let color = (identify_card_exn t play).Card.color in
            Map.add_multi played_cards ~key:color ~data:play,
            remove_from_hand hands play)
        |> fun init -> Option.fold discard ~init
          ~f:(fun (played_cards, hands) discard ->
            played_cards,
            remove_from_hand hands discard)
        |> fun init -> Option.fold draw ~init
          ~f:(fun (played_cards, hands) draw ->
            played_cards,
            add_to_hand hands draw)
      in
      { t with deck
        ; bombs_left = t.bombs_left - bombs_used
        ; hints_left = min (t.hints_left - hints_used) t.game_params.Game_params.max_hints
        ; played_cards
        ; discarded_cards = begin
          match discard with
          | None -> t.discarded_cards
          | Some card_id -> card_id :: t.discarded_cards
        end
        ; hands
      }
    in
    let final_turns_used =
      if t.deck = []
      then 1
      else 0
    in
    let new_t =
      List.fold turn.Turn.events ~init:t ~f:eval_event_exn
    in
    { new_t with
      final_turns_left = new_t.final_turns_left - final_turns_used;
      rev_history = turn :: t.rev_history }

  let eval_action_exn t action =
    let turn = turn_of_action_exn t action in
    eval_turn_exn t turn, turn

  (* creates the all-known-cards initial state *)
  let create game_params =
    let { Game_params. deck_params; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; player_count; hand_size; _ } = game_params
    in
    let cards = random_permutation (Deck_params.to_deck deck_params) in
    let deck = List.init (List.length cards) ~f:Card_id.of_int in
    let bombs_left = bombs_before_loss in
    let hints_left = initial_hints in
    let final_turns_left = player_count in
    let played_cards = Color.Map.empty in
    let discarded_cards = [] in
    let players = List.init player_count ~f:Player_id.of_int in
    let hands =
      let init = Player_id.Map.empty in
      List.fold players ~init
        ~f:(fun map p -> Player_id.Map.add map ~key:p ~data:[])
    in
    let card_infos =
      let init = Card_id.Map.empty in
      List.zip_exn deck cards
      |> List.fold ~init ~f:(fun map (id, card) ->
        let data = { Card_info. id; card = Some card; annot = () } in
        Card_id.Map.add map ~key:id ~data)
    in
    let rev_history = [] in
    let init =
      { game_params; deck; bombs_left; hints_left; final_turns_left
      ; played_cards; discarded_cards; hands; card_infos; rev_history }
    in
    let initial_turns =
      List.init (hand_size * player_count)
        ~f:(fun i ->
          { Turn. who = Player_id.of_int (i mod player_count)
          ; events = [ Turn.Draw (Card_id.of_int i, None) ] }
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

let () =
  let state, _ =
    State.create (Game_params.standard ~player_count:2)
    |> fun t -> State.eval_action_exn t (Action.Discard 2)
    |> fun (t, _) -> State.eval_action_exn t (Action.Play 4)
  in
  printf "%s\n%!" (Sexp.to_string (State.sexp_of_t (fun _ -> Sexp.unit) state))

(* module type Player = sig
 *   type t
 *   val update: t -> Action.t -> unit
 *   val act: t -> State.t -> Action.t
 * end *)
