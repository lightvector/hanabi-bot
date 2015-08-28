open Core.Std
open Hanabi_types

module Turn = struct
  type event =
  | Hint of Hint.t
  | Discard of int * Card_id.t * Card.t
  | Play of int * Card_id.t * Card.t
  | Draw of Card_id.t
  with sexp
  type t =
    { who : Player_id.t
    ; events : event list
    } with sexp

  let to_string t = Sexp.to_string (sexp_of_t t)
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

module Params = struct
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

  let max_score t =
    let deck = Deck_params.to_deck t.deck_params in
    List.fold Color.all ~init:0 ~f:(fun sum color ->
      let max =
        List.fold deck ~init:0 ~f:(fun max card ->
          if card.Card.color = color
          then Int.max max card.Card.number
          else max
        )
      in
      sum + max
    )

end

module State = struct
  type t =
    { game_params: Params.t
    ; deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int
    ; played_cards: Card_id.t list Color.Map.t
    ; discarded_cards: Card_id.t list
    ; known_cards: Card.t Card_id.Map.t
    ; hands: Card_id.t list Player_id.Map.t
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
    Map.find_exn t.known_cards card_id

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

  let matching_indices t hint hand =
    List.fold hand ~init:([], 0)
      ~f:(fun (matching_indices, i) card_id ->
        let card = identify_card_exn t card_id in
        if hint_matches_card hint card
        then (i :: matching_indices, (i + 1))
        else matching_indices, (i + 1))
    |> fst
    |> List.rev

  let is_legal_hint_exn t hint =
    let { Hint. target; hint; hand_indices } = hint in
    not (target = next_player t)
    && t.hints_left > 0
    && hand_indices <> []
    && match Map.find t.hands target with
    | None -> false
    | Some hand ->
      matching_indices t hint hand = hand_indices

  let all_legal_hints t hand =
    (List.map Color.default_5 ~f:(fun c -> Hint.Color c)
     @ List.map Number.all ~f:(fun n -> Hint.Number n))
    |> List.map ~f:(fun hint -> hint, matching_indices t hint hand)
    |> List.filter ~f:(fun (hint, matches) -> matches <> [])

  let is_definitely_legal_exn t action =
    match action with
    | Action.Hint hint -> is_legal_hint_exn t hint
    | Action.Discard i | Action.Play i ->
      0 <= i
      && i < t.game_params.Params.hand_size

  let rec random_permutation l ~rand =
    let l_with_floats =
      List.map l ~f:(fun a -> a, Random.State.float rand 1.)
    in
    try
      List.sort l_with_floats ~cmp:(fun (_,x) (_,y) ->
        if x > y
        then 1
        else if x < y
        then -1
        else failwith "picked same floats")
      |> List.map ~f:fst
    with _ -> random_permutation l ~rand

  (* GAMEPLAY *)

  let turn_of_action_exn t action =
    assert (is_definitely_legal_exn t action);
    let active_player = next_player t in
    let card_details_of_index i =
      let card_id = List.nth_exn (Player_id.Map.find_exn t.hands active_player) i in
      let card = identify_card_exn t card_id in
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
            [ Turn.Draw next_card_id ]
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
        | Turn.Draw card_id ->
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
        ; hints_left = min (t.hints_left - hints_used) t.game_params.Params.max_hints
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
  let create game_params ~seed =
    let { Params. deck_params; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; player_count; hand_size; _ } = game_params
    in
    let rand = Random.State.make [|seed|] in
    let cards = random_permutation (Deck_params.to_deck deck_params) ~rand in
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
    let known_cards =
      List.zip_exn deck cards
      |> Card_id.Map.of_alist_exn
    in
    let rev_history = [] in
    let init =
      { game_params; deck; bombs_left; hints_left; final_turns_left
      ; played_cards; discarded_cards; hands; known_cards; rev_history }
    in
    let initial_turns =
      List.init (hand_size * player_count)
        ~f:(fun i ->
          { Turn. who = Player_id.of_int (i mod player_count)
          ; events = [ Turn.Draw (Card_id.of_int i) ] }
        )
    in
    List.fold initial_turns ~init ~f:eval_turn_exn

  let specialize t player =
    let invisible_cards =
      (Option.value ~default:[] (Map.find t.hands player))
      @ t.deck
    in
    let is_invisible card_id = List.mem invisible_cards card_id in
    { t with known_cards = Card_id.Map.filter t.known_cards
        ~f:(fun ~key:card_id ~data:_ -> not (is_invisible card_id)) }



  let num_played t =
    List.fold (List.map ~f:snd (Color.Map.to_alist t.played_cards)) ~init:0
      ~f:(fun acc plays -> acc + List.length plays)
  let score t =
    if t.bombs_left = 0
    then 0
    else num_played t


  let is_game_over t =
    t.bombs_left = 0
    || t.final_turns_left = 0

  (* MISC *)
  let display_string ?(use_ansi_colors=false) t =
    let player_ids = Map.keys t.hands |> List.sort ~cmp:Player_id.compare in
    let next_player = next_player t in
    let hand_str =
      List.map player_ids ~f:(fun id ->
        let hand = Player_id.Map.find_exn t.hands id in
        let hand_str =
          List.map hand ~f:(fun card_id ->
            match Map.find t.known_cards card_id with
            | None -> "?"
            | Some card ->
              if use_ansi_colors
              then Card.to_ansicolor_string card
              else Card.to_string card
          )
          |> String.concat ~sep:""
        in
        let to_play_str =
          if next_player = id
          then "*"
          else ""
        in
        sprintf "%sP%d: %s" to_play_str id hand_str
      )
      |> String.concat ~sep:" "
    in
    let played_str =
      List.map (Map.data t.played_cards) ~f:(fun cards ->
        let cards = List.map cards ~f:(fun id -> Map.find_exn t.known_cards id) in
        match List.reduce cards ~f:(fun x y ->
          if Number.(>) x.Card.number y.Card.number then x else y)
        with
        | None -> ""
        | Some card ->
          if use_ansi_colors
          then Card.to_ansicolor_string card
          else Card.to_string card
      )
      |> String.concat ~sep:" "
    in
    sprintf "Hintsleft %d Bombsleft %d Played: %s  %s"
      t.hints_left t.bombs_left played_str hand_str
end

module Player = struct
  module Intf = struct
    type 'a t =
      { create : (Player_id.t -> seed:int -> 'a)
      ; act : ('a -> State.t -> Action.t)
      }

    type wrapped = T:'a t -> wrapped
  end

  type 'a t = Player_id.t * 'a * 'a Intf.t

  type wrapped = T:'a t -> wrapped
end

let play game_params players ~seed =
  assert (game_params.Params.player_count = List.length players);
  let players =
    List.mapi players ~f:(fun i (Player.Intf.T intf) ->
      let player_id = Player_id.of_int i in
      Player.T (player_id, intf.Player.Intf.create player_id ~seed, intf))
    |> Queue.of_list
  in
  let state = State.create game_params ~seed in
  let rec loop state =
    if State.is_game_over state
    then state
    else
      let player = Queue.dequeue_exn players in
      let (Player.T (player_id, player_state, intf)) = player in
      let action =
        intf.Player.Intf.act player_state (State.specialize state player_id)
      in
      let state, _turn = State.eval_action_exn state action in
      Queue.enqueue players player;
      loop state
  in
  loop state
