open Core.Std
open Hanabi_types
open Int.Replace_polymorphic_compare

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
    ; rainbow_colors: Color.Set.t
    ; rainbow_numbers: Number.Set.t
    ; hintable_colors: Color.Set.t
    ; hintable_numbers: Number.Set.t
    ; possible_hints: Hint.hint list
    ; player_count: int
    ; hand_size: int
    ; max_score: int
    }
  with sexp

  let standard ~player_count =
    let hand_size =
      match player_count with
      | 2 | 3 -> 5
      | 4 | 5 -> 4
      | _ -> failwithf "no standard game params with player_count = %d" player_count ()
    in
    let hintable_colors = Color.default_5 |> Color.Set.of_list in
    let hintable_numbers = Number.all ~num_numbers:5 |> Number.Set.of_list in
    { deck_params = Deck_params.standard;
      initial_hints = 8;
      max_hints = 8;
      bombs_before_loss = 3;
      rainbow_colors = Color.Set.empty;
      rainbow_numbers = Number.Set.empty;
      hintable_colors;
      hintable_numbers;
      possible_hints =
        (List.map (Set.to_list hintable_colors) ~f:(fun c -> Hint.Color c)
         @ List.map (Set.to_list hintable_numbers) ~f:(fun n -> Hint.Number n));
      player_count;
      hand_size;
      max_score = 25;
    }

end

module State = struct
  type t =
    { params: Params.t
    ; deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int
    ; num_played: int
    ; played_cards: Card_id.t list Color.Map.t
    ; discarded_cards: Card_id.t list
    ; known_cards: Card.t Card_id.Map.t
    ; hands: Card_id.t list Player_id.Map.t
    ; rev_history: Turn.t list
    ; cur_player: Player_id.t
    }
  with sexp

  (* BASIC UTILITIES *)

  let card t card_id =
    Map.find t.known_cards card_id
  let card_exn t card_id =
    Map.find_exn t.known_cards card_id

  let is_playable_exn t card =
    let { Card. color; number } = card in
    match Map.find t.played_cards color with
    | None | Some [] -> Number.is_first number
    | Some (last_played :: _) ->
      Number.is_after number ~after:(card_exn t last_played).Card.number

  let hint_matches_card t hint card =
    match hint with
    | Hint.Number n ->
      Number.(=) card.Card.number n || Set.mem t.params.Params.rainbow_numbers card.Card.number
    | Hint.Color c ->
      Color.(=) card.Card.color c  || Set.mem t.params.Params.rainbow_colors card.Card.color

  let matching_indices t hint hand =
    List.filter_mapi hand ~f:(fun i card_id ->
      if hint_matches_card t hint (card_exn t card_id)
      then Some i
      else None
    ) |> Int.Set.of_list

  let rec for_all_i list ~f =
    let rec loop i list ~f =
      match list with
      | [] -> true
      | hd :: tl ->
        if f i hd
        then loop (i+1) tl ~f
        else false
    in
    loop 0 list ~f

  let is_legal_hint_exn t hint =
    let { Hint. target; hint; hand_indices } = hint in
    Player_id.is_legal target ~player_count:t.params.Params.player_count
    && Player_id.(<>) target t.cur_player
    && t.hints_left > 0
    && not (Set.is_empty hand_indices)
    && for_all_i (Map.find_exn t.hands target) ~f:(fun i card_id ->
      Bool.(=) (hint_matches_card t hint (card_exn t card_id)) (Set.mem hand_indices i))

  let all_legal_hints t hand =
    List.map t.params.Params.possible_hints ~f:(fun hint -> hint, matching_indices t hint hand)
    |> List.filter ~f:(fun (hint, matches) -> not (Set.is_empty matches))

  let is_definitely_legal_exn t action =
    match action with
    | Action.Hint hint -> is_legal_hint_exn t hint
    | Action.Discard i | Action.Play i ->
      0 <= i
      && i < t.params.Params.hand_size

  (* GAMEPLAY *)

  let turn_of_action_exn t action =
    if not (is_definitely_legal_exn t action)
    then failwith (
      Sexp.to_string (Action.sexp_of_t action)
      ^ "\n"
      ^ Sexp.to_string (sexp_of_t t));
    let card_details_of_index i =
      let card_id = List.nth_exn (Player_id.Map.find_exn t.hands t.cur_player) i in
      let card = card_exn t card_id in
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
          | next_card_id :: _ -> [ Turn.Draw next_card_id ]
      in
      non_draw_event :: draws
    in
    { Turn. who = t.cur_player; events }

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
          ~data:(List.filter hand ~f:(fun c -> Card_id.(<>) c card_id))
      in
      let add_to_hand hands card_id =
        let hand = Player_id.Map.find_exn hands who in
        Map.add hands ~key:who
          ~data:(card_id :: hand)
      in
      let played_cards, num_played, hands =
        Option.fold play ~init:(t.played_cards, t.num_played, t.hands)
          ~f:(fun (played_cards, num_played, hands) play ->
            let color = (card_exn t play).Card.color in
            Map.add_multi played_cards ~key:color ~data:play,
            num_played + 1,
            remove_from_hand hands play)
        |> fun init -> Option.fold discard ~init
          ~f:(fun (played_cards, num_played, hands) discard ->
            played_cards,
            num_played,
            remove_from_hand hands discard)
        |> fun init -> Option.fold draw ~init
          ~f:(fun (played_cards, num_played, hands) draw ->
            played_cards,
            num_played,
            add_to_hand hands draw)
      in
      { t with deck
        ; bombs_left = t.bombs_left - bombs_used
        ; hints_left = min (t.hints_left - hints_used) t.params.Params.max_hints
        ; num_played
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
      if List.is_empty t.deck
      then 1
      else 0
    in
    let new_t =
      List.fold turn.Turn.events ~init:t ~f:eval_event_exn
    in
    { new_t with
      final_turns_left = new_t.final_turns_left - final_turns_used;
      rev_history = turn :: t.rev_history;
      cur_player = Player_id.next t.cur_player ~player_count:t.params.Params.player_count;
    }

  let eval_action_exn t action =
    let turn = turn_of_action_exn t action in
    eval_turn_exn t turn, turn

  let rec random_permutation l ~rand =
    let arr = Array.of_list l in
    for i = 1 to Array.length arr - 1 do
      let r = Random.State.int rand (i+1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(r);
      arr.(r) <- temp;
    done;
    Array.to_list arr

  (* creates the all-known-cards initial state *)
  let create params ~seed =
    let { Params. deck_params; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; player_count; hand_size; _ } = params
    in
    let rand = Random.State.make [|seed|] in
    let cards = random_permutation (Deck_params.to_deck deck_params) ~rand in
    let deck = List.init (List.length cards) ~f:Card_id.of_int in
    let bombs_left = bombs_before_loss in
    let hints_left = initial_hints in
    let final_turns_left = player_count in
    let num_played = 0 in
    let played_cards = Color.Map.empty in
    let discarded_cards = [] in
    let players = Player_id.all ~player_count in
    let hands =
      List.map players ~f:(fun p -> (p,[]))
      |> Player_id.Map.of_alist_exn
    in
    let known_cards =
      List.zip_exn deck cards
      |> Card_id.Map.of_alist_exn
    in
    let rev_history = [] in
    let cur_player = Player_id.first in
    let init =
      { params; deck; bombs_left; hints_left; final_turns_left; num_played
      ; played_cards; discarded_cards; hands; known_cards; rev_history
      ; cur_player }
    in
    let initial_turns =
      List.init (hand_size * player_count) ~f:(fun i ->
        { Turn. who = Player_id.of_int (i mod player_count)
        ; events = [ Turn.Draw (Card_id.of_int i) ] }
      )
    in
    List.fold initial_turns ~init ~f:eval_turn_exn

  let specialize_exn t player =
    let invisible_cards =
      Map.find_exn t.hands player @ t.deck
      |> Card_id.Set.of_list
    in
    { t with known_cards = Card_id.Map.filter t.known_cards
        ~f:(fun ~key:card_id ~data:_ -> not (Set.mem invisible_cards card_id)) }

  let score t =
    if t.bombs_left = 0
    then 0
    else t.num_played

  let is_game_over t =
    t.bombs_left = 0
    || t.final_turns_left = 0

  (* MISC *)
  let display_string ?(use_ansi_colors=false) t =
    let player_ids = Map.keys t.hands |> List.sort ~cmp:Player_id.compare in
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
          if Player_id.(=) id t.cur_player
          then "*"
          else ""
        in
        sprintf "%sP%d: %s" to_play_str (Player_id.to_int id) hand_str
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

(* let () =
 *   let state =
 *     State.create (Params.standard ~player_count:2)
 *     |> fun t -> State.eval_action_exn t (Action.Discard 2)
 *     |> fun (t, _) -> State.eval_action_exn t (Action.Play 4)
 *     |> fun (t, _) ->
 *       let target = Player_id.of_int 1 in
 *       let hint, hand_indices =
 *         List.hd_exn (State.all_legal_hints t (Map.find_exn t.State.hands target))
 *       in
 *       State.eval_action_exn t (Action.Hint { Hint. target; hint; hand_indices })
 *     |> fun (t, _) -> State.specialize t (Player_id.of_int 0)
 *   in
 *   printf "%s\n%!" (Sexp.to_string (State.sexp_of_t (fun _ -> Sexp.unit) state)) *)

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

let play params players ~seed =
  assert (params.Params.player_count = List.length players);
  let players =
    List.mapi players ~f:(fun i (Player.Intf.T intf) ->
      let player_id = Player_id.of_int i in
      Player.T (player_id, intf.Player.Intf.create player_id ~seed, intf))
    |> Queue.of_list
  in
  let state = State.create params ~seed in
  let rec loop state =
    if State.is_game_over state
    then state
    else
      let player = Queue.dequeue_exn players in
      let (Player.T (player_id, player_state, intf)) = player in
      let action =
        intf.Player.Intf.act player_state (State.specialize_exn state player_id)
      in
      let state, _turn = State.eval_action_exn state action in
      Queue.enqueue players player;
      loop state
  in
  loop state

let _base_player () = assert false

(* let () =
 *   let state =
 *     play (Params.standard ~player_count:2) ~seed:123
 *       [ Player.Intf.auto_player
 *       ; Player.Intf.auto_player ]
 *   in
 *   printf "%s\n%!" (Sexp.to_string (State.sexp_of_t (fun _ -> Sexp.unit) state)) *)
(* module type Player = sig
 *   type t
 *   val update: t -> Action.t -> unit
 *   val act: t -> State.t -> Action.t
 * end *)
