open Core.Std
open Hanabi

module State = struct
  type t =
    { hint_token : int
    ; bombs : int
    ; player_cards : Card.t List.t Int.Map.t
    ; top_played_cards : Card.t List.t
    ; remaining_cards : Card.t List.t
    }
  with sexp
end

module Player = struct
  type t =
  | Player of (State.t -> Action.t Int.Map.t -> (t * Action.t))
end

let shuffle l =
  Random.init 42;
  List.sort l ~cmp:(fun _ _ -> if Random.bool () then 1 else -1)


let standard_deck =
  List.concat_map Color.all ~f:(fun color ->
    List.map Number.all ~f:(fun number ->
      {Card.color; number}
    )
  )

let standard_init_state n =
  let deck = shuffle standard_deck in
  let number_cards =
    if n < 5
    then 5
    else if n = 5
    then 4
    else failwith "too many players"
  in
  let draw_cards m remaining_deck =
    let rec draw_cards m remaining_deck accu =
      match m, remaining_deck with
      | 0, _ -> remaining_deck, accu
      | _, t :: q -> draw_cards (m-1) q (t :: accu)
      | _, _ -> failwith "not enough cards"
    in
    draw_cards m remaining_deck []
  in
  let remaining_cards, player_cards =
    List.init n ~f:(fun i -> i)
    |> List.fold
        ~init:(deck, Int.Map.empty)
        ~f:(fun (remaining_deck, player_cards) i ->
          draw_cards number_cards remaining_deck
          |> fun (x, y) ->
            x, Map.add player_cards ~key:i ~data:y)
  in
  { State.
    hint_token = 8
  ; bombs = 3
  ; player_cards
  ; top_played_cards = []
  ; remaining_cards
  }

let get_hint hint =
  let test =
    match hint with
    | Hint.Color color -> (fun card -> card.Card.color = color)
    | Hint.Number n -> (fun card -> card.Card.number = n)
  in
  List.filter_mapi ~f:(fun i card ->
    if test card
    then Some i
    else None
  )

let remove_card state player i =
  let player_cards =
    Map.find_exn state.State.player_cards player
  in
  let selected_card = List.nth_exn player_cards i in
  match state.State.remaining_cards with
  | [] -> failwith "game lost"
  | t :: q ->
    let new_player_cards =
      t :: List.drop player_cards i
    in
    let state =
      { state with
        State.
        remaining_cards = q
        ; player_cards =
          Map.add
            state.State.player_cards
            ~key:player
            ~data:new_player_cards
      }
    in
    state, selected_card

let do_play played_cards card = (* return [] if bomb *)
  match List.partition_tf played_cards ~f:(fun c -> c.Card.color = card.Card.color) with
  | [], q -> if card.Card.number = 1 then card :: q else []
  | [c], q ->  if card.Card.number = c.Card.number+1 then card :: q else []
  | _, _ -> failwith "multiple card with the same color"

let test_victory played_cards =
  List.length played_cards = 5
  && List.for_all played_cards ~f:(fun c -> c.Card.number = 5)

let hanabi
    state
    next_player
    players =
  let rec hanabi
      last_plays
      state
      next_player
      players =
    let state_for_player =
      { state with
        State.
        player_cards = Map.remove state.State.player_cards next_player
        ; remaining_cards = []
      }
    in
    let (Player.Player next_player_function) =
      Map.find_exn players next_player
    in
    let new_function, play =
      next_player_function
        state_for_player
        last_plays
    in
    let state, last_play =
      match play with
      | Action.Hint (player, hint, hint_list) ->
        let real_hint =
          Map.find_exn state.State.player_cards player
          |> get_hint hint
        in
        if hint_list = real_hint
        then begin
          if state.State.hint_token > 1
          then
            { state with
              State.hint_token = state.State.hint_token - 1 },
            play
          else failwith "no token left"
        end
        else failwith "incorrect hint"
      | Action.Discard i ->
        let state, _ = remove_card state next_player i in
        { state with State.hint_token = state.State.hint_token + 1 },
        play
      | Action.Play i ->
        begin
          let state, card = remove_card state next_player i in
          match do_play state.State.top_played_cards card with
          | [] ->
            state, Action.Play i
          | l ->
            { state with State.top_played_cards = l }, play
        end
    in
    if test_victory state.State.top_played_cards
    then true
    else
      if state.State.remaining_cards = []
      then false
      else
        let last_plays =
          Map.add last_plays next_player last_play
        in
        let next_player =
          match Map.find state.State.player_cards (next_player+1) with
          | None -> 0
          | _ -> next_player+1
        in
        hanabi
          last_plays
          state
          next_player
          players
  in
  hanabi
    Int.Map.empty
    state
    next_player
    players

let rec test_bot number_runs number_players player =
  match number_runs with
  | 0 -> 0
  | _ ->
    test_bot (number_runs-1) number_players player
    + if hanabi
        (standard_init_state number_players)
        0
        (List.init number_players ~f:(fun i -> i,player) |> Int.Map.of_alist_exn)
      then 1
      else 0
