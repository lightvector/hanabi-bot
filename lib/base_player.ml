open Core.Std
open Hanabi_types

module Tag = struct
  type card_state =
    { played : Number.t Color.Map.t
    ; discarded : Card.t list
    } with sexp

  type t =
  | Hinted of card_state * Hint.hint * int list * int
  (* Currently using none of the below *)
  | Anti_hinted of card_state * Hint.hint
  | Unhinted_front of card_state
  | Unhinted_back of card_state
  with sexp
end

let is_playable card_state card =
  match Map.find card_state.Tag.played card.Card.color with
  | None -> Number.of_int 1
  | Some n -> Number.of_int (n + 1)

let is_eventually_playable card_state card =
  let { Card. color; number } = card in
  let last_played = Map.find_exn card_state.Tag.played color in
  let init =
    match game_state.Game.State.game_params.Game.Params.deck_params with
    | Symmetric (map, _colors) ->
      Array.init (Map.length map) ~f:(fun i -> Map.find_exn map (Number.of_int (i + 1)))
    | _ -> assert false
  in
  Number.to_int number > last_played
  && begin
    let left_to_play =
      List.fold card_state.Tag.discarded ~init ~f:(fun left card ->
        if card.Card.color = color
        then left.[(Number.to_int card.Card.number) - 1] <-
          left.[(Number.to_int card.Card.number) - 1] - 1;
        left)
    in
    Array.foldi left_to_play ~init:true (fun i can_play count ->
      can_play
      && ((i + 1 > Number.to_int number)
          || count > 0))
  end

let is_danger card_state card =
  is_eventually_playable card_state card
  && begin
    let discards_needed =
      match game_state.Game.State.game_params.Game.Params.deck_params with
      | Symmetric (map, _colors) ->
        Map.find_exn map card.Card.number - 1
      | _ -> assert false
    in
    List.fold card_state.Tag.discarded ~init:discards_needed
      ~f:(fun discards_needed discard ->
        if card = discard
        then discards_needed - 1
        else discards_needed) = 0
  end

let _maybe_identify_card tags =
  assert false

module State = struct
  type t =
    { me : Player_id.t
    ; tags : Tag.t list Card_id.Map.t
    } with sexp
end

let was_hinted state card_id =
  match Map.find state.State.tags card_id with
  | None -> false
  | Some l -> List.exists l ~f:(fun tag ->
    match tag with
    | Tag.Hinted _ -> true
    | _ -> false)

let card_state_of_game_state game_state =
  let played =
    Map.map game_state.Game.State.played_cards ~f:(fun l ->
      Number.of_int (List.length l))
  in
  let discarded =
    List.map game_state.Game.State.discarded_cards
      ~f:(Game.State.identify_card_exn game_state)
  in
  { Tag. played; discarded }

let update_state game_state state turn =
  let { Game.Turn. who; events } = turn in
  let tags = state.State.tags in
  let assign_tags_of_event tags event =
    match event with
    | Game.Turn.Draw _ | Game.Turn.Play _ -> tags
    | Game.Turn.Discard _ ->
      let couldve_hinted =
        game_state.Game.State.hints_left > 0
      in
      if not couldve_hinted
      then tags
      else
        Map.fold game_state.Game.State.hands ~init:tags
          ~f:(fun ~key:player_id ~data:card_ids tags ->
            if who = player_id
            then tags
            else
              let card_state = card_state_of_game_state game_state in
              let front = List.hd_exn card_ids in
              let back = List.last_exn card_ids in
              let unhinted_front = not (was_hinted tags front) in
              let unhinted_back = not (was_hinted tags back) in
              let tags =
                if unhinted_front
                then Map.add_multi tags ~key:front
                  ~data:(Tag.Unhinted_front card_state)
                else tags
              in
              if unhinted_back
              then Map.add_multi tags ~key:back
                ~data:(Tag.Unhinted_back card_state)
              else tags)
    | Game.Turn.Hint hint ->
      let { Hint. target; hint; hand_indices } = hint in
      let card_state = card_state_of_game_state game_state in
      let hand = Map.find_exn game_state.Game.State.hands target in
      let tags,_ =
        List.foldi ~init:(tags,0) hand
          ~f:(fun i (tags, hint_index) card_id ->
            if not (List.mem hand_indices i)
            then Map.add_multi tags ~key:card_id
              ~data:(Tag.Anti_hinted (card_state, hint)),
              hint_index
            else Map.add_multi tags ~key:card_id
              ~data:(Tag.Hinted (card_state, hint, hand_indices, hint_index)),
              hint_index + 1)
      in
      tags
  in
  let tags = List.fold ~init:tags events ~f:assign_tags_of_event in
  { State. me; tags }

let create player_id =
  { State. me = player_id
  ; tags = Card_id.Map.empty }

let fold_over_other_players_left_to_right player_count ~me ~init ~f =
  List.init (player_count - 1) ~f:(fun i ->
    Player_id.of_int ((i + 1 + Player_id.to_int me) mod player_count))
  |> List.fold ~init ~f

let act state game_state =
  let { Game.State. game_params; hints_left; known_cards; hands; rev_history; _ } =
    game_state
  in
  let { Game.Params. deck_params; player_count; hand_size; _ } = game_params in
  let { State. tags; me } = state in
  let new_history =
    let rec loop acc rev_history =
      match rev_history with
      | [] -> acc
      | turn :: rest ->
        if turn.Game.Turn.who = me
        then turn :: acc
        else loop (turn :: acc) rest
    in
    loop [] rev_history
  in
  let state = List.fold ~init:state new_history ~f:(update_state game_state) in
  let card_state = card_state_of_game_state game_state in
  let hint_playable_opt =
    if hints_left = 0
    then None
    else
      fold_over_other_players player_count ~me ~init:None ~f:(fun hint_opt other_player ->
        if Option.is_some hint_opt
        then hint_opt
        else
          let hand = Map.find_exn hands other_player in
          List.foldi hand ~init:None ~f:(fun hand_index hint_opt card_id ->
            match hint_opt with
            | Some hint -> Some hint
            | None ->
              let card = identify_card_exn game_state card_id in
              if not (is_playable card_state card)
              then None
              else
                let already_hinted =
                  Option.value ~default:[] (Map.find tags card_id)
                  |> List.exists ~f:(function
                    | Hinted (_, _, indices, hint_index) ->
                      (hint_index = 0
                      && indices <> [ hand_size - 1 ]))
                in
                if already_hinted
                then None
                else
                  List.filter (Game.all_legal_hints game_state hand)
                    ~f:(fun (hint, indices) ->
                      List.hd_exn indices = hand_index)
                  |> List.hd
                  |> Option.map ~f:(fun (hint, hand_indices) ->
                    { Hint. target = other_player; hint; hand_indices })
          ))
  in
  match hint_playable_opt with
  | Some hint -> Action.Hint hint
  | None ->
    let hint_dangerous_opt =
      fold_over_other_players player_count ~me ~init:None ~f:(fun hint_opt other_player ->
        if Option.is_some hint_opt
        then hint_opt
        else
          let hand = Map.find_exn hands other_player in
          let last_card_id = List.last_exn hand in
          let last_card = Game.identify_card_exn game_state last_card_id in
          if not (is_dangerous last_card)
          then None
          else
            let already_hinted =
              Option.value ~default:[] (Map.find tags card_id)
              |> List.exists ~f:(function
                | Hinted (_, _, indices, hint_index) ->
                  (hint_index = 0
                  && indices = [ hand_size - 1 ]))
            in
            if alread_hinted
            then None
            else
              List.filter (Game.all_legal_hints game_state hand)
                ~f:(fun (hint, indices) ->
                  List.hd_exn indices = hand_index)
              |> List.hd
              |> Option.map ~f:(fun (hint, hand_indices) ->
                { Hint. target = other_player; hint; hand_indices })
      )
    in
    match hint_dangerous_opt with
    | Some hint -> Action.Hint hint
    | None ->
      let my_hand = Map.find_exn hands me in
      let play =
        List.fold my_hand ~init:None ~f:(fun play_opt card_id ->
          if Option.is_some play_opt
          then play_opt
          else
            match Map.find tags card_id with
            | None -> None
            | Some card_tags ->
              if List.exists card_tags ~f:(fun card_tag ->
                match card_tag with
                | Hinted (hint_card_state, hint, indices, hint_index) ->
                  hint_index = 0
                  && (indices <> [ hand_size - 1 ])
                  && (match hint with
                  | Hint.Number number ->
                    let possible_colors =
                      Map.fold hint_card_state.Tag.played ~init:[]
                        ~f:(fun acc ~key:color ~data ->
                          if Number.next data = number
                          then color :: acc
                          else acc)
                    in
                    List.exists possible_colors ~f:(fun color ->
                      match Map.find card_state.Tag.played with
                      | None -> true
                      | Some n -> Number.next n = number)
                  | Hint.Color color ->
                    Map.find hint_card_state.Tag.played color
                    = Map.find card_state.Tag.played color)
                | _ -> false)
                ||
