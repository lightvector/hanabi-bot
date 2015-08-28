open Core.Std
open Hanabi_types

module Tag = struct
  type card_state =
    { played : Number.t Color.Map.t
    ; discarded : Card.t list
    } with sexp

  type t =
  | Identified of Card.t
  | Hinted of card_state * Hint.hint * Int.Set.t * int
  (* Currently using none of the below *)
  | Anti_hinted of card_state * Hint.hint
  | Unhinted_front of card_state
  | Unhinted_back of card_state
  with sexp
end

let card_state_of_game_state game_state =
  let played =
    Map.map game_state.Game.State.played_cards ~f:(fun l ->
      Number.of_int (List.length l))
  in
  let discarded =
    List.map game_state.Game.State.discarded_cards
      ~f:(Game.State.card_exn game_state)
  in
  { Tag. played; discarded }

let playable_number_of_color card_state color =
  match Map.find card_state.Tag.played color with
  | None -> Number.of_int 1
  | Some n -> Number.next n

let is_playable card_state card =
  card.Card.number = playable_number_of_color card_state card.Card.color

let is_eventually_playable game_state card =
  let card_state = card_state_of_game_state game_state in
  let { Card. color; number } = card in
  let last_played = Map.find_exn card_state.Tag.played color in
  let init =
    match game_state.Game.State.params.Game.Params.deck_params with
    | Game.Deck_params.Symmetric (map, _colors) ->
      Array.init (Map.length map) ~f:(fun i -> Map.find_exn map (Number.of_int (i + 1)))
    | _ -> assert false
  in
  Number.(>) number last_played
  && begin
    let left_to_play =
      List.fold card_state.Tag.discarded ~init ~f:(fun left card ->
        if card.Card.color = color
        then left.((Number.to_int card.Card.number) - 1) <-
          left.((Number.to_int card.Card.number) - 1) - 1;
        left)
      |> Array.to_list
    in
    List.foldi left_to_play ~init:true ~f:(fun i can_play count ->
      can_play
      && ((i + 1 > Number.to_int number)
          || count > 0))
  end

let is_danger game_state card =
  let card_state = card_state_of_game_state game_state in
  is_eventually_playable game_state card
  && begin
    let discards_needed =
      match game_state.Game.State.params.Game.Params.deck_params with
      | Game.Deck_params.Symmetric (map, _colors) ->
        Map.find_exn map card.Card.number - 1
      | _ -> assert false
    in
    List.fold card_state.Tag.discarded ~init:discards_needed
      ~f:(fun discards_needed discard ->
        if card = discard
        then discards_needed - 1
        else discards_needed) = 0
  end

let is_discardable card_state card =
  not (is_eventually_playable card_state card)

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

let update_state game_state state turn =
  let { Game.Turn. who; events } = turn in
  let { State. me; tags } = state in
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
              let unhinted_front = not (was_hinted state front) in
              let unhinted_back = not (was_hinted state back) in
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
            if not (Set.mem hand_indices i)
            then Map.add_multi tags ~key:card_id
              ~data:(Tag.Anti_hinted (card_state, hint)),
              hint_index
            else
              let identified_as =
                Option.bind (Map.find tags card_id) (fun card_tags ->
                  match hint with
                  | Hint.Number number ->
                    List.find_map card_tags ~f:(fun tag ->
                      match tag with
                      | Tag.Hinted (_, Hint.Color color, _, _) ->
                        Some { Card. color; number }
                      | _ -> None)
                  | Hint.Color color ->
                    List.find_map card_tags ~f:(fun tag ->
                      match tag with
                      | Tag.Hinted (_, Hint.Number number, _, _) ->
                        Some { Card. color; number }
                      | _ -> None))
              in
              let tags =
                match identified_as with
                | None -> tags
                | Some card ->
                  Map.add_multi tags ~key:card_id ~data:(Tag.Identified card)
              in
              Map.add_multi tags ~key:card_id
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

let find_new_history me rev_history =
  let rec loop acc rev_history =
    match rev_history with
    | [] -> acc
    | turn :: rest ->
      if turn.Game.Turn.who = me
      then turn :: acc
      else loop (turn :: acc) rest
  in
  loop [] rev_history

let first_some_from_other_players ~player_count ~me ~f =
  let rec loop player =
    match f player with
    | None ->
      if Player_id.(=) player me
      then None
      else loop (Player_id.next ~player_count player)
    | Some x -> Some x
  in
  loop (Player_id.next ~player_count me)

let act state game_state =
  let { Game.State. params; hints_left; known_cards; hands; rev_history; _ } =
    game_state
  in
  let { Game.Params. deck_params; player_count; hand_size; _ } = params in
  let new_history = find_new_history state.State.me rev_history in
  let state = List.fold ~init:state new_history ~f:(update_state game_state) in
  let { State. tags; me } = state in
  let card_state = card_state_of_game_state game_state in
  let hint_playable_opt =
    if hints_left = 0
    then None
    else
      first_some_from_other_players ~player_count ~me ~f:(fun other_player ->
        let hand = Map.find_exn hands other_player in
        List.foldi hand ~init:None ~f:(fun hand_index hint_opt card_id ->
          match hint_opt with
          | Some hint -> Some hint
          | None ->
            let card = Game.State.card_exn game_state card_id in
            if not (is_playable card_state card)
            then None
            else
              let already_hinted =
                Option.value ~default:[] (Map.find tags card_id)
                |> List.exists ~f:(function
                  | Tag.Hinted (_, _, indices, hint_index) ->
                    (hint_index = 0
                    && indices <> Int.Set.singleton (hand_size - 1))
                  | _ -> false)
              in
              if already_hinted
              then None
              else
                List.filter (Game.State.all_legal_hints game_state hand)
                  ~f:(fun (hint, indices) ->
                    Int.Set.min_elt_exn indices = hand_index)
                |> List.hd
                |> Option.map ~f:(fun (hint, hand_indices) ->
                  { Hint. target = other_player; hint; hand_indices })
        ))
  in
  match hint_playable_opt with
  | Some hint -> Action.Hint hint
  | None ->
    let hint_dangerous_opt =
      first_some_from_other_players ~player_count ~me ~f:(fun other_player ->
        let hand = Map.find_exn hands other_player in
        let last_card_id = List.last_exn hand in
        let last_card = Game.State.card_exn game_state last_card_id in
        if not (is_danger game_state last_card)
        then None
        else
          let already_hinted =
            Option.value ~default:[] (Map.find tags last_card_id)
            |> List.exists ~f:(function
              | Tag.Hinted (_, _, indices, hint_index) ->
                (hint_index = 0
                && indices = Int.Set.singleton (hand_size - 1))
              | _ -> false)
          in
          if already_hinted
          then None
          else
            List.filter (Game.State.all_legal_hints game_state hand)
              ~f:(fun (hint, indices) ->
                Int.Set.min_elt_exn indices = (hand_size - 1))
            |> List.hd
            |> Option.map ~f:(fun (hint, hand_indices) ->
              { Hint. target = other_player; hint; hand_indices })
      )
    in
    match hint_dangerous_opt with
    | Some hint -> Action.Hint hint
    | None ->
      let my_hand = Map.find_exn hands me in
      let identified_play =
        List.foldi my_hand ~init:None ~f:(fun i play_opt card_id ->
          if Option.is_some play_opt
          then play_opt
          else
            match Map.find tags card_id with
            | None -> None
            | Some card_tags ->
              if List.exists card_tags ~f:(fun card_tag ->
                match card_tag with
                | Tag.Identified card -> is_playable card_state card
                | _ -> false)
              then Some i
              else None)
      in
      match identified_play with
      | Some i -> Action.Play i
      | None ->
        let hinted_play =
          List.foldi my_hand ~init:None ~f:(fun i play_opt card_id ->
            if Option.is_some play_opt
            then play_opt
            else
              match Map.find tags card_id with
              | None -> None
              | Some card_tags ->
                if List.exists card_tags ~f:(function
                | Tag.Identified _ -> true
                | _ -> false)
                then None
                else if List.exists card_tags ~f:(fun card_tag ->
                  match card_tag with
                  | Tag.Hinted (hint_card_state, hint, indices, hint_index) ->
                    hint_index = 0
                    && (indices <> Int.Set.singleton (hand_size - 1))
                    && (match hint with
                    | Hint.Number number ->
                      let impossible_colors =
                        List.filter_map card_tags ~f:(fun card_tag ->
                          match card_tag with
                          | Tag.Anti_hinted (_, hint) -> begin
                            match hint with
                            | Hint.Color color -> Some color
                            | _ -> None
                          end
                          | _ -> None)
                      in
                      let possible_colors =
                        Map.fold hint_card_state.Tag.played ~init:[]
                          ~f:(fun ~key:color ~data acc ->
                            if Number.next data = number
                            then color :: acc
                            else acc)
                        |> List.filter ~f:(fun c -> not (List.mem impossible_colors c))
                      in
                      List.exists possible_colors ~f:(fun color ->
                        match Map.find card_state.Tag.played color with
                        | None -> true
                        | Some n -> Number.next n = number)
                    | Hint.Color color ->
                      let implied_number =
                        playable_number_of_color hint_card_state color
                      in
                      (implied_number = playable_number_of_color card_state color)
                      && not (List.exists card_tags ~f:(function
                      | Tag.Anti_hinted (_, hint) -> begin
                        match hint with
                        | Hint.Number implied_number -> true
                        | _ -> false
                      end
                      | _ -> false))
                    )
                  | _ -> false)
                then Some i
                else None)
        in
        match hinted_play with
        | Some i -> Action.Play i
        | None ->
          let identified_discard =
            List.foldi my_hand ~init:None ~f:(fun i discard_opt card_id ->
              if Option.is_some discard_opt
              then discard_opt
              else match Map.find tags card_id with
              | None -> None
              | Some card_tags ->
                if List.exists card_tags ~f:(function
                | Tag.Identified card -> is_discardable game_state card
                | _ -> false)
                then Some i
                else None)
          in
          match identified_discard with
          | Some i -> Action.Discard i
          | None ->
            let unhinted_discard =
              List.foldi my_hand ~init:None ~f:(fun i discard_opt card_id ->
                match Map.find tags card_id with
                | None -> Some i
                | Some card_tags ->
                  if List.exists card_tags ~f:(function
                  | Tag.Hinted _ -> true
                  | _ -> false)
                  then discard_opt
                  else Some i)
            in
            match unhinted_discard with
            | Some i -> Action.Discard i
            | None ->
              let last_card = List.last_exn my_hand in
              match Map.find tags last_card with
              | None -> Action.Discard (hand_size - 1)
              | Some card_tags ->
                if List.exists card_tags ~f:(function
                | Tag.Hinted (_, _, l, 0) -> l = Int.Set.singleton (hand_size - 1)
                | _ -> false)
                then Action.Discard (hand_size - 2)
                else Action.Discard (hand_size - 1)
