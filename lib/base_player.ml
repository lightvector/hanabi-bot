open Core.Std
open Hanabi_types

open Int.Replace_polymorphic_compare

module Tag = struct
  type card_state =
    { playable : Number.t Color.Map.t
    ; discarded : Card.t list
    } with sexp

  type t =
  | Identified of Card.t
  | Hinted of card_state * Hint.hint * Int.Set.t * int
  | Anti_hinted of card_state * Hint.hint
  (* Currently using none of the below *)
  | Unhinted_front of card_state
  | Unhinted_back of card_state
  with sexp
end

let card_state_of_game_state game_state =
  let playable = game_state.Game.State.playable_numbers in
  let discarded =
    List.map game_state.Game.State.discarded_cards
      ~f:(Game.State.card_exn game_state)
  in
  { Tag. playable; discarded }

let playable_number_of_color card_state color =
  Map.find_exn card_state.Tag.playable color

let is_playable card_state card =
  Number.(=) card.Card.number (playable_number_of_color card_state card.Card.color)

let is_eventually_playable game_state card =
  let card_state = card_state_of_game_state game_state in
  let { Card. color; number } = card in
  let playable = Map.find card_state.Tag.playable color in
  let init =
    match game_state.Game.State.params.Game.Params.deck_params with
    | Game.Deck_params.Symmetric (map, _colors) ->
      Array.init (Map.length map) ~f:(fun i ->
        Map.find_exn map (Number.of_int (i + 1)))
    | _ -> assert false
  in
  (Option.value_map ~default:true playable ~f:(Number.(>=) number))
  && begin
    let left_to_play =
      List.fold card_state.Tag.discarded ~init ~f:(fun left card ->
        if Color.(=) card.Card.color color
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
        if Card.(=) card discard
        then discards_needed - 1
        else discards_needed) = 0
  end

let is_discardable card_state card =
  not (is_eventually_playable card_state card)

let _maybe_identify_card tags =
  assert false

module T = struct
  type t =
    { me : Player_id.t
    ; mutable tags : Tag.t list Card_id.Map.t
    } with sexp
end

let was_hinted t card_id =
  match Map.find t.T.tags card_id with
  | None -> false
  | Some l -> List.exists l ~f:(fun tag ->
    match tag with
    | Tag.Hinted _ -> true
    | _ -> false)

let update t ~old_state ~new_state ~turn =
  let { Game.Turn. who; events } = turn in
  let { T. me; tags } = t in

  let assign_tags_of_event tags event =
    match event with
    | Game.Turn.Draw _ | Game.Turn.Play _ -> tags
    | Game.Turn.Discard _ ->
      let couldve_hinted =
        old_state.Game.State.hints_left > 0
      in
      if not couldve_hinted
      then tags
      else
        Map.fold old_state.Game.State.hands ~init:tags
          ~f:(fun ~key:player_id ~data:card_ids tags ->
            if Player_id.(=) who player_id
            then tags
            else
              let card_state = card_state_of_game_state new_state in
              let front = List.hd_exn card_ids in
              let back = List.last_exn card_ids in
              let unhinted_front = not (was_hinted t front) in
              let unhinted_back = not (was_hinted t back) in
              let tags =
                if unhinted_front
                then
                  Map.add_multi tags ~key:front
                    ~data:(Tag.Unhinted_front card_state)
                else tags
              in
              if unhinted_back
              then
                Map.add_multi tags ~key:back
                  ~data:(Tag.Unhinted_back card_state)
              else tags)
    | Game.Turn.Hint None ->
      failwith "base player received unknown hint"
    | Game.Turn.Hint (Some { Hint. target; hint; hand_indices }) ->
      let card_state = card_state_of_game_state new_state in
      let hand = Map.find_exn old_state.Game.State.hands target in
      let tags,_ =
        List.foldi ~init:(tags,0) hand
          ~f:(fun hand_index (tags, hint_index) card_id ->
            if not (Set.mem hand_indices hand_index)
            then
              Map.add_multi tags ~key:card_id
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
  t.T.tags <- tags

let create player_id ~params:_ ~state:_ ~pseed:_ =
  { T. me = player_id
  ; tags = Card_id.Map.empty
  }

let first_some_from_other_players ~player_count ~me ~f =
  let rec loop player =
    if Player_id.(=) player me
    then None
    else
      match f player with
      | None ->
        loop (Player_id.next ~player_count player)
      | Some x -> Some x
  in
  loop (Player_id.next ~player_count me)

let tag_is_hinted_as_danger tag ~hand_size =
  match tag with
  | Tag.Hinted (_, _, indices, _) ->
    Int.Set.min_elt_exn indices = hand_size - 1
  | _ -> false

let tag_is_hinted_as_playable tag ~hand_size =
  match tag with
  | Tag.Hinted (_, _, indices, hint_index) ->
    not (Int.Set.min_elt_exn indices = hand_size - 1)
    && hint_index = 0
  | _ -> false

let tag_is_identified tag =
  match tag with
  | Tag.Identified _ -> true
  | _ -> false

let find_hint_of_playable t game_state other_player =
  let card_state = card_state_of_game_state game_state in
  let tags = t.T.tags in
  let hand_size = game_state.Game.State.params.Game.Params.hand_size in
  let hand = Map.find_exn game_state.Game.State.hands other_player in
  List.foldi hand ~init:None ~f:(fun hand_index hint_opt card_id ->
    match hint_opt with
    | Some hint -> Some hint
    | None ->
      let card = Game.State.card_exn game_state card_id in
      if not (is_playable card_state card)
      then None
      else
        let already_hinted_as_playable_or_identified =
          let card_tags = Option.value ~default:[] (Map.find tags card_id) in
          List.exists card_tags ~f:(fun tag ->
            tag_is_hinted_as_playable ~hand_size tag
            || tag_is_identified tag)
          || (List.count card_tags ~f:(tag_is_hinted_as_danger ~hand_size)) > 1
        in
        if already_hinted_as_playable_or_identified
        then None
        else
          List.filter (Game.State.all_legal_hints_of_hand_exn game_state hand ~target:other_player)
            ~f:(fun hint ->Int.Set.min_elt_exn hint.Hint.hand_indices = hand_index)
          |> List.hd
  )

let find_hint_of_danger t game_state other_player =
  let tags = t.T.tags in
  let hand_size = game_state.Game.State.params.Game.Params.hand_size in
  let hand = Map.find_exn game_state.Game.State.hands other_player in
  let last_card_id = List.last_exn hand in
  let last_card = Game.State.card_exn game_state last_card_id in
  if not (is_danger game_state last_card)
  then None
  else
    let already_hinted =
      Option.value ~default:[] (Map.find tags last_card_id)
      |> List.exists ~f:(tag_is_hinted_as_danger ~hand_size)
    in
    if already_hinted
    then None
    else
      List.filter (Game.State.all_legal_hints_of_hand_exn game_state hand ~target:other_player)
        ~f:(fun hint -> Int.Set.min_elt_exn hint.Hint.hand_indices = hand_size - 1)
      |> List.hd

let find_identified_play t game_state ~my_hand =
  let card_state = card_state_of_game_state game_state in
  let { T. tags; _ } = t in
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

let find_mapi list ~f =
  let rec loop i l =
    match l with
    | [] -> None
    | hd :: tl ->
      match f i hd with
      | Some y -> Some y
      | None -> loop (i + 1) tl
  in
  loop 0 list

let find_hinted_play t game_state ~my_hand =
  let card_state = card_state_of_game_state game_state in
  let { T. tags; _ } = t in
  let hand_size = game_state.Game.State.params.Game.Params.hand_size in
  find_mapi my_hand ~f:(fun i card_id ->
    match Map.find tags card_id with
    | None -> None
    | Some card_tags ->
        if List.exists card_tags ~f:(function
        (* these have already been considered in 'identified' code above *)
        | Tag.Identified _ -> true
        | _ -> false)
        then None
        else if List.count card_tags ~f:(tag_is_hinted_as_danger ~hand_size) > 1
            || begin
              List.exists card_tags ~f:(fun card_tag ->
                match card_tag with
                | Tag.Hinted (hint_card_state, hint, indices, hint_index) ->
                  tag_is_hinted_as_playable card_tag ~hand_size
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
                      Map.fold hint_card_state.Tag.playable ~init:[]
                        ~f:(fun ~key:color ~data acc ->
                          if Number.(=) data number
                          then color :: acc
                          else acc)
                      |> List.filter ~f:(fun c -> not (List.mem impossible_colors c))
                    in
                    List.exists possible_colors ~f:(fun color ->
                      Number.(=) (Map.find_exn card_state.Tag.playable color) number)
                  | Hint.Color color ->
                    let implied_number =
                      playable_number_of_color hint_card_state color
                    in
                    Number.(=) implied_number (playable_number_of_color card_state color)
                    && not (List.exists card_tags ~f:(function
                    | Tag.Anti_hinted (_, hint) -> begin
                      match hint with
                      | Hint.Number implied_number -> true
                      | _ -> false
                    end
                    | _ -> false))
                  )
                | _ -> false)
            end
              then Some i
              else None)

let (>>>) x y = Option.first_some x y

let act t game_state =
  let { Game.State. params; hints_left; known_cards; hands; rev_history; _ } =
    game_state
  in
  let { Game.Params. deck_params; player_count; hand_size; _ } = params in
  let { T. tags; me; _ } = t in
  begin
    begin
      let hint_playable_opt =
        if hints_left = 0
        then None
        else first_some_from_other_players ~player_count ~me
          ~f:(find_hint_of_playable t game_state)
      in
      Option.map hint_playable_opt ~f:(fun hint -> Action.Hint (Some hint))
    end
    >>>
      begin
        let hint_dangerous_opt =
          if hints_left = 0
          then None
          else
            first_some_from_other_players ~player_count ~me
              ~f:(find_hint_of_danger t game_state)
        in
        Option.map hint_dangerous_opt ~f:(fun hint -> Action.Hint (Some hint))
      end
    >>>
      let my_hand = Map.find_exn hands me in
      begin
        find_identified_play t game_state ~my_hand
        |> Option.map ~f:(fun index -> Action.Play index)
      end
    >>>
        begin
          find_hinted_play t game_state ~my_hand
          |> Option.map ~f:(fun index -> Action.Play index)
        end
    >>>
        begin
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
          Option.map identified_discard ~f:(fun index -> Action.Discard index)
        end
    >>>
        begin
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
          Option.map unhinted_discard ~f:(fun index -> Action.Discard index)
        end
    >>>
        begin
          let last_card = List.last_exn my_hand in
          match Map.find tags last_card with
          | None -> Some (Action.Discard (hand_size - 1))
          | Some card_tags ->
            if List.exists card_tags ~f:(tag_is_hinted_as_danger ~hand_size)
            then Some (Action.Discard (hand_size - 2))
            else Some (Action.Discard (hand_size - 1))
        end
  end |> fun action_opt ->  Option.value_exn action_opt
