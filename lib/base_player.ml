open Core.Std
open Hanabi_types

module Tag = struct
  type card_state =
    { played : Number.t Color.Map.t
    ; discarded : Card.t list
    } with sexp

  type t =
  | Hinted of card_state * Hint.hint * int list * int
  | Unhinted_front of card_state
  | Unhinted_back of card_state
  with sexp
end

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
            then (tags, hint_index)
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

let act state game_state =
  let new_history =
    let rec loop acc rev_history =
      match rev_history with
      | [] -> acc
      | turn :: rest ->
        if turn.Game.Turn.who = state.State.me
        then turn :: acc
        else loop (turn :: acc) rest
    in
    loop [] game_state.Game.State.rev_history
  in
  let state = List.fold ~init:state new_history ~f:(update_state game_state) in
