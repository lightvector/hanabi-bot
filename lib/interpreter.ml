open Core.Std
open Hanabi_types
open Game

module Tag = struct
  type t =
  (* At the time of a turn, hints or signals will be assigned exactly one of these
     qualities.
     For hints, maybe based on public information, so it cannot be mis-assigned? *)
  | Playable
  | Danger
  | Discardable
  | Hinted
  with sexp
end

module Beliefs = struct
  type t =
    { player : Player_id.t
    ; tags   : Tag.t list Card_id.Map.t
    }

end

type t = Beliefs.t Player_id.Map.t

let has_tag tag t ~player ~card_id =
  match Map.find t player with
  | None -> false
  | Some beliefs ->
    match Map.find beliefs.Beliefs.tags card_id with
    | None -> false
    | Some tags -> List.exists ~f:(fun x -> Tag.(=) x tag)

let is_playable = has_tag Tag.Playable
let is_danger = has_tag Tag.Danger
let is_discardable = has_tag Tag.Discardable
let is_hinted = has_tag Tag.Hinted

let most_discardable t player hand =
  match
    List.filter hand ~f:(fun card_id -> not (is_danger t ~player ~card_id))
  with
  (* All cards in hand are danger *)
  | [] -> List.last_exn hand
  | non_dangers ->
    match
      List.filter non_dangers ~f:(fun card_id -> is_discardable t ~player ~card_id)
      |> List.last
    with
    (* Oldest discardable *)
    | Some card_id -> card_id
    | None ->
      match
        List.filter non_dangers ~f:(fun card_id -> not (is_hinted t ~player ~card_id))
        |> List.last
      with
      (* Oldest non-hinted when no discardable cards *)
      | Some card_id -> card_id
      | None ->
        match
          List.filter non_dangers ~f:(fun card_id -> not (is_playable t ~player ~card_id))
          |> List.last
        with
        (* All non dangers are hinted, pick oldest non-playable one *)
        | Some card_id -> card_id
        | None -> List.last_exn non_dangers

let interpret t ~old_state turn state =
  let { Turn. who; events } = turn in
  let update_beliefs_for_event beliefs event =
    match event with
    | Game.Turn.Draw _
    | Game.Turn.Play _
    (* want to add some things here eventually *)
    | Game.Turn.Discard _ -> beliefs
    | Game.Turn.Hint hint ->
      let { Hint. target; hint; hand_indices } = hint in
      let hand = Map.find_exn game_state.Game.State.hands target in
      let most_discardable_card = most_discardable t target hand in

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

