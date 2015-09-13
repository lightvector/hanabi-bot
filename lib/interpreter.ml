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



let interpret t ~old_state turn state =
  let { Turn. who; events } = turn in
  let update_beliefs_for_event beliefs event =
    match event with
    | Game.Turn.Draw _
    | Game.Turn.Play _
    | Game.Turn.Discard _ -> beliefs
    | Game.Turn.Hint hint ->
      let { Hint. target; hint; hand_indices } = hint in
      let hand = Map.find_exn game_state.Game.State.hands target in
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

