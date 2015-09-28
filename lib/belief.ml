open Core.Std
open Hanabi_types
open Game

module K = Knowledge

module Hint_info = struct
  type t = {
    cids : Card_id.t list;
    hint : Hint.t;
    giver : Player_id.t;
  } with sexp
end

module Tag = struct
  type t =
  (* At the time of a turn, hints or signals will be assigned exactly one of these
     qualities.
     For hints, maybe based on public information, so it cannot be mis-assigned? *)
  (* int is hint index *)
  | Hinted of Hint_info.t * int * [ `was_most_discardable | `ok ]
  with sexp

  let is_hinted t =
    match t with
    | Hinted _ -> true
end

module Per_player = struct
  type t =
    { view : View.t
    ; tags : Tag.t list Card_id.Map.t
    } with sexp_of

  let empty view = { view; tags = Card_id.Map.empty }

  let add_tag t ~cid ~tag =
    { t with
      tags = Map.add_multi t.tags ~key:cid ~data:tag
    }

  let has_tag t ~f ~cid =
    match Map.find t.tags cid with
    | None -> false
    | Some tags -> List.exists ~f tags

  let is_hinted = has_tag ~f:Tag.is_hinted
  let is_probably_playable = has_tag ~f:(function
    | Tag.Hinted (_hint_info, hint_index, `ok) ->
      hint_index = 0
    | _ -> false)
  let is_danger = has_tag ~f:(function
    | Tag.Hinted (_hint_info, _hint_index, `was_most_discardable) -> true
    | _ -> false)

  (* Most discardable from the perspective of the knowledge view passed in (the giver of a hint) *)
  let most_discardable_for_interpreting_hint t ~kview ~old_state hand =
    match
      List.filter hand ~f:(fun cid -> not (is_danger t ~cid))
    with
    (* All cards in hand are danger *)
    (* CR wartysoybean: check knowledge, the last card we don't DEFINITIVELY know will
       kill us. *)
    | [] -> List.last_exn hand
    | non_dangers ->
      match
        List.filter non_dangers ~f:(fun cid ->
          K.Of_card.definitely (K.View.card kview cid) (K.Cond.useless old_state))
        |> List.last
      with
      (* Oldest discardable *)
      | Some cid -> cid
      | None ->
        match
          List.filter non_dangers ~f:(fun cid -> not (is_hinted t ~cid))
          |> List.last
        with
        (* Oldest non-hinted when no discardable cards *)
        | Some cid -> cid
        | None ->
          match
            List.filter non_dangers ~f:(fun cid -> not (is_probably_playable t ~cid))
            |> List.last
          with
          (* All non dangers are hinted, pick oldest non-playable one *)
          | Some cid -> cid
          | None -> List.last_exn non_dangers

  let update t ~old_state ~new_state ~old_knowledge ~new_knowledge turn =
    let { Turn. who; events } = turn in
    let update_beliefs_for_event beliefs event =
      match event with
      | Game.Turn.Draw _
      | Game.Turn.Play _
    (* want to add some things here eventually *)
      | Game.Turn.Discard _ -> beliefs
      | Game.Turn.Hint None -> beliefs
      | Game.Turn.Hint (Some hint) ->
        let { Hint. target; hint=_; hand_indices } = hint in
        let view_to_update =
          match t.view with
          | View.Common -> true
          | View.Pid pid -> Player_id.(=) target pid
        in
        if not view_to_update
        then beliefs
        else
          let hand = old_state.Game.State.hands.{target} in
          let most_discardable_card =
            let kview = K.view old_knowledge who in
            most_discardable_for_interpreting_hint t ~kview ~old_state hand
          in
          let tags,_ =
            let hint_info =
              let cids = List.filteri hand ~f:(fun i _cid -> Set.mem hand_indices i) in
              { Hint_info. cids; hint; giver = who }
            in
            List.foldi ~init:(t.tags,0) hand
              ~f:(fun hand_index (tags, hint_index) cid ->
                if not (Set.mem hand_indices hand_index)
                then tags,hint_index
                else
                  let data =
                    let extra =
                      if Card_id.(=) cid most_discardable_card
                      then `was_most_discardable
                      else `ok
                    in
                    Tag.Hinted (hint_info, hint_index, extra)
                  in
                  Map.add_multi tags ~key:cid ~data,
                  hint_index + 1)
          in
          { beliefs with tags }
    in
    List.fold events ~f:update_beliefs_for_event ~init:t

end

type t = Per_player.t View.Map.t with sexp_of

let empty params =
  let views =
    Player_id.all ~player_count:params.Params.player_count
    |> List.map ~f:(fun pid -> View.Pid pid)
    |> fun l -> View.Common :: l
  in
  List.map views ~f:(fun view -> view, Per_player.empty view)
  |> View.Map.of_alist_exn

let update t ~old_state ~new_state ~old_knowledge ~new_knowledge turn =
  Map.mapi t ~f:(fun ~key:view ~data:per_player ->
    let new_state = State.specialize new_state view in
    let old_state = State.specialize old_state view in
    let new_knowledge = K.descend new_knowledge view in
    let old_knowledge = K.descend old_knowledge view in
    Per_player.update per_player ~old_state ~new_state ~old_knowledge ~new_knowledge turn)

(* CR stabony: implement *)
let descend t view = t

let is_probably_playable t pid cid =
  Per_player.is_probably_playable t.{View.Pid pid} cid

(* CR lightvector for stabony: implement, maybe set it to something very simple to start *)
let prob_of_good_hint t pid =
  assert false
