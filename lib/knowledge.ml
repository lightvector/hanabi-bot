open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

(* CR lightvector: Note that count-based reasoning is NOT implemented.
   Ex: You have 3 fives and you don't know their colors, and someone else has a red 5, and
   someone has a white 5. Everyone knows their fives are fives. Then, you should know that
   both of them know that their fives are red and white. Currently this deduction won't
   get made because you don't know your own fives' colors. *)

module Cond = struct
  type t = (Card.t -> bool)

  let create f = f

  let not t = (fun card -> not (t card))
  let (||) t1 t2 = (fun card -> t1 card || t2 card)
  let (&&) t1 t2 = (fun card -> t1 card && t2 card)

  let never = (fun _ -> false)
  let always = (fun _ -> true)

  let color c = (fun card -> Color.(=) card.Card.color c)
  let number n = (fun card -> Number.(=) card.Card.number n)
  let card c = (fun card -> Card.(=) card c)
  let hint params h = (fun card -> Params.hint_matches_card params h card)

  let playable state = (fun card -> State.is_playable state card)
  let useless state = (fun card -> State.is_useless state card)
  let dangerous state = (fun card -> State.is_dangerous state card)
end


module Of_card = struct
  (* Unnormalized distribution on cards that this card could be *)
  type t = (Card.t * float) list
  with sexp_of

  let lift cond = (fun (c,_) -> cond c)

  let maybe t cond = List.exists t ~f:(lift cond)
  let definitely t cond = List.for_all t ~f:(lift cond)
  let condition_on t cond = List.filter t ~f:(lift cond)
  let condition_on_not t cond = List.filter t ~f:(lift (Cond.not cond))

  let prob t cond =
    let total = List.fold t ~init:0. ~f:(fun acc (_,x) -> acc +. x) in
    let sum = List.fold t ~init:0. ~f:(fun acc (c,x) -> if cond c then acc +. x else acc) in
    if Float.(<) total 1e-20
    then failwith "Total rounds to zero!"
    else total /. sum

  let add_evidence t cond ~likelihood =
    List.map t ~f:(fun ((c,x) as pair) -> if cond c then (c, x *. likelihood) else pair)

  let contradiction t =
    match t with
    | [] -> true
    | _ :: _ -> false

  let is_known_or_contradiction t =
    match t with
    | [] | [_] -> true
    | _ :: _ :: _ -> false

  let known t =
    match t with
    | [] | _ :: _ :: _ -> None
    | [x,_] -> Some x

  let known_color t =
    match t with
    | [] -> None
    | (card,_) :: rest ->
      if definitely rest (Cond.color card.Card.color)
      then Some card.Card.color
      else None

  let known_number t =
    match t with
    | [] -> None
    | (card,_) :: rest ->
      if definitely rest (Cond.number card.Card.number)
      then Some card.Card.number
      else None

end

module View = struct
  type t = {
    of_cards: Of_card.t Card_id.Map.t;
    unknown_count: int Card.Map.t;
  }
  with sexp_of

  let card t cid = t.of_cards.{cid}

  (* If of_card -> new_of_card is a new deduction of exactly what of_card is, call f *)
  let if_new_deduce ~of_card ~new_of_card ~f =
    if Option.is_none (Of_card.known of_card)
    then Option.iter (Of_card.known new_of_card) ~f
  let new_deduce ~of_card ~new_of_card =
    if Option.is_none (Of_card.known of_card)
    then Of_card.known new_of_card
    else None

  (* Updates knowledge to reflect knowing the identity of a card *)
  let rec reveal t cid card =
    (* If we already know what this card is, do nothing *)
    if Of_card.is_known_or_contradiction t.of_cards.{cid}
    then t
    else begin
      (* Based on how many cards with this identity remain unknown, figure out how much it
         should affect our likelihoods for other cards *)
      let num_unknown = t.unknown_count.{card} - 1 in
      let unknown_count = Map.add t.unknown_count ~key:card ~data:num_unknown in
      let cond = Cond.card card in
      let likelihood =
        if num_unknown <= 0
        then 0.
        else float num_unknown /. (float num_unknown +. 1.)
      in
      (* Fold over all cards and add the information *)
      let newly_deduced = ref [] in
      let of_cards = Map.mapi t.of_cards ~f:(fun ~key ~data:of_card ->
        (* For the card itself, add the info that it is this card *)
        if Card_id.(=) key cid
        then Of_card.condition_on of_card cond
        (* For other cards, if there are still copies out there, adjust likelihood *)
        else if num_unknown > 0
        then Of_card.add_evidence of_card cond ~likelihood
        (* Else there are no other copies out there, condition away the possiblity *)
        else begin
          let new_of_card = Of_card.condition_on_not of_card cond in
          if_new_deduce ~of_card ~new_of_card ~f:(fun card ->
            newly_deduced := (key,card) :: !newly_deduced;
          );
          new_of_card
        end
      )
      in
      let t = { unknown_count; of_cards } in
      (* And recurse on newly determined cards *)
      List.fold !newly_deduced ~init:t ~f:(fun t (cid,card) -> reveal t cid card)
    end

  (* Updates knowledge to reflect a change in knowledge about a card. *)
  let inform t cid ~f =
    let of_card = t.of_cards.{cid} in
    let new_of_card = f of_card in
    match new_deduce ~of_card ~new_of_card with
    | Some card -> reveal t cid card
    | None -> { t with of_cards = Map.change t.of_cards cid (Option.map ~f) }

  (* Updates knowledge to condition a card on a cond. *)
  let inform_cond t cid cond =
    inform t cid ~f:(fun of_card -> Of_card.condition_on of_card cond)
end


type t = {
  (* pid -> what I think that player knows, 2 levels *)
  viewss: View.t Player_id.Map.t Player_id.Map.t;
  common: View.t;
}
with sexp_of

let empty params =
  let deck = Deck_params.to_deck params.Params.deck_params in
  let base_per_card =
    List.map deck ~f:(fun card -> (card,()))
    |> Card.Map.of_alist_multi
    |> Map.map ~f:List.length
    |> Map.map ~f:Float.of_int
    |> Map.to_alist
  in
  let of_cards =
    Card_id.all ~deck
    |> List.map ~f:(fun cid -> (cid,base_per_card))
    |> Card_id.Map.of_alist_exn
  in
  let unknown_count =
    List.map deck ~f:(fun card -> (card,()))
    |> Card.Map.of_alist_multi
    |> Map.map ~f:List.length
  in
  let initial_view = { View. of_cards; unknown_count } in
  let viewss =
    Player_id.all ~player_count:params.Params.player_count
    |> List.map ~f:(fun pid1 ->
      let map =
        Player_id.all ~player_count:params.Params.player_count
        |> List.map ~f:(fun pid2 -> (pid2, initial_view))
        |> Player_id.Map.of_alist_exn
      in
      (pid1, map)
    )
    |> Player_id.Map.of_alist_exn
  in
  { viewss;
    common = initial_view;
  }

let view t pid =
  t.viewss.{pid}.{pid}
let view2 t pid1 pid2 =
  t.viewss.{pid1}.{pid2}

let card t pid cid =
  View.card (view t pid) cid
let card2 t pid1 pid2 cid =
  View.card (view2 t pid1 pid2) cid

let map_all_views t ~f = {
  viewss = Map.map t.viewss ~f:(fun views -> Map.map views ~f);
  common = f t.common;
}

let update t ~old_state ~new_state ~turn =
  let reveal_if_able view state cid =
    match State.card state cid with
    | None -> view
    | Some card -> View.reveal view cid card
  in
  List.fold turn.Turn.events ~init:t ~f:(fun t event ->
    match event with
    | Turn.Draw cid -> {
      viewss = Map.mapi t.viewss ~f:(fun ~key:pid1 ~data:views ->
        Map.mapi views ~f:(fun ~key:pid2 ~data:view ->
          (* Player cannot see what they themselves draw *)
          if Player_id.(=) pid1 turn.Turn.who
            || Player_id.(=) pid2 turn.Turn.who
          then view
          else reveal_if_able view new_state cid
        ));
      common = t.common;
    }
    | Turn.Discard (_,cid) -> {
      viewss = Map.map t.viewss ~f:(fun views ->
        Map.map views ~f:(fun view ->
          reveal_if_able view new_state cid
        ));
      common = reveal_if_able t.common new_state cid;
    }
    | Turn.Play (_,cid,playable) ->
      let update_view view =
        match State.card new_state cid with
        | None ->
          let cond =
            if playable
            then Cond.playable old_state
            else Cond.not (Cond.playable old_state)
          in
          View.inform_cond view cid cond
        | Some card -> View.reveal view cid card
      in
      map_all_views t ~f:update_view
    | Turn.Hint None -> t
    | Turn.Hint (Some { Hint. target; hint; hand_indices }) ->
      let cond =
        match hint with
        | Hint.Number n ->
          Set.fold new_state.State.params.Params.rainbow_numbers
            ~init:(Cond.number n) ~f:(fun cond n -> Cond.(||) cond (Cond.number n))
        | Hint.Color c ->
          Set.fold new_state.State.params.Params.rainbow_colors
          ~init:(Cond.color c) ~f:(fun cond c -> Cond.(||) cond (Cond.color c))
      in
      let hand = old_state.State.hands.{target} in
      let update_view view =
        List.foldi hand ~init:view ~f:(fun hidx view cid ->
          if Set.mem hand_indices hidx
          then View.inform_cond view cid cond
          else View.inform_cond view cid (Cond.not cond)
        )
      in
      map_all_views t ~f:update_view
  )

let reveal t cid card =
  map_all_views t ~f:(fun view -> View.reveal view cid card)

let inform t cid ~f =
  map_all_views t ~f:(fun view -> View.inform view cid ~f)

let inform_cond t cid cond =
  map_all_views t ~f:(fun view -> View.inform_cond view cid cond)

let descend t gameview =
  match gameview with
  | Game.View.Common -> map_all_views t ~f:(fun _ -> t.common)
  | Game.View.Pid pid0 ->
    let views0 = t.viewss.{pid0} in
    { viewss = Map.mapi t.viewss ~f:(fun ~key:pid1 ~data:views ->
      Map.mapi views ~f:(fun ~key:pid2 ~data:_ ->
        if Player_id.(=) pid1 pid2
        then views0.{pid1}
        else t.common
      ));
      common = t.common;
    }
