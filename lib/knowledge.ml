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

module Per_player = struct
  type t = {
    of_cards: Of_card.t Card_id.Map.t;
    unknown_count: int Card.Map.t;
  }
  with sexp_of

  let card t id = t.of_cards.{id}

  (* If of_card -> new_of_card is a new deduction of exactly what of_card is, call f *)
  let if_new_deduce ~of_card ~new_of_card ~f =
    if Option.is_none (Of_card.known of_card)
    then Option.iter (Of_card.known new_of_card) ~f
  let new_deduce ~of_card ~new_of_card =
    if Option.is_none (Of_card.known of_card)
    then Of_card.known new_of_card
    else None

  (* Updates knowledge to reflect knowing the identity of a card *)
  let rec reveal t id card =
    (* If we already know what this card is, do nothing *)
    if Of_card.is_known_or_contradiction t.of_cards.{id}
    then t
    else begin
      (* Based on how many cards with this identity remain unknown, figure out how much it
         should affect our likelihoods for other cards *)
      let num_unknown = Map.find_exn t.unknown_count card - 1 in
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
        if Card_id.(=) key id
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
      List.fold !newly_deduced ~init:t ~f:(fun t (id,card) -> reveal t id card)
    end

  (* Updates knowledge to reflect a change in knowledge about a card. *)
  let inform t id ~f =
    let of_card = t.of_cards.{id} in
    let new_of_card = f of_card in
    match new_deduce ~of_card ~new_of_card with
    | Some card -> reveal t id card
    | None -> { t with of_cards = Map.change t.of_cards id (Option.map ~f) }

  (* Updates knowledge to condition a card on a cond. *)
  let inform_cond t id cond =
    inform t id ~f:(fun of_card -> Of_card.condition_on of_card cond)
end

(* player -> what I think that player knows *)
type t = Per_player.t Player_id.Map.t
with sexp_of

let create params =
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
    |> List.map ~f:(fun id -> (id,base_per_card))
    |> Card_id.Map.of_alist_exn
  in
  let unknown_count =
    List.map deck ~f:(fun card -> (card,()))
    |> Card.Map.of_alist_multi
    |> Map.map ~f:List.length
  in
  Player_id.all ~player_count:params.Params.player_count
  |> List.map ~f:(fun player -> (player, { Per_player. of_cards; unknown_count } ))
  |> Player_id.Map.of_alist_exn

let player t player =
  Map.find_exn t player

let card t player id =
  Per_player.card (Map.find_exn t player) id

let update t ~old state turn =
  List.fold turn.Turn.events ~init:t ~f:(fun t event ->
    match event with
    | Turn.Draw id ->
      Map.mapi t ~f:(fun ~key:player ~data:per_player ->
        (* Player cannot see what they themselves draw *)
        if Player_id.(=) player turn.Turn.who
        then per_player
        else match State.card state id with
          | None -> per_player
          | Some card -> Per_player.reveal per_player id card
      )
    | Turn.Discard (_,id) ->
      Map.map t ~f:(fun per_player ->
        match State.card state id with
        | None -> per_player
        | Some card -> Per_player.reveal per_player id card
      )
    | Turn.Play (_,id,playable) ->
      Map.map t ~f:(fun per_player ->
        match State.card state id with
        | None ->
          let cond =
            if playable
            then Cond.playable old
            else Cond.not (Cond.playable old)
          in
          Per_player.inform_cond per_player id cond
        | Some card -> Per_player.reveal per_player id card
      )
    | Turn.Hint { Hint. target; hint; hand_indices } ->
      let cond =
        match hint with
        | Hint.Number n ->
          Set.fold state.State.params.Params.rainbow_numbers
            ~init:(Cond.number n) ~f:(fun cond n -> Cond.(||) cond (Cond.number n))
        | Hint.Color c ->
          Set.fold state.State.params.Params.rainbow_colors
          ~init:(Cond.color c) ~f:(fun cond c -> Cond.(||) cond (Cond.color c))
      in
      let hand = old.State.hands.{target} in
      Map.map t ~f:(fun per_player ->
        List.foldi hand ~init:per_player ~f:(fun hidx per_player id ->
          if Set.mem hand_indices hidx
          then Per_player.inform_cond per_player id cond
          else Per_player.inform_cond per_player id (Cond.not cond)
        )
      )
  )

let reveal t id card =
  Map.map t ~f:(fun per_player ->
    Per_player.reveal per_player id card
  )

let inform t id ~f =
  Map.map t ~f:(fun per_player ->
    Per_player.inform per_player id ~f
  )

let inform_cond t id cond =
  Map.map t ~f:(fun per_player ->
    Per_player.inform_cond per_player id cond
  )

let descend t pid = assert false
