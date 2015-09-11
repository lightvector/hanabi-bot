open Core.Std
open Hanabi_types
open Game
open Int.Replace_polymorphic_compare

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


module Per_card = struct
  (* Unnormalized distribution on cards that this card could be *)
  type t = (Card.t * float) list

  let lift cond = (fun (c,_) -> cond c)

  let maybe t cond = List.exists t ~f:(lift cond)
  let definitely t cond = List.for_all t ~f:(lift cond)
  let condition_on t cond = List.filter t ~f:(lift cond)

  let prob t cond =
    let total = List.fold t ~init:0. ~f:(fun acc (_,x) -> acc +. x) in
    let sum = List.fold t ~init:0. ~f:(fun acc (c,x) -> if cond c then acc +. x else acc) in
    if Float.(<) total 1e-20
    then failwith "Total rounds to zero!"
    else total /. sum

  let add_evidence t cond ~odds =
    List.map t ~f:(fun ((c,x) as pair) -> if cond c then (c, x *. odds) else pair)
end

(* player -> what I think that player knows *)
type t = Per_card.t Card_id.Map.t Player_id.Map.t


let create params =
  let deck = Deck_params.to_deck params.Params.deck_params in
  let base_per_card =
    List.map deck ~f:(fun card -> (card,()))
    |> Card.Map.of_alist_multi
    |> Map.map ~f:List.length
    |> Map.map ~f:Float.of_int
    |> Map.to_alist
  in
  Player_id.all ~player_count:params.Params.player_count
  |> List.map ~f:(fun player ->
    (player,
     Card_id.all ~deck
     |> List.map ~f:(fun id -> (id,base_per_card))
     |> Card_id.Map.of_alist_exn
    ))
  |> Player_id.Map.of_alist_exn

let player t player =
  Map.find_exn t player

let card t player id =
  Map.find_exn (Map.find_exn t player) id

let update _t ~old:_ _state _turn =
  assert false
  (* let reveal bycard
   *
   * List.fold turn.Turn.events ~init:t ~f:(fun t event ->
   *   match event with
   *   | Turn.Draw id ->
   *     Map.mapi t ~f:(fun ~key:id ~data:bycard ->
   *       if id = turn.Turn.who
   *       then bycard
   *       else begin
   *
   *       end
   *
   * match turn with
   * | *)




