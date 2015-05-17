open Core.Std

type color =
    White 
  | Blue
  | Yellow
  | Red
  | Green

type card = {
  color : color
; number : int
}

type player_name = int

type state = {
  hint_token : int
; bombs : int
; player_cards : card List.t Int.Map.t
; top_played_cards : card List.t
; remaining_cards : card List.t
} 

type hint =
    Color of color
  | Number of int

type play = 
    Hint of player_name * hint * (int List.t)
  | Discard of int
  | Play of int
  | Bomb of int

type player =
    Player of (state -> play Int.Map.t -> (player * play))

let shuffle l =
  Random.init 42;
  List.sort l ~cmp:(fun _ _ -> if Random.bool () then 1 else -1) 

let all_colors = [ White; Blue; Yellow; Red; Green ]

let standard_deck =
  List.fold
    all_colors
    ~init:[]
    ~f:(fun accu color ->
	(List.init
	   5
	   ~f:(fun number -> { color; number }))@accu)

let standard_init_state n =
  let deck = shuffle standard_deck in
  let number_cards =
    if n < 5 then
      5
    else if n = 5 then
      4
    else
      failwith "too many players"
  in
  let draw_cards m remaining_deck =
    let rec draw_cards m remaining_deck accu =
      match m, remaining_deck with
      | 0, _ -> remaining_deck, accu
      | _, t::q ->
	 draw_cards (m-1) q (t::accu)
      | _, _ -> failwith "not enough cards"
    in
    draw_cards m remaining_deck []
  in
  let remaining_cards, player_cards =
    List.init
      n
      ~f:(fun i -> i)
    |> List.fold
	 ~init:(deck, Int.Map.empty)
	 ~f:(fun (remaining_deck, player_cards) i ->
	     draw_cards number_cards remaining_deck
	     |> fun (x, y) ->
		x, Map.add player_cards ~key:i ~data:y)
  in
  { hint_token = 8
  ; bombs = 3
  ; player_cards
  ; top_played_cards = []
  ; remaining_cards
  } 

let get_hint hint =
  let test =
    match hint with
    | Color color ->
       fun card ->
       card.color = color
    | Number n ->
       fun card ->
       card.number = n
  in
  List.filter_mapi
    ~f:(fun i card ->
	if test card then
	  Some i else None)

let remove_card state player i =
  let player_cards =
    Map.find_exn state.player_cards player
  in
  let selected_card = List.nth_exn player_cards i in
  match state.remaining_cards with
  | [] -> failwith "game lost"
  | t::q -> 
     let new_player_cards =
       t::(List.drop player_cards i)
     in
     { state with
       remaining_cards = q
     ; player_cards =
	 Map.add
	   state.player_cards
	   ~key:player
	   ~data:new_player_cards },
     selected_card

let do_play played_cards card = (* return [] if bomb *)
  match List.partition_tf played_cards ~f:(fun c -> c.color = card.color) with
  | [], q -> if card.number = 1 then card::q else []
  | [c], q ->  if card.number = c.number+1 then card::q else []
  | _, _ -> failwith "multiple card with the same color"

let test_victory played_cards =
  List.length played_cards = 5
  && List.for_all played_cards ~f:(fun c -> c.number =5)

let hanabi
      state
      next_player
      players =
  let rec hanabi
	    last_plays
	    state
	    next_player
	    players =
    let state_for_player =
      { state with
	player_cards =
	  Map.remove state.player_cards next_player
      ; remaining_cards = [] }
    in
    let Player next_player_function =
      Map.find_exn players next_player
    in
    let new_function, play =
      next_player_function
	state_for_player
	last_plays
    in
    let state, last_play =
      match play with
      | Hint (player, hint, hint_list) ->
	 let real_hint =
	   Map.find_exn state.player_cards player
	   |> get_hint hint
	 in
	 if hint_list = real_hint then
	   if state.hint_token > 1 then 
	     { state with
	       hint_token = state.hint_token - 1 },
	     play
	   else
	     failwith "no token left"
	 else
	   failwith "incorrect hint"
      | Discard i ->
	 let state, _ = remove_card state next_player i in
	 { state with hint_token = state.hint_token + 1 },
	 play
      | Play i ->
	 begin
	   let state, card = remove_card state next_player i in
	   match do_play state.top_played_cards card with
	   | [] -> 
	      state, Bomb i
	   | l ->
	      { state with top_played_cards = l }, play
	 end
      | Bomb _ -> failwith "cannot choose to bomb"
     
    in
    if test_victory state.top_played_cards then
      true
    else
      if state.remaining_cards = [] then
	false
      else
	let last_plays = 
	  Map.add last_plays next_player last_play 
	in
	let next_player =
	  match Map.find state.player_cards (next_player+1) with
	  | None -> 0
	  | _ -> next_player+1
	in
	hanabi
	  last_plays
	  state
	  next_player
	  players
  in
  hanabi
    Int.Map.empty
    state
    next_player
    players
    
let rec test_bot number_runs number_players player =
  match number_runs with
  | 0 -> 0
  | _ ->
     test_bot (number_runs-1) number_players player
     + if hanabi
	    (standard_init_state number_players)
	    0
	    (List.init number_players ~f:(fun i -> i,player)
	     |> Int.Map.of_alist_exn) then
	 1
       else
	 0
