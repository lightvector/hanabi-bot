open Core.Std
open Hanabi_types
open Int.Replace_polymorphic_compare

module Turn = struct
  type event =
  | Hint of Hint.t
  | Discard of int * Card_id.t (* int = hand idx *)
  | Play of int * Card_id.t * bool (* int = hand_idx, bool = was_playable *)
  | Draw of Card_id.t
  with sexp
  type t =
    { who : Player_id.t
    ; events : event list
    } with sexp

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Deck_params = struct
  type t =
  | Symmetric of int Number.Map.t * Color.t list
  | Explicit  of Card.t list
  with sexp

  let standard =
    let number_distribution =
      Number.Map.of_alist_exn
	[ (Number.of_int 1, 3)
	; (Number.of_int 2, 2)
	; (Number.of_int 3, 2)
	; (Number.of_int 4, 2)
	; (Number.of_int 5, 1)
	]
    in
    Symmetric (number_distribution, Color.default_5)

  let to_deck = function
    | Explicit cards -> cards
    | Symmetric (number_dist, colors) ->
      let deck = ref [] in
      Number.Map.iter number_dist
	~f:(fun ~key ~data ->
	  let cards_to_add =
	    List.concat_map colors
	      ~f:(fun color ->
		List.init data
		  ~f:(fun _ -> { Card. color; number = key }))
	  in
	  deck := (cards_to_add @ !deck));
      !deck
end

module Params = struct
  type t =
    { deck_params: Deck_params.t
    ; colors: Color.Set.t
    ; max_number: Number.t
    ; initial_hints: int
    ; max_hints: int
    ; bombs_before_loss: int
    ; rainbow_colors: Color.Set.t
    ; rainbow_numbers: Number.Set.t
    ; hintable_colors: Color.Set.t
    ; hintable_numbers: Number.Set.t
    ; possible_hints: Hint.hint list
    ; player_count: int
    ; hand_size: int
    ; max_score: int
    ; max_discards: int
    }
  with sexp

  let standard ~player_count =
    let hand_size =
      match player_count with
      | 2 | 3 -> 5
      | 4 | 5 -> 4
      | _ -> failwithf "no standard game params with player_count = %d" player_count ()
    in
    let hintable_colors = Color.default_5 |> Color.Set.of_list in
    let hintable_numbers = Number.all ~num_numbers:5 |> Number.Set.of_list in
    let deck_params = Deck_params.standard in
    let deck_size = Deck_params.to_deck deck_params |> List.length in
    let max_score = 25 in
    { deck_params;
      colors = Color.default_5 |> Color.Set.of_list;
      max_number = Number.of_int 5;
      initial_hints = 8;
      max_hints = 8;
      bombs_before_loss = 3;
      rainbow_colors = Color.Set.empty;
      rainbow_numbers = Number.Set.empty;
      hintable_colors;
      hintable_numbers;
      possible_hints =
        (List.map (Set.to_list hintable_colors) ~f:(fun c -> Hint.Color c)
         @ List.map (Set.to_list hintable_numbers) ~f:(fun n -> Hint.Number n));
      player_count;
      hand_size;
      max_score = 25;
      max_discards = deck_size - max_score - player_count * (hand_size-1);
    }

  let hint_matches_card t hint card =
    match hint with
    | Hint.Number n ->
      Number.(=) card.Card.number n || Set.mem t.rainbow_numbers card.Card.number
    | Hint.Color c ->
      Color.(=) card.Card.color c  || Set.mem t.rainbow_colors card.Card.color
end

module View = struct
  module T = struct
    type t =
    | Common
    | Pid of Player_id.t
    with sexp, compare
  end
  include T
  include Comparable.Make(T)
end

module State = struct
  type t =
    { params: Params.t
    ; deck: Card_id.t list
    ; bombs_left: int
    ; hints_left: int
    ; final_turns_left: int
    ; num_played: int
    ; played_cards: Card_id.t list
    ; playable_numbers: Number.t Color.Map.t (* keys have full domain *)
    ; handdeck_count: int Card.Map.t         (* keys have full domain *)
    ; unknown_count: int Card.Map.t          (* keys have full domain *)
    ; dead_cards: Card.Set.t
    ; discarded_cards: Card_id.t list
    ; known_cards: Card.t Card_id.Map.t
    ; hands: Card_id.t list Player_id.Map.t  (* keys have full domain *)
    ; rev_history: Turn.t list
    ; cur_player: Player_id.t
    }
  with sexp

  (* BASIC UTILITIES *)

  let card t card_id =     Map.find     t.known_cards card_id
  let card_exn t card_id = Map.find_exn t.known_cards card_id
  let player_card_exn t player hand_idx = List.nth_exn t.hands.{player} hand_idx

  let is_playable t card =
    Number.(=) card.Card.number t.playable_numbers.{card.Card.color}

  let is_useless t card =
    Number.(<) card.Card.number t.playable_numbers.{card.Card.color}
    || Set.mem t.dead_cards card

  let is_dangerous t card =
    Number.(>=) card.Card.number t.playable_numbers.{card.Card.color}
    && t.handdeck_count.{card} = 1

  let hint_matches_card t hint card =
    Params.hint_matches_card t.params hint card

  let rec for_all_i list ~f =
    let rec loop i list ~f =
      match list with
      | [] -> true
      | hd :: tl ->
        if f i hd
        then loop (i+1) tl ~f
        else false
    in
    loop 0 list ~f

  let is_legal_hint ?(allow_unknown_unhinted=false) t hint =
    let { Hint. target; hint; hand_indices } = hint in
    Player_id.is_legal target ~player_count:t.params.Params.player_count
    && Player_id.(<>) target t.cur_player
    && t.hints_left > 0
    && not (Set.is_empty hand_indices)
    && for_all_i t.hands.{target} ~f:(fun i card_id ->
      match card t card_id with
      | None ->
        if not (Set.mem hand_indices i)
          && allow_unknown_unhinted
        then true
        else failwith "hint of hand with unknown card"
      | Some card ->  Bool.(=) (hint_matches_card t hint card) (Set.mem hand_indices i)
    )

  let matching_indices_exn t hint hand =
    List.filter_mapi hand ~f:(fun i card_id ->
      if hint_matches_card t hint (card_exn t card_id)
      then Some i
      else None
    ) |> Int.Set.of_list


  let all_legal_hints_of_hand_exn t hand ~target =
    List.map t.params.Params.possible_hints ~f:(fun hint -> hint, matching_indices_exn t hint hand)
    |> List.filter_map ~f:(fun (hint, matches) ->
      if not (Set.is_empty matches)
      then Some { Hint.target; hint; hand_indices = matches }
      else None
    )

  let all_legal_hints_exn t player =
    let hand = t.hands.{player} in
    all_legal_hints_of_hand_exn t hand ~target:player

  let all_cards_in_hand_known t player =
    let hand = t.hands.{player} in
    List.for_all hand ~f:(fun id -> Map.mem t.known_cards id)

  let maybe_matching_indices t hint hand =
    List.filter_mapi hand ~f:(fun i card_id ->
      match card t card_id with
      | None -> None
      | Some card ->
        if hint_matches_card t hint card
        then Some i
        else None
    ) |> Int.Set.of_list

  let maybe_legal_hints t player =
    let hand = t.hands.{player} in
    List.map t.params.Params.possible_hints ~f:(fun hint -> hint, maybe_matching_indices t hint hand)
    |> List.filter_map ~f:(fun (hint, matches) ->
      if not (Set.is_empty matches)
      then Some { Hint.target = player; hint; hand_indices = matches }
      else None
    )


  let is_definitely_legal ?allow_unknown_unhinted t action =
    match action with
    | Action.Hint hint -> is_legal_hint ?allow_unknown_unhinted t hint
    | Action.Discard i | Action.Play i ->
      0 <= i
      && i < List.length t.hands.{t.cur_player}

  let turn_of_action_exn ?playable_if_unknown ?allow_unknown_unhinted t action  =
    if not (is_definitely_legal ?allow_unknown_unhinted t action)
    then failwith (
      Sexp.to_string (Action.sexp_of_t action)
      ^ "\n"
      ^ Sexp.to_string (sexp_of_t t));
    let events =
      let non_draw_event =
        match action with
        | Action.Hint hint -> Turn.Hint hint
        | Action.Discard i ->
          let card_id = player_card_exn t t.cur_player i in
          Turn.Discard (i, card_id)
        | Action.Play i ->
          let card_id = player_card_exn t t.cur_player i in
          let playable =
            match playable_if_unknown with
            | None -> is_playable t (card_exn t card_id)
            | Some b ->
              match card t card_id with
              | None -> b
              | Some card -> is_playable t card
          in
          Turn.Play (i, card_id, playable)
      in
      let draws =
        match action with
        | Action.Hint hint -> []
        | Action.Discard _ | Action.Play _ ->
          match t.deck with
          | [] -> []
          | next_card_id :: _ -> [ Turn.Draw next_card_id ]
      in
      non_draw_event :: draws
    in
    { Turn. who = t.cur_player; events }

  let eval_turn_exn t turn =
    let { Turn. who; events } = turn in
    let eval_event t event =
      let bombs_used, hints_used, play, discard, draw =
        match event with
        | Turn.Discard (_, card_id) ->
          0, (-1), None, Some card_id, None
        | Turn.Play (i, card_id, playable) ->
          if playable
          then 0, 0, Some card_id, None, None
          else 1, 0, None, Some card_id, None
        | Turn.Hint _ ->
          0, 1, None, None, None
        | Turn.Draw card_id ->
          0, 0, None, None, Some card_id
      in
      let deck =
        match event with
        | Turn.Draw _ -> List.tl_exn t.deck
        | _ -> t.deck
      in
      let remove_from_hand hands card_id =
        Map.add hands ~key:who
          ~data:(List.filter hands.{who} ~f:(fun c -> Card_id.(<>) c card_id))
      in
      let add_to_hand hands card_id =
        Map.add hands ~key:who ~data:(card_id :: hands.{who})
      in
      let played_cards, num_played, hands =
        Option.fold play ~init:(t.played_cards, t.num_played, t.hands)
          ~f:(fun (played_cards, num_played, hands) play ->
            play :: played_cards,
            num_played + 1,
            remove_from_hand hands play)
        |> fun init -> Option.fold discard ~init
          ~f:(fun (played_cards, num_played, hands) discard ->
            played_cards,
            num_played,
            remove_from_hand hands discard)
        |> fun init -> Option.fold draw ~init
          ~f:(fun (played_cards, num_played, hands) draw ->
            played_cards,
            num_played,
            add_to_hand hands draw)
      in
      let playable_numbers, handdeck_count, dead_cards =
        Option.fold play ~init:(t.playable_numbers, t.handdeck_count, t.dead_cards)
          ~f:(fun ((playable_numbers, handdeck_count, dead_cards) as s) play ->
            match card t play with
            | None -> s
            | Some card ->
              Map.add playable_numbers ~key:card.Card.color ~data:(Number.next card.Card.number),
              Map.add handdeck_count ~key:card ~data:(handdeck_count.{card} - 1),
              dead_cards
          )
        |> fun init -> Option.fold discard ~init
          ~f:(fun ((playable_numbers, handdeck_count, dead_cards) as s) discard ->
            match card t discard with
            | None -> s
            | Some card ->
              let hdcount = handdeck_count.{card} - 1 in
              let dead_cards =
                if hdcount = 0 && Number.(>=) card.Card.number playable_numbers.{card.Card.color}
                then List.fold (Number.between ~min:card.Card.number ~max:t.params.Params.max_number)
                  ~init:dead_cards ~f:(fun dead_cards n -> Set.add dead_cards {
                    Card.color = card.Card.color; number = n; })
                else dead_cards
              in
              playable_numbers,
              Map.add handdeck_count ~key:card ~data:hdcount,
              dead_cards
          )
      in
      { t with deck
        ; bombs_left = t.bombs_left - bombs_used
        ; hints_left = min (t.hints_left - hints_used) t.params.Params.max_hints
        ; num_played
        ; played_cards
        ; playable_numbers
        ; handdeck_count
        ; dead_cards
        ; discarded_cards = begin
          match discard with
          | None -> t.discarded_cards
          | Some card_id -> card_id :: t.discarded_cards
        end
        ; hands
      }
    in
    let final_turns_used =
      if List.is_empty t.deck
      then 1
      else 0
    in
    let new_t =
      List.fold turn.Turn.events ~init:t ~f:eval_event
    in
    { new_t with
      final_turns_left = new_t.final_turns_left - final_turns_used;
      rev_history = turn :: t.rev_history;
      cur_player = Player_id.next t.cur_player ~player_count:t.params.Params.player_count;
    }

  let eval_action_exn ?playable_if_unknown ?allow_unknown_unhinted t action =
    let turn = turn_of_action_exn ?playable_if_unknown ?allow_unknown_unhinted t action in
    eval_turn_exn t turn, turn

  let rec random_permutation l ~rand =
    let arr = Array.of_list l in
    for i = 1 to Array.length arr - 1 do
      let r = Random.State.int rand (i+1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(r);
      arr.(r) <- temp;
    done;
    Array.to_list arr

  (* creates the all-known-cards initial state *)
  let create params ~seed =
    let { Params. deck_params; colors; initial_hints
	; max_hints; bombs_before_loss; rainbow_colors
	; rainbow_numbers; player_count; hand_size; _ } = params
    in
    let rand = Random.State.make [|seed|] in
    let cards = random_permutation (Deck_params.to_deck deck_params) ~rand in
    let deck = Card_id.all ~deck:cards in
    let bombs_left = bombs_before_loss in
    let hints_left = initial_hints in
    let final_turns_left = player_count in
    let num_played = 0 in
    let played_cards = [] in
    let playable_numbers =
      Color.Map.of_alist_exn (List.map (Set.to_list colors) ~f:(fun c -> c, Number.first))
    in
    let handdeck_count =
      List.map cards ~f:(fun card -> (card,()))
      |> Card.Map.of_alist_multi
      |> Map.map ~f:List.length
    in
    let handdeck_count =
      (* Just to be able to handle weird decks and out-of-bounds queries *)
      List.fold (Number.between ~min:Number.first ~max:params.Params.max_number)
        ~init:handdeck_count ~f:(fun handdeck_count number ->
          Set.fold params.Params.colors
            ~init:handdeck_count ~f:(fun handdeck_count color ->
              let card = { Card. number; color } in
              Map.change handdeck_count card (function None -> Some 0 | x -> x)
            )
        )
    in
    let unknown_count =
      (* All cards are known, none are unknown *)
      Map.map handdeck_count ~f:(fun _ -> 0)
    in
    let dead_cards = Card.Set.empty in
    let discarded_cards = [] in
    let players = Player_id.all ~player_count in
    let hands =
      List.map players ~f:(fun p -> (p,[]))
      |> Player_id.Map.of_alist_exn
    in
    let known_cards =
      List.zip_exn deck cards
      |> Card_id.Map.of_alist_exn
    in
    let rev_history = [] in
    let cur_player = Player_id.first in
    let init =
      { params; deck; bombs_left; hints_left; final_turns_left; num_played
      ; played_cards; playable_numbers; handdeck_count; unknown_count; dead_cards
      ; discarded_cards; hands; known_cards; rev_history
      ; cur_player }
    in
    let initial_turns =
      List.init (hand_size * player_count) ~f:(fun i ->
        { Turn. who = Player_id.of_int (i mod player_count)
        ; events = [ Turn.Draw (Card_id.of_int i) ] }
      )
    in
    List.fold initial_turns ~init ~f:eval_turn_exn

  let reveal_exn t card_id card =
    if Map.mem t.known_cards card_id then failwith "reveal_exn revealing known card";
    let count = Map.find_exn t.unknown_count card in
    if count <= 0 then failwith "reveal_exn revealing card all of which are known";
    let unknown_count = Map.add t.unknown_count ~key:card ~data:(count-1) in
    let known_cards = Map.add t.known_cards ~key:card_id ~data:card in
    { t with known_cards; unknown_count }

  let specialize t view =
    let invisible_cards =
      (match view with
      | View.Common  -> List.concat (Map.data t.hands)
      | View.Pid pid -> t.hands.{pid}
      ) @ t.deck
      |> Card_id.Set.of_list
    in
    let unknown_count = Card_id.Map.fold t.known_cards ~init:t.unknown_count
      ~f:(fun ~key:card_id ~data:card unknown_count ->
      if Set.mem invisible_cards card_id
      then Map.change unknown_count card (fun count -> Some (Option.value_exn count + 1))
      else unknown_count
      )
    in
    let known_cards = Card_id.Map.filter t.known_cards ~f:(fun ~key:card_id ~data:card ->
      not (Set.mem invisible_cards card_id))
    in
    { t with known_cards; unknown_count }

  let score t =
    if t.bombs_left = 0
    then 0
    else t.num_played

  let is_game_over t =
    t.bombs_left = 0
    || t.final_turns_left = 0

  let discards_left t =
    t.params.Params.max_discards - List.length t.discarded_cards

  (* MISC *)
  let display_string ?(use_ansi_colors=false) t =
    let cardstr card =
      if use_ansi_colors
      then Card.to_ansicolor_string card
      else Card.to_string card
    in
    let idstr id =
      Option.value_map (card t id)
        ~default:"?"
        ~f:cardstr
    in
    let player_ids = Map.keys t.hands |> List.sort ~cmp:Player_id.compare in
    let hand_str =
      List.map player_ids ~f:(fun id ->
        let hand = t.hands.{id} in
        let hand_str = List.map hand ~f:idstr |> String.concat ~sep:"" in
        let to_play_str =
          if Player_id.(=) id t.cur_player
          then "*"
          else " "
        in
        sprintf "%sP%d: %s" to_play_str (Player_id.to_int id) hand_str
      )
      |> String.concat ~sep:" "
    in
    let played_str =
      List.map (Set.to_list t.params.Params.colors) ~f:(fun color ->
        let cards = List.filter_map t.played_cards ~f:(fun id -> card t id) in
        let cards = List.filter cards ~f:(fun card -> Color.(=) card.Card.color color) in
        match List.reduce cards ~f:(fun x y ->
          if Number.(>) x.Card.number y.Card.number then x else y)
        with
        | None -> " "
        | Some card ->
          if use_ansi_colors
          then Card.to_ansicolor_string card
          else Card.to_string card
      )
      |> String.concat ~sep:" "
    in
    let danger_str =
      List.filter_map t.discarded_cards ~f:(fun id ->
        if Option.exists (card t id) ~f:(fun card -> is_useless t card)
        then None
        else Some id
      )
      |> List.sort ~cmp:(fun c0 c1 ->
        match card t c0, card t c1 with
        | None, None -> Card_id.compare c0 c1
        | None, _ -> 1
        | _, None -> -1
        | Some c0, Some c1 ->
          match Color.compare c0.Card.color c1.Card.color with
          | 0 -> Number.compare c0.Card.number c1.Card.number
          | x -> x
      )
      |> List.map ~f:idstr
      |> (fun list ->
        if List.length list >= 8
        then list
        else List.init (8 - List.length list) ~f:(fun _ -> " ") @ list)
      |> String.concat
    in
    sprintf "T%3d HL %d BL %d DL %2d Played %s %s danger %s"
      (List.length t.rev_history - t.params.Params.hand_size * t.params.Params.player_count)
      t.hints_left
      t.bombs_left
      (discards_left t)
      played_str
      hand_str
      danger_str

  let turn_display_string ?(use_ansi_colors=false) t turn =
    let cardstr card =
      if use_ansi_colors
      then Card.to_ansicolor_string card
      else Card.to_string card
    in
    let idstr id =
      Option.value_map (card t id)
        ~default:"?"
        ~f:cardstr
    in
    sprintf "Player %d: " (Player_id.to_int turn.Turn.who)
    ^ String.concat ~sep:", " (List.map turn.Turn.events ~f:(fun event ->
      match event with
      | Turn.Draw id -> "Draw " ^ idstr id
      | Turn.Play (idx,id,true) -> sprintf "Play #%d %s" idx (idstr id)
      | Turn.Play (idx,id,false) -> sprintf "Bomb #%d %s" idx (idstr id)
      | Turn.Discard (idx,id)  -> sprintf "Discard #%d %s" idx (idstr id)
      | Turn.Hint { Hint.target; hint; hand_indices } ->
        sprintf "Hint %s to P%d - %s - %s"
          (Sexp.to_string (Hint.sexp_of_hint hint))
          (Player_id.to_int target)
          (String.concat (List.map (Set.to_list hand_indices) ~f:(fun hand_idx ->
            "#" ^ Int.to_string hand_idx)))
          (String.concat (List.map (Set.to_list hand_indices) ~f:(fun hand_idx ->
            idstr (List.nth_exn (t.hands.{target}) hand_idx))))
    ))

end

module Player = struct
  module Intf = struct
    type 'a t =
      { create : (Player_id.t -> params:Params.t -> seed:int -> 'a)
      ; act : ('a -> State.t -> Action.t)
      }

    type wrapped = T:'a t -> wrapped
  end

  type 'a t = Player_id.t * 'a * 'a Intf.t

  type wrapped = T:'a t -> wrapped
end

let play params players ~seed ~f =
  assert (params.Params.player_count = List.length players);
  let players =
    List.mapi players ~f:(fun i (Player.Intf.T intf) ->
      let player_id = Player_id.of_int i in
      Player.T (player_id, intf.Player.Intf.create player_id ~params ~seed, intf))
    |> Queue.of_list
  in
  let state = State.create params ~seed in
  let rec loop state =
    if State.is_game_over state
    then state
    else
      let player = Queue.dequeue_exn players in
      let (Player.T (player_id, player_state, intf)) = player in
      let action =
        intf.Player.Intf.act player_state (State.specialize state (View.Pid player_id))
      in
      let new_state, turn = State.eval_action_exn state action in
      Queue.enqueue players player;
      f ~old:state new_state turn;
      loop new_state
  in
  loop state

let _base_player () = assert false

(* let () =
 *   let state =
 *     play (Params.standard ~player_count:2) ~seed:123
 *       [ Player.Intf.auto_player
 *       ; Player.Intf.auto_player ]
 *   in
 *   printf "%s\n%!" (Sexp.to_string (State.sexp_of_t (fun _ -> Sexp.unit) state)) *)
(* module type Player = sig
 *   type t
 *   val update: t -> Action.t -> unit
 *   val act: t -> State.t -> Action.t
 * end *)
