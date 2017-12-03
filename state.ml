open Action
(* ############################################################################

  DEFINING TYPES

##############################################################################*)

(**
 * List of continents. Structured as (String name, (int num territories in
 * continent, int troops given for continent control))
 *)
let continents =
  [("North America", (9, 5));
   ("South America", (4, 2));
   ("Europe", (7, 5));
   ("Africa", (6, 3));
   ("Asia", (12, 7));
   ("Australia", (4, 2))]

(** [region] represents a region in Risk. *)
type region =
  {
    name: string;
    routes: string list;
    continent: string;
  }

type player =
  {
    id: string;
    cards: card list;
    total_troops: int;
    controls: (string * int) list;
    continent_troops: (string * int) list; (*regions owned per continent*)
    controls_cont: string list;
  }

type action =
  | ADeployment of string (* places one troop on region s *)
  | APlay_Cards of (card * card * card) (* the cards to trade in *)
  | AWait_Reinforcement
  | AReinforcement of string * int (* reinforce region s with n troops *)
  | AAttack of string * string (* attack from region s1 to region s2 *)
  | AMovement of (string * string) * int
      (* move n troops from region s1 to region s2 *)
  | ANext_Turn

type curr_move =
  | CDeployment
  | CReinforcement of int (* reinforce with n total troops *)
  | CAttack
  | CMovement
  | CRecieve_Card of card
  | CNext_Turn
  | CGame_Won of string (* player s won the game *)

(** Represents the current game state. The head of the [players] list is the
    current player whose turn it is. *)
type state =
  {
    current_move: curr_move;
    players: player list;
    turns: int;
    continents: (string * string option) list;
        (* continent s is controlled by player s_opt *)
    bonus_troops: int;
    log: string;
  }




(* ############################################################################

  Initial state stuff

##############################################################################*)


let init_regions =
  [{name = "Alaska";
    routes = ["Northwest Territory"; "Alberta"; "Kamchatka"];
    continent = "North America"; };
   {name = "Northwest Territory";
    routes = ["Alaska"; "Alberta"; "Ontario"; "Greenland"];
    continent = "North America"; };
   {name = "Alberta";
    routes =
      ["Alaska"; "Northwest Territory"; "Ontario"; "Western United States"];
    continent = "North America"; };
   {name = "Greenland";
    routes = ["Northwest Territory"; "Ontario"; "Quebec"; "Iceland"];
    continent = "North America"; };
   {name = "Ontario";
    routes =
      ["Alberta"; "Northwest Territory"; "Greenland"; "Quebec";
       "Eastern United States"; "Western US"];
    continent = "North America"; };
   {name = "Quebec";
    routes = ["Ontario"; "Greenland"; "Eastern United States"];
    continent = "North America"; };
   {name = "Western United States";
    routes = ["Alberta"; "Ontario"; "Eastern United States"; "Central America"];
    continent = "North America"; };
   {name = "Eastern United States";
    routes = ["Western United States"; "Ontario"; "Quebec"; "Central America"];
    continent = "North America"; };
   {name = "Central America";
    routes = ["Western United States"; "Eastern United States"; "Venezuela"];
    continent = "North America"; };
   {name = "Venezuela";
    routes = ["Peru"; "Brazil"];
    continent = "South America"; };
   {name = "Peru";
    routes = ["Venezuela"; "Brazil"; "Argentina"];
    continent = "South America"; };
   {name = "Argentina";
    routes = ["Peru"; "Brazil"];
    continent = "South America"; };
   {name = "Brazil";
    routes = ["Venezuela"; "Peru"; "Argentina"; "North Africa"];
    continent = "South America"; };
   {name = "Iceland";
    routes = ["Greenland"; "Great Britain"; "Scandinavia"];
    continent = "Europe"; };
   {name = "Great Britain";
    routes = ["Iceland"; "Western Europe"; "Scandinavia"; "Northern Europe"];
    continent = "Europe"; };
   {name = "Western Europe";
    routes =
      ["Great Britain"; "Northern Europe"; "Southern Europe"; "North Africa"];
    continent = "Europe"; };
   {name = "Scandinavia";
    routes = ["Iceland"; "Great Britain"; "Northern Europe"; "Ukraine"];
    continent = "Europe"; };
   {name = "Northern Europe";
    routes =
      ["Great Britain"; "Southern Europe"; "Western Europe"; "Scandinavia";
       "Ukraine"];
    continent = "Europe"; };
   {name = "Southern Europe";
    routes =
      ["Western Europe"; "North Africa"; "Egypt"; "Middle East"; "Ukraine"];
    continent = "Europe"; };
   {name = "Ukraine";
    routes =
      ["Scandinavia"; "Northern Europe"; "Southern Europe"; "Ural";
       "Afghanistan"; "Middle East"];
    continent = "Europe"; };
   {name = "North Africa";
    routes =
      ["Brazil"; "Western Europe"; "Southern Europe"; "Egypt"; "East Africa";
       "Congo"];
    continent = "Africa"; };
   {name = "Egypt";
    routes = ["North Africa"; "Southern Europe"; "Middle East"; "East Africa"];
    continent = "Africa"; };
   {name = "Congo";
    routes = ["North Africa"; "East Africa"; "South Africa"];
    continent = "Africa"; };
   {name = "East Africa";
    routes =
      ["North Africa"; "Egypt"; "Middle East"; "Madagascar"; "South Africa";
       "Congo"];
    continent = "Africa"; };
   {name = "South Africa";
    routes = ["Congo"; "East Africa"; "Madagascar"];
    continent = "Africa"; };
   {name = "Madagascar";
    routes = ["South Africa"; "East Africa"];
    continent = "Africa"; };
   {name = "Middle East";
    routes = ["East Africa"; "Egypt"; "Ukraine"; "Afghanistan"; "India"];
    continent = "Asia"; };
   {name = "Afghanistan";
    routes = ["Middle East"; "Ukraine"; "Ural"; "China"; "India"];
    continent = "Asia"; };
   {name = "Ural";
    routes = ["Siberia"; "China"; "Afghanistan"; "Ukraine"];
    continent = "Asia"; };
   {name = "Siberia";
    routes = ["Yakutsk"; "Irkutsk"; "Mongolia"; "China"; "Ural"];
    continent = "Asia"; };
   {name = "India";
    routes = ["Middle East"; "Afghanistan"; "China"; "Siam"];
    continent = "Asia"; };
   {name = "Siam";
    routes = ["India"; "China"; "Indonesia"];
    continent = "Asia"; };
   {name = "China";
    routes = ["Siam"; "India"; "Afghanistan"; "Ural"; "Siberia"; "Mongolia"];
    continent = "Asia"; };
   {name = "Mongolia";
    routes = ["China"; "Siberia"; "Irkutsk"; "Kamchatka"; "Japan"];
    continent = "Asia"; };
   {name = "Japan";
    routes = ["Mongolia"; "Kamchatka"];
    continent = "Asia"; };
   {name = "Kamchatka";
    routes = ["Mongolia"; "Japan"; "Alaska"; "Irkutsk"; "Yakutsk"];
    continent = "Asia"; };
   {name = "Irkutsk";
    routes = ["Ural"; "Yakutsk"; "Kamchatka"; "Mongolia"];
    continent = "Asia"; };
   {name = "Yakutsk";
    routes = ["Ural"; "Irkutsk"; "Kamchatka"];
    continent = "Asia"; };
   {name = "Indonesia";
    routes = ["Siam"; "New Guinea"; "Western Australia"];
    continent = "Australia"; };
   {name = "New Guinea";
    routes = ["Indonesia"; "Eastern Australia"; "Western Australia"];
    continent = "Australia"; };
   {name = "Western Australia";
    routes = ["Indonesia"; "New Guinea"; "Eastern Australia"];
    continent = "Australia"; };
   {name = "Eastern Australia";
    routes = ["Indonesia"; "New Guinea"; "Western Australia"];
    continent = "Australia"; }
  ]

let rec first_n lst n =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h::t -> h::(first_n t (n-1))

let rec add_regions ps rs =
  match rs with
  | [] -> ps
  | (_, h)::t ->
    let p = List.hd ps in
    let new_p =
      {p with
       total_troops = p.total_troops + 1;
       controls = h::p.controls;
       continent_troops =
         let in_cont = List.assoc h.continent p.continent_troops + 1 in
         List.remove_assoc h.continent p.continent_troops |>
         List.cons (h.continent, in_cont)
      } in
    add_regions ((List.tl ps) @ [new_p]) t

let rec add_conts c_ts =
  match c_ts with
  | [] -> []
  | h::t ->
      if fst (List.assoc (fst h) continents) = snd h
      then (fst h)::(add_conts t)
      else add_conts t

let init_state n =
  let players =
    first_n ["Red"; "Blue"; "Green"; "Yellow"; "Purple"; "Orange"] n |>
    List.map
      (fun name ->
         {
           id = name;
           cards = [];
           total_troops = 0;
           controls = [];
           continent_troops = [("Asia", 0); ("Africa", 0); ("North America", 0);
                         ("South America", 0); ("Europe", 0); ("Australia", 0)];
           controls_cont = [];
         }) in
  let players_w_regions =
    Random.self_init ();
    List.map (fun r -> (Random.float 5.0, r)) init_regions |>
    List.sort (fun c1 c2 -> compare (fst c1) (fst c2)) |>
    add_regions players in
  let players_w_continents =
    List.map
      (fun p -> {p with controls_cont = add_conts p.continent_troops}) players_w_regions in
  let total_conts =
    List.fold_left
      (fun cs p ->
         List.fold_left
           (fun cs c ->
              List.remove_assoc c cs |>
              List.cons (c, Some p.id))
           cs p.controls_cont)
      [("Asia", None); ("Africa", None); ("North America", None);
       ("South America", None); ("Europe", None); ("Australia", None)]
      players_w_continents in
  {
    players = players_w_continents;
    current_move = CDeployment;
    turns = 0;
    continents = total_conts;
    bonus_troops = 4;
    log = "Game started!" (*TODO: Make more rich*)
  }







(* ############################################################################

  Gameplay

##############################################################################*)

(* Returns the next number of bonus troops to be given when cards are traded in
 *)
let increment_bonus n =
  if n < 12 then n + 2
  else if n = 12 then 15
  else n + 5

let prepend_player p = function
  | [] -> [p]
  | h::t -> p::t

let append_player p = function
  | [] -> [p]
  | h::t -> t @ [p]

let rec remove_cards c l =
  match l with
  | [] -> l
  | h::t -> if h = c then t
            else remove_cards c t

let update st = function
  | ADeployment r ->
    if st.current_move = CDeployment then
      let p = List.hd st.players in
        (try
          let n = List.assoc r p.controls in
          let new_controls = List.remove_assoc r p.controls in
          let p' = { p with controls = (r, n + 1)::new_controls } in
          let p_list = prepend_player p' st.players in
          { st with players = p_list;
                    log = "Successfuly reinforced " ^ r }
        with
        | Not_found -> { st with log = "You don't control that territory." })
    else { st with log = "Invalid move" } (* make more descriptive *)
  | APlay_Cards (c1, c2, c3) ->
    (match st.current_move with
    | CReinforcement n ->
      let p = List.hd st.players in
    (* pretend there's some code to make sure l is a subset of head player's cards *)
    (* also force players with 5+ cards to trade in their cards *)
      if (c1 = c2 && c2 = c3) || (c1 <> c2 && c2 <> c3 && c1 <> c3) then
        let bonus_n = st.bonus_troops + n in
        let new_cards = remove_cards c1 p.cards
                        |> remove_cards c2 |> remove_cards c3 in
        let p' = { p with cards = new_cards } in
        let p_list = prepend_player p' st.players in
        { st with current_move = CReinforcement bonus_n;
                  players = p_list;
                  bonus_troops = increment_bonus bonus_n;
                  log = "Successfully traded in cards for " ^
                        (string_of_int bonus_n) ^ " extra troops"; }
      else
        { st with log = "Invalid card trade-in" }
    | _ -> { st with log = "Invalid move" })
  | AWait_Reinforcement ->
    (match st.current_move with
    | CReinforcement n ->
      let p = List.hd st.players in
      let n_controls = (List.length p.controls) / 3 in
      let rec n_continents acc = function
        | [] -> acc
        | h::t -> n_continents (acc + (snd (List.assoc h continents))) t in
      let troops = n_continents 0 p.controls_cont + n_controls + n in
      { st with current_move = CReinforcement troops;
                log = "You have " ^ (string_of_int troops) ^ " to deploy."; }
    | _ -> { st with log = "Invalid move" })
  | AReinforcement (r, i) ->
    (match st.current_move with
    | CReinforcement n ->
      if n >= i then
        let p = List.hd st.players in
        (try
          let n = List.assoc r p.controls in
          let new_controls = List.remove_assoc r p.controls in
          let p' = { p with controls = (r, n + i)::new_controls } in
          let p_list = prepend_player p' st.players in
          { st with current_move = CReinforcement (n - i);
                    players = p_list;
                    log = "Successfully reinforced " ^ r ^ " with " ^
                          (string_of_int i) ^ " new troops." }
        with
        | Not_found -> { st with log = "You don't control that territory." })
      else { st with log = "You don't have enough troops. Try again." }
    | _ -> { st with log = "Invalid move"; })
  | _ -> failwith "TODO"
