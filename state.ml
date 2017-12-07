open Action
open Map
(* ############################################################################

  DEFINING TYPES

##############################################################################*)


(** [region] represents a region in Risk. *)
type region =
  {
    name: string;
    controller: string;
    troops: int;
    routes: string list;
    continent: string;
  }

type player =
  {
    id: string;
    cards: card list;
    total_troops: int;
    continent_regions: (string * int) list; (*regions owned per continent*)
    controls_cont: string list;
  }

type curr_move =
  | CDeployment of int (* final player has n troops left to place *)
  | CReinforcement of int (* reinforce with n total troops *)
  | CAttack
  | CRecieve_Card of card option
  | CGame_Won of string (* player s won the game *)

(** Map representing region, with key region name and value [region] *)
module Regions = Map.Make(String)

(** Represents the current game state. The head of the [players] list is the
    current player whose turn it is. *)
type state =
  {
    current_move: curr_move;
    players: player list;
    gets_card: bool;
    turns: int;
    continents: (string * string option) list;
        (* continent s is controlled by player s_opt *)
    regions: region Regions.t;
    bonus_troops: int;
    log: string;
  }


(* ############################################################################

   Constants

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

(**
 * List of all regions in the game. Should not be used except to assign
 * regions or create map [regions]*)
let init_regions =
  [{name = "Alaska";
    routes = ["Northwest Territory"; "Alberta"; "Kamchatka"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Northwest Territory";
    routes = ["Alaska"; "Alberta"; "Ontario"; "Greenland"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Alberta";
    routes =
      ["Alaska"; "Northwest Territory"; "Ontario"; "Western United States"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Greenland";
    routes = ["Northwest Territory"; "Ontario"; "Quebec"; "Iceland"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Ontario";
    routes =
      ["Alberta"; "Northwest Territory"; "Greenland"; "Quebec";
       "Eastern United States"; "Western United States"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Quebec";
    routes = ["Ontario"; "Greenland"; "Eastern United States"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Western United States";
    routes = ["Alberta"; "Ontario"; "Eastern United States"; "Central America"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Eastern United States";
    routes = ["Western United States"; "Ontario"; "Quebec"; "Central America"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Central America";
    routes = ["Western United States"; "Eastern United States"; "Venezuela"];
    continent = "North America";
    controller = "None";
    troops = 1};
   {name = "Venezuela";
    routes = ["Peru"; "Brazil"; "Central America"];
    continent = "South America";
    controller = "None";
    troops = 1};
   {name = "Peru";
    routes = ["Venezuela"; "Brazil"; "Argentina"];
    continent = "South America";
    controller = "None";
    troops = 1};
   {name = "Argentina";
    routes = ["Peru"; "Brazil"];
    continent = "South America";
    controller = "None";
    troops = 1};
   {name = "Brazil";
    routes = ["Venezuela"; "Peru"; "Argentina"; "North Africa"];
    continent = "South America";
    controller = "None";
    troops = 1};
   {name = "Iceland";
    routes = ["Greenland"; "Great Britain"; "Scandinavia"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Great Britain";
    routes = ["Iceland"; "Western Europe"; "Scandinavia"; "Northern Europe"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Western Europe";
    routes =
      ["Great Britain"; "Northern Europe"; "Southern Europe"; "North Africa"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Scandinavia";
    routes = ["Iceland"; "Great Britain"; "Northern Europe"; "Ukraine"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Northern Europe";
    routes =
      ["Great Britain"; "Southern Europe"; "Western Europe"; "Scandinavia";
       "Ukraine"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Southern Europe";
    routes =
      ["Western Europe"; "North Africa"; "Egypt"; "Middle East"; "Ukraine";
       "Northern Europe"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "Ukraine";
    routes =
      ["Scandinavia"; "Northern Europe"; "Southern Europe"; "Ural";
       "Afghanistan"; "Middle East"];
    continent = "Europe";
    controller = "None";
    troops = 1};
   {name = "North Africa";
    routes =
      ["Brazil"; "Western Europe"; "Southern Europe"; "Egypt"; "East Africa";
       "Congo"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "Egypt";
    routes = ["North Africa"; "Southern Europe"; "Middle East"; "East Africa"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "Congo";
    routes = ["North Africa"; "East Africa"; "South Africa"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "East Africa";
    routes =
      ["North Africa"; "Egypt"; "Middle East"; "Madagascar"; "South Africa";
       "Congo"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "South Africa";
    routes = ["Congo"; "East Africa"; "Madagascar"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "Madagascar";
    routes = ["South Africa"; "East Africa"];
    continent = "Africa";
    controller = "None";
    troops = 1};
   {name = "Middle East";
    routes = ["East Africa"; "Egypt"; "Ukraine"; "Afghanistan"; "India";
              "Southern Europe"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Afghanistan";
    routes = ["Middle East"; "Ukraine"; "Ural"; "China"; "India"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Ural";
    routes = ["Siberia"; "China"; "Afghanistan"; "Ukraine"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Siberia";
    routes = ["Yakutsk"; "Irkutsk"; "Mongolia"; "China"; "Ural"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "India";
    routes = ["Middle East"; "Afghanistan"; "China"; "Siam"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Siam";
    routes = ["India"; "China"; "Indonesia"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "China";
    routes = ["Siam"; "India"; "Afghanistan"; "Ural"; "Siberia"; "Mongolia"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Mongolia";
    routes = ["China"; "Siberia"; "Irkutsk"; "Kamchatka"; "Japan"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Japan";
    routes = ["Mongolia"; "Kamchatka"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Kamchatka";
    routes = ["Mongolia"; "Japan"; "Alaska"; "Irkutsk"; "Yakutsk"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Irkutsk";
    routes = ["Ural"; "Yakutsk"; "Kamchatka"; "Mongolia"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Yakutsk";
    routes = ["Ural"; "Irkutsk"; "Kamchatka"];
    continent = "Asia";
    controller = "None";
    troops = 1};
   {name = "Indonesia";
    routes = ["Siam"; "New Guinea"; "Western Australia"];
    continent = "Australia";
    controller = "None";
    troops = 1};
   {name = "New Guinea";
    routes = ["Indonesia"; "Eastern Australia"; "Western Australia"];
    continent = "Australia";
    controller = "None";
    troops = 1};
   {name = "Western Australia";
    routes = ["Indonesia"; "New Guinea"; "Eastern Australia"];
    continent = "Australia";
    controller = "None";
    troops = 1};
   {name = "Eastern Australia";
    routes = ["Indonesia"; "New Guinea"; "Western Australia"];
    continent = "Australia";
    controller = "None";
    troops = 1}
  ]

(** [Regions] with all regions in game stored*)
let init_regions_map =
  List.fold_left
    (fun map r -> Regions.add r.name r map) Regions.empty init_regions

(* ############################################################################

       Helpful functions for outside use

############################################################################# *)

let current_player st =
  List.hd st.players

let num_inf pl =
  List.length (List.find_all (fun x -> x = Infantry) pl.cards)

let num_cav pl =
  List.length (List.find_all (fun x -> x = Cavalry) pl.cards)

let num_art pl =
  List.length (List.find_all (fun x -> x = Artillery) pl.cards)

let num_wild pl =
  List.length (List.find_all (fun x -> x = Wild) pl.cards)

let player_id pl =
  pl.id

let avail_troops st =
  match st.current_move with
  | CDeployment i -> i
  | CReinforcement i -> i
  | _ -> 0 (** TODO handle this better *)

let player_of_id st id =
  List.hd ( List.filter (fun p -> p.id = id ) st.players )

let region_of_name st r =
  Regions.find r st.regions

let troops_in st r =
  (region_of_name st r).troops

let num_controlled pl =
  List.fold_left ( fun acc (str, n) -> acc + n ) 0 pl.continent_regions

let ctrl_of_reg st r =
  (region_of_name st r).controller

let owner_of_cont st c =
  match List.assoc c st.continents with
  | None -> "Grey"
  | Some owner -> owner

let cont_of_reg st r =
  (region_of_name st r).continent

let get_log st =
  st.log

let get_regions st =
  Regions.bindings st.regions |> List.map fst

(* ############################################################################

  Initial state stuff

##############################################################################*)

(**
 * [first_n lst n] is [lst] truncated to have maximum length [n]
 * requires: [n] >= 0.
*)
let rec first_n lst n =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h::t -> h::(first_n t (n-1))

(**
 * [add_regions ps map rs] is a tuple of [ps] with the regions from [rs]
 * distributed to each player (with other fields in player excluding
 * [controls_cont] updated accordingly). If some players have one more region
 * than other players, the result is rotated so the head is the first player
 * with one less region. The second element is [map] updated with each region
 * given its new controller.
 *)
let rec add_regions ps map rs =
  match rs with
  | [] -> (ps, map)
  | (_, h)::t ->
    let p = List.hd ps in
    let new_p =
      {p with
       total_troops = p.total_troops + 1;
       continent_regions =
         let in_cont = List.assoc h.continent p.continent_regions + 1 in
         List.remove_assoc h.continent p.continent_regions |>
         List.cons (h.continent, in_cont)
      } in
    let new_map = Regions.add h.name {h with controller = p.id} map in
    add_regions ((List.tl ps) @ [new_p]) new_map t

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
           continent_regions =
             [("Asia", 0); ("Africa", 0); ("North America", 0);
              ("South America", 0); ("Europe", 0); ("Australia", 0)];
           controls_cont = [];
         }) in
  let (players_w_regions, regions_map) =
    Random.self_init ();
    List.map (fun r -> (Random.float 5.0, r)) init_regions |>
    List.sort (fun c1 c2 -> compare (fst c1) (fst c2)) |>
    add_regions players init_regions_map in
  let players_w_continents =
    List.map
      (fun p -> {p with controls_cont = add_conts p.continent_regions})
      players_w_regions in
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
    current_move = CDeployment (50 - 5 * n - (42 / n));
    gets_card = false;
    turns = 0;
    continents = total_conts;
    regions = regions_map;
    bonus_troops = 4;
    log = "Game started! It is now " ^
          (List.hd players_w_continents).id ^ "'s turn to deploy troops."
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

(* [prepend_player p l] returns the tail of [l] with [p] as its new head.
 * to be used when a player's fields are updated, and the player list needs
 * to be updated accordingly  ** and ** it is still their turn.
 *)
let prepend_player p = function
  | [] -> [p]
  | h::t -> p::t

(* [append_player p l] returns the tail of [l] with [p] appended to it.
 * to be used when a player's field is updated and the player list needs
 * to be updated accordingly, and it is the end of the player's turn.
 *)
let append_player p = function
  | [] -> [p]
  | h::t -> t @ [p]


(* [remove_cards c l] removes card [c] from the list of cards [l]. *)
let rec remove_cards c l =
  match l with
  | [] -> raise Not_found
  | h::t -> if h = c then t
    else remove_cards c t

(* [get_player_reinforcments p] is the number of reinforcements given to [p] *)
let get_player_reinforcements p =
  List.fold_left
    (fun acc c -> acc + snd (List.assoc c continents))
    (num_controlled p / 3) p.controls_cont

(**
 * [give_player_region r st] is an updated [st] in which the current player
 * now controls region [r]. Fields within [r] are not changed, so it is a
 * precondition that [r]'s controller field is already updated. The player's
 * total_troops is also unchanged.
 *)
let give_player_region r st =
  let p = List.hd st.players in
  let troops_in_cont = List.assoc r.name p.continent_regions + 1 in
  let makes_continent =
    List.assoc r.continent continents |> fst = troops_in_cont in
  {st with
   regions = Regions.add r.name r st.regions;
   players =
     (let new_p =
       {p with
        continent_regions =
          List.remove_assoc r.name p.continent_regions |>
          List.cons (r.name, troops_in_cont);
        controls_cont =
          if makes_continent then r.continent::p.controls_cont
          else p.controls_cont} in
     prepend_player new_p st.players);
   continents =
     if makes_continent
     then
       List.remove_assoc r.continent st.continents |>
       List.cons (r.continent, Some p.id)
     else st.continents}

(** [get_player p_id] returns the player in input list with id [p_id] *)
let rec get_player p_id = function
  | [] -> failwith "precondition violation"
  | h::t -> if h.id = p_id then h else get_player p_id t

(** [remove_player p_id] removes the player in input list with id [p_id] *)
let rec remove_player p_id = function
  | [] -> failwith "precondition violation"
  | h::t -> if h.id = p_id then t else h::(remove_player p_id t)

(**
 * [replace_player new_p] removes the player in input list with id [new_p.id]
 * and replaces it with [new_p]
 *)
let rec replace_player new_p = function
  | [] -> failwith "precondition violation"
  | h::t -> if h.id = new_p.id then new_p::t else h::(replace_player new_p t)

let rec get_next_player_id default p_id = function
  | [] | _::[]-> default
  | _::h2::_ -> h2.id

let rec remove_from_list v = function
  | [] -> []
  | h::t -> if h = v then t else h::(remove_from_list v t)

(**
 * [transfer_region r st t] is an updated [st] where [r] is given to the current
 * player, and [t] troops are placed on [r]. total_troops is not changed for any
 * player.
 *)
let transfer_region r st t =
  let p_a = List.hd st.players in
  let p_d = get_player r.controller st.players in
  let new_r = {r with controller = p_a.id; troops = t} in
  let new_st =
    {st with
     regions = Regions.add new_r.name new_r st.regions;
     gets_card = true} |>
    give_player_region new_r in
  if num_controlled p_d = 1
  then
    let moved_cards = p_d.cards in
    let p_a' = List.hd new_st.players in
    let new_players =
      prepend_player {p_a' with cards = p_a'.cards @ moved_cards}
        new_st.players in
    {new_st with
     players = remove_player p_d.id new_players;
     log =
       p_a.id ^ " has taken " ^ r.name ^ " and eliminated" ^ p_d.id ^ "!" ^
       (if List.length moved_cards > 0
        then
          "They take " ^ p_d.id ^ "'s " ^
          string_of_int (List.length moved_cards) ^ " cards."
        else "")}
  else
    let curr_cont_reg = List.assoc r.continent p_d.continent_regions in
    let new_p_d =
      {p_d with
       controls_cont = remove_from_list r.continent p_d.controls_cont;
       continent_regions =
         List.remove_assoc r.continent p_d.continent_regions |>
         List.cons (r.continent, curr_cont_reg - 1)} in
    {new_st with
     players = replace_player new_p_d new_st.players;
     continents =
       List.remove_assoc r.continent new_st.continents |>
       List.cons (r.continent, None);
     log = p_a.id ^ " has taken " ^ r.name ^ "from " ^ p_d.id ^ "."}

(* helper function for testing dfs in utop delete later *)
let find_terr p st =
  Regions.bindings st.regions
  |> List.filter (fun (x, y) -> y.controller = p)
  |> List.map (fun (x, y) -> x)

let check_target p s1 s2 st =
  let r1 = Regions.find s1 st in
  let r2 = Regions.find s2 st in
  r1.name = r2.name && p = r1.controller && p = r2.controller

let check_controls p s st =
  let r = Regions.find s st in
  r.controller = p

let rec check_path p s1 s2 st =
  if not (check_controls p s1 st) then false
  else if check_target p s1 s2 st then true
  else
    let r1_routes = (Regions.find s1 st).routes in
    let rec search_helper visited = function
      | [] -> (false, visited)
      | h::t -> if List.mem h visited then search_helper visited t
                else begin
                  if check_target p h s2 st then (true, visited)
                  else if check_controls p h st then
                    let () = print_endline h in
                    search_helper (h::visited) (Regions.find h st).routes
                  else search_helper (h::visited) t
                end
                in
    let rec search visited = function
      | [] -> false
      | x::xs ->
        if List.mem x visited then search visited xs
        else begin
          if check_target p x s2 st
            then true
          else if check_controls p x st then
            let () = print_endline x in
            match search_helper (x::visited) (Regions.find x st).routes with
            | true, _ -> true
            | false, l -> search (x::(l @ visited)) xs
          else search (x::visited) xs
        end
        in
    search [s1] r1_routes

let rec update st a =
  match a, st.current_move with
  | ADeployment r, CDeployment n ->
    let p = List.hd st.players in
    let reg = Regions.find r st.regions in
    if reg.controller <> p.id
    then {st with log = "Invalid move: You don't control " ^ r ^ "." }
    else
      let p' = {p with total_troops = p.total_troops + 1} in
      let p_list = append_player p' st.players in
      {st with
       players = p_list;
       current_move =
         if (List.hd p_list).id = "Red"
         then
           if n = 1
           then CReinforcement (get_player_reinforcements (List.hd p_list))
           else CDeployment (n - 1)
         else CDeployment n;
       regions =
         Regions.add r {reg with troops = reg.troops + 1} st.regions;
       log =
         "Successfuly deployed to " ^ r ^ ". It is now " ^ (List.hd p_list).id
         ^ "'s turn."}
  | ADeployment _, _ ->
    {st with log = "Invalid move: cannot deploy at this time."}
  | APlayCards (c1, c2, c3), CReinforcement n ->
    let p = List.hd st.players in
    (* making sure card trade-in is valid *)
    if (c1 <> c2 && c2 <> c3 && c1 <> c3) ||
       (c1 = Wild && c2 = Wild) ||
       (c1 = Wild && c3 = Wild) ||
       (c2 = Wild && c3 = Wild) ||
       (c1 = c2 && c3 = Wild) ||
       (c1 = c3 && c2 = Wild) ||
       (c2 = c3 && c1 = Wild) ||
       (c1 = c2 && c2 = c3)
    then
      begin
        try
          let new_cards = remove_cards c1 p.cards
                          |> remove_cards c2 |> remove_cards c3 in
          let bonus_n = st.bonus_troops + n in
          let p' = { p with cards = new_cards } in
          let p_list = prepend_player p' st.players in
          { st with current_move = CReinforcement bonus_n;
                    players = p_list;
                    bonus_troops = increment_bonus bonus_n;
                    log = "Successfully traded in cards for " ^
                          (string_of_int bonus_n) ^ " extra troops"; }
        with Not_found ->
          {st with log = "Invalid move: you don't have those cards."}
      end
    else { st with log = "Invalid card trade-in" }
  | APlayCards _, CAttack ->
    if List.length (List.hd st.players).cards > 4
    then update {st with current_move = CReinforcement 0} a
    else {st with log = "Invalid move: cannot play cards at this time."}
  | APlayCards _, _ ->
    {st with log = "Invalid move: cannot play cards at this time."}
  | AReinforcement (r, i), CReinforcement n ->
    if n >= i then
      let p = List.hd st.players in
      let region = Regions.find r st.regions in
      if region.controller <> p.id
      then {st with log = "Invalid move: you don't control " ^ r ^ "." }
      else
        let p' = {p with total_troops = p.total_troops + i} in
        let p_list = prepend_player p' st.players in
        { st with current_move =
                    if n > i then CReinforcement (n - i)
                    else CAttack;
                  players = p_list;
                  regions =
                    Regions.add r {region with troops = region.troops + i}
                      st.regions;
                  log = "Successfully reinforced " ^ r ^ " with " ^
                        (string_of_int i) ^ " new troops." ^
                        (if n = i then " You may now attack." else "")}
    else { st with log = "You don't have enough troops. Try again." }
  | AReinforcement _, _ ->
    {st with log = "Invalid move: cannot reinforce at this time"}
  | AAttack ((r1_name, r2_name), t), CAttack ->
    let r1 = Regions.find r1_name st.regions in
    let r2 = Regions.find r2_name st.regions in
    let a = List.hd st.players in
    let d = get_player r2.controller st.players in
    if List.length a.cards > 4
    then {st with log = "Invalid move: you must play cards."}
    else if r1.controller <> a.id
    then {st with log = "Invalid move: you don't control " ^ r1_name ^ "."}
    else if r2.controller = a.id
    then {st with log = "Invalid move: you control " ^ r2_name ^ "."}
    else if r1.troops <= t
    then {st with log = "Invalid move: you don't have enough troops."}
    else
      let a_troops = min 3 t in
      let d_troops = min 2 r2.troops in
      let (a_lost_troops, d_lost_troops) =
        if min a_troops d_troops = 1
        then
          let a_max =
            Random.int 6
            |> max (if a_troops > 1 then (Random.int 6) else 0)
            |> max (if a_troops > 2 then (Random.int 6) else 0) in
          let b_max =
            Random.int 6
            |> max (if a_troops = 2 then (Random.int 6) else 0) in
          begin if a_max <= b_max then (1, 0) else (0, 1) end
        else
          let a_max =
            let fst_roll = Random.int 6 in
            let snd_roll = Random.int 6 in
            let first_two = (max fst_roll snd_roll, min fst_roll snd_roll) in
            if a_troops = 2 then first_two
            else
              let trd_roll = Random.int 6 in
              if trd_roll > fst first_two then (trd_roll, fst first_two)
              else if trd_roll > snd first_two then (fst first_two, trd_roll)
              else first_two in
          let b_max =
            let fst_roll = Random.int 6 in
            let snd_roll = Random.int 6 in
            (max fst_roll snd_roll, min fst_roll snd_roll) in
          let top = if fst a_max <= fst b_max then (1, 0) else (0, 1) in
          if snd a_max <= snd b_max
          then (fst top + 1, snd top)
          else (fst top, snd top + 1) in
    let new_a = {a with total_troops = a.total_troops - a_lost_troops} in
    let new_d = {d with total_troops = d.total_troops - d_lost_troops} in
    let new_r1 = {r1 with troops = r1.troops - a.total_troops} in
    let new_r2 =  {r2 with troops = r2.troops - d.total_troops} in
    if new_r2.troops = 0
    then
      let new_r1' = {new_r1 with troops = new_r1.troops - t} in
      let new_st =
        {st with
         regions = Regions.add r1_name new_r1' st.regions;
         players =
           replace_player new_a st.players |>
           replace_player new_d} in
      let final_st = transfer_region new_r2 new_st t in
      begin
        if List.length final_st.players = 1
        then {final_st with
              current_move = CGame_Won a.id;
              turns = final_st.turns + 1}
        else final_st
      end
    else
      {st with
       regions =
         Regions.add r1_name new_r1 st.regions |>
         Regions.add r2_name new_r2;
       players =
         replace_player new_a st.players |>
         replace_player new_d}
  | AAttack _, _ ->
    {st with log = "Invalid move: cannot attack at this time"}
  | AMovement ((s1, s2), n), CAttack -> 
    let p = List.hd st.players in
    let r1 = Regions.find s1 st.regions in
    let r2 = Regions.find s2 st.regions in
    if check_path p.id s1 s2 st.regions then
      if r1.troops <= n then
        { st with log = "Invalid move: you don't have enough troops"}
      else
        let r1' = { r1 with troops = r1.troops - n } in
        let r2' = { r2 with troops = r2.troops + n } in
        { st with regions = Regions.add s1 r1' st.regions |>
                            Regions.add s2 r2' }
    else
      { st with log = "Invalid move: try different regions."}
    (* add stuff for updating continent thing in player *)
  | AMovement _, _ -> 
    { st with log = "Invalid move: cannot move troops at this time" }
  | ANextTurn, CAttack ->
    if List.length (List.hd st.players).cards > 4
    then {st with log = "Invalid move: you must play cards."}
    else if st.gets_card
    then
      let card_val = Random.int 22 in
      let card =
        if card_val = 21
        then Wild
        else if card_val mod 3 = 0
        then Artillery
        else if card_val mod 3 = 1
        then Cavalry
        else Infantry in
      let p =
        {(List.hd st.players) with
         cards = card::(List.hd st.players).cards} in
      {st with
       players = prepend_player p st.players;
       current_move = CRecieve_Card (Some card)}
    else
    {st with
     current_move = CRecieve_Card None}
  | ANextTurn, CRecieve_Card _ ->
    let players = append_player (List.hd st.players) st.players in
    let new_troops = get_player_reinforcements (List.hd players) in
    {st with
     players = players;
     current_move =
       if new_troops = 0
       then CAttack
       else CReinforcement new_troops}
  | ANextTurn, CReinforcement _ ->
    {st with log = "Invalid move: must place all troops before ending turn."}
  | ANextTurn, _ ->
    {st with log = "Invalid move: cannot end turn at this time."}

let valid_mode a st =
  failwith "unimplemented"
