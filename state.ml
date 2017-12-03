
(* ############################################################################

  DEFINING TYPES

##############################################################################*)


(** Variant [card] represents the type of card *)
type card = Infantry | Cavalry | Artillery | Wild

(** [region] represents a region in Risk. *)
type region =
  {
    name: string;
    troops: int;
    routes: string list;
    continent: string;
  }

type player =
  {
    id: string;
    cards: card list;
    total_troops: int;
    controls: region list;
    continents: (string * int) list (*regions owned per continent*)
  }

type action =
  | ADeployment of string (*places one troop on region s*)
  | APlay_Cards of card list (*play 3 cards in list*)
  | AReinforcement of (string * int) list (*reinforce region s with n troops*)
  | AAttack of string * string (*attack from region s1 to region s2*)
  | AMovement of (string * string) * int
      (*move n troops from region s1 to region s2*)
  | ANext_Turn

type curr_move =
  | CNew_Game
  | CDeployment
  | CReinforcement of int (*reinforce with n total troops*)
  | CAttack
  | CMovement
  | CRecieve_Card of card
  | CNext_Turn
  | CGame_Won of string (*player s won the game*)

type state =
  {
    players: player list;
    turns: int;
    continents: (string * string option) list;
        (*continent s is controlled by player s_opt*)
    bonus_troops: int;
    log: string;
  }




(* ############################################################################

  Initial state stuff

##############################################################################*)



(**
 * List of continents. Structured as (String name, (int num territories in
 * continent, int troops given for continent control))
 *)
let continents =
  [("North America", (9, 5));("South America", (4, 2)); ("Europe", (7, 5));
   ("Africa", (6, 3)); ("Asia", (12, 7)); ("Australia", (4, 2))]

let init_regions =
  [{name = "Alaska";
    routes = ["Northwest Territory"; "Alberta"; "Kamchatka"];
    continent = "North America";
    troops = 1};
   {name = "Northwest Territory";
    routes = ["Alaska"; "Alberta"; "Ontario"; "Greenland"];
    continent = "North America";
    troops = 1};
   {name = "Alberta";
    routes =
      ["Alaska"; "Northwest Territory"; "Ontario"; "Western United States"];
    continent = "North America";
    troops = 1};
   {name = "Greenland";
    routes = ["Northwest Territory"; "Ontario"; "Quebec"; "Iceland"];
    continent = "North America";
    troops = 1};
   {name = "Ontario";
    routes =
      ["Alberta"; "Northwest Territory"; "Greenland"; "Quebec";
       "Eastern United States"; "Western US"];
    continent = "North America";
    troops = 1};
   {name = "Quebec";
    routes = ["Ontario"; "Greenland"; "Eastern United States"];
    continent = "North America";
    troops = 1};
   {name = "Western United States";
    routes = ["Alberta"; "Ontario"; "Eastern United States"; "Central America"];
    continent = "North America";
    troops = 1};
   {name = "Eastern United States";
    routes = ["Western United States"; "Ontario"; "Quebec"; "Central America"];
    continent = "North America";
    troops = 1};
   {name = "Central America";
    routes = ["Western United States"; "Eastern United States"; "Venezuela"];
    continent = "North America";
    troops = 1};
   {name = "Venezuela";
    routes = ["Peru"; "Brazil"];
    continent = "South America";
    troops = 1};
   {name = "Peru";
    routes = ["Venezuela"; "Brazil"; "Argentina"];
    continent = "South America";
    troops = 1};
   {name = "Argentina";
    routes = ["Peru"; "Brazil"];
    continent = "South America";
    troops = 1};
   {name = "Brazil";
    routes = ["Venezuela"; "Peru"; "Argentina"; "North Africa"];
    continent = "South America";
    troops = 1};
   {name = "Iceland";
    routes = ["Greenland"; "Great Britain"; "Scandinavia"];
    continent = "Europe";
    troops = 1};
   {name = "Great Britain";
    routes = ["Iceland"; "Western Europe"; "Scandinavia"; "Northern Europe"];
    continent = "Europe";
    troops = 1};
   {name = "Western Europe";
    routes =
      ["Great Britain"; "Northern Europe"; "Southern Europe"; "North Africa"];
    continent = "Europe";
    troops = 1};
   {name = "Scandinavia";
    routes = ["Iceland"; "Great Britain"; "Northern Europe"; "Ukraine"];
    continent = "Europe";
    troops = 1};
   {name = "Northern Europe";
    routes =
      ["Great Britain"; "Southern Europe"; "Western Europe"; "Scandinavia";
       "Ukraine"];
    continent = "Europe";
    troops = 1};
   {name = "Southern Europe";
    routes =
      ["Western Europe"; "North Africa"; "Egypt"; "Middle East"; "Ukraine"];
    continent = "Europe";
    troops = 1};
   {name = "Ukraine";
    routes =
      ["Scandinavia"; "Northern Europe"; "Southern Europe"; "Ural";
       "Afghanistan"; "Middle East"];
    continent = "Europe";
    troops = 1};
   {name = "North Africa";
    routes =
      ["Brazil"; "Western Europe"; "Southern Europe"; "Egypt"; "East Africa";
       "Congo"];
    continent = "Africa";
    troops = 1};
   {name = "Egypt";
    routes = ["North Africa"; "Southern Europe"; "Middle East"; "East Africa"];
    continent = "Africa";
    troops = 1};
   {name = "Congo";
    routes = ["North Africa"; "East Africa"; "South Africa"];
    continent = "Africa";
    troops = 1};
   {name = "East Africa";
    routes =
      ["North Africa"; "Egypt"; "Middle East"; "Madagascar"; "South Africa";
       "Congo"];
    continent = "Africa";
    troops = 1};
   {name = "South Africa";
    routes = ["Congo"; "East Africa"; "Madagascar"];
    continent = "Africa";
    troops = 1};
   {name = "Madagascar";
    routes = ["South Africa"; "East Africa"];
    continent = "Africa";
    troops = 1};
   {name = "Middle East";
    routes = ["East Africa"; "Egypt"; "Ukraine"; "Afghanistan"; "India"];
    continent = "Asia";
    troops = 1};
   {name = "Afghanistan";
    routes = ["Middle East"; "Ukraine"; "Ural"; "China"; "India"];
    continent = "Asia";
    troops = 1};
   {name = "Ural";
    routes = ["Siberia"; "China"; "Afghanistan"; "Ukraine"];
    continent = "Asia";
    troops = 1};
   {name = "Siberia";
    routes = ["Yakutsk"; "Irkutsk"; "Mongolia"; "China"; "Ural"];
    continent = "Asia";
    troops = 1};
   {name = "India";
    routes = ["Middle East"; "Afghanistan"; "China"; "Siam"];
    continent = "Asia";
    troops = 1};
   {name = "Siam";
    routes = ["India"; "China"; "Indonesia"];
    continent = "Asia";
    troops = 1};
   {name = "China";
    routes = ["Siam"; "India"; "Afghanistan"; "Ural"; "Siberia"; "Mongolia"];
    continent = "Asia";
    troops = 1};
   {name = "Mongolia";
    routes = ["China"; "Siberia"; "Irkutsk"; "Kamchatka"; "Japan"];
    continent = "Asia";
    troops = 1};
   {name = "Japan";
    routes = ["Mongolia"; "Kamchatka"];
    continent = "Asia";
    troops = 1};
   {name = "Kamchatka";
    routes = ["Mongolia"; "Japan"; "Alaska"; "Irkutsk"; "Yakutsk"];
    continent = "Asia";
    troops = 1};
   {name = "Irkutsk";
    routes = ["Ural"; "Yakutsk"; "Kamchatka"; "Mongolia"];
    continent = "Asia";
    troops = 1};
   {name = "Yakutsk";
    routes = ["Ural"; "Irkutsk"; "Kamchatka"];
    continent = "Asia";
    troops = 1};
   {name = "Indonesia";
    routes = ["Siam"; "New Guinea"; "Western Australia"];
    continent = "Australia";
    troops = 1};
   {name = "New Guinea";
    routes = ["Indonesia"; "Eastern Australia"; "Western Australia"];
    continent = "Australia";
    troops = 1};
   {name = "Western Australia";
    routes = ["Indonesia"; "New Guinea"; "Eastern Australia"];
    continent = "Australia";
    troops = 1};
   {name = "Eastern Australia";
    routes = ["Indonesia"; "New Guinea"; "Western Australia"];
    continent = "Australia";
    troops = 1}
  ]

let rec first_n lst n =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h::t -> h::(first_n t (n-1))

let rec add_regions rs ps =
  match rs with
  | [] -> ps
  | (_, h)::t ->
    let p = List.hd ps in
    let new_p =
      {p with
       total_troops = p.total_troops + 1;
       controls = h::p.controls;
       continents =
         let in_cont = List.assoc h.continent p.continents + 1 in
         List.remove_assoc h.continent p.continents |>
         List.cons (h.continent, in_cont)
      } in
    add_regions t ((List.tl ps) @ [new_p])

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
           continents = [("Asia", 0); ("Africa", 0); ("North America", 0);
                         ("South America", 0); ("Europe", 0); ("Australia", 0)]
         }) in
  let rand_init_regions =
    Random.self_init ();
    List.map (fun r -> (Random.float 5.0, r)) init_regions |>
    List.sort (fun c1 c2 -> compare (fst c1) (fst c2)) in
  {
    players = add_regions rand_init_regions players;
    turns = 0;
    continents = [];
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


let update st = function
  | ADeployment r ->
    let p = List.hd st.players in
      (try
        let n = List.assoc r p.controls in
        let new_controls = List.remove_assoc r p.controls in
        let p' = { p with controls = (r, n + 1)::new_controls } in
        let p_list = (fun (h::t) -> t @ [p']) st.players in
        { st with players = p_list;
                  log = "Successfuly reinforced " ^ r }
      with
      | Not_found -> { st with log = "Invalid move bitch"})
  | _ -> failwith "TODO"
