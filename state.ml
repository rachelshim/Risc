
(* ############################################################################

  DEFINING TYPES

##############################################################################*)


(* Variant [card] represents the type of card *)
type card = Infantry | Cavalry | Artillery | Wild

type player =
  {
    id: string;
    cards: card list;
    total_troops: int;
    controls: (string * int) list; (* name of the region, # of troops on it *)
    continents: (string * int) list (*regions owned per continent*)
  }

(* [region] represents a region in Risk, which has fields for
 * its name, the number of troops on it, the player that controls it, and
 * the other names of the other regions connected to it.
 *)
type region =
  {
    name: string;
    troops: int;
    controller: player;
    routes: string list;
  }

type action =
  | ADeployment of string (*places one troop on region s*)
  | APlay_Cards of card list (*play 3 cards in list*)
  | AMovement of (string * int) list (*reinforce region s with n troops*)
  | AAttack of string * string (*attack from region s1 to region s2*)
  | AReinforcement of (string * string) * int
      (*move n troops from region s1 to region s2*)
  | ANext_Turn

type curr_move =
  | CNew_Game
  | CDeployment
  | CMovement of int (*reinforce with n total troops*)
  | CAttack
  | CReinforcement
  | CRecieve_Card of card
  | CNext_Turn
  | CGame_Won of string (*player s won the game*)

type state =
  {
    current_player: player;
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
  [("Alaska", ["Northwest Territory"; "Alberta"; "Kamchatka"]);
   ("Northwest Territory", ["Alaska"; "Alberta"; "Ontario"; "Greenland"]);
   ("Alberta", ["Alaska"; "Northwest Territory"; "Ontario"; "Western United States"]);
   ("Greenland", ["Northwest Territory"; "Ontario"; "Quebec"; "Iceland"]);
   ("Ontario", ["Alberta"; "Northwest Territory"; "Greenland"; "Quebec"; "Eastern United States"; "Western US"]);
   ("Quebec", ["Ontario"; "Greenland"; "Eastern United States"]);
   ("Western United States", ["Alberta"; "Ontario"; "Eastern United States"; "Central America"]);
   ("Eastern United States", ["Western United States"; "Ontario"; "Quebec"; "Central America"]);
   ("Central America", ["Western United States"; "Eastern United States"; "Venezuela"]);
   ("Venezuela", ["Peru"; "Brazil"]);
   ("Peru", ["Venezuela"; "Brazil"; "Argentina"]);
   ("Argentina", ["Peru"; "Brazil"]);
   ("Brazil", ["Venezuela"; "Peru"; "Argentina"; "North Africa"]);
   ("Iceland", ["Greenland"; "Great Britain"; "Scandinavia"]);
   ("Great Britain", ["Iceland"; "Western Europe"; "Scandinavia"; "Northern Europe"]);
   ("Western Europe", ["Great Britain"; "Northern Europe"; "Southern Europe"; "North Africa"]);
   ("Scandinavia", ["Iceland"; "Great Britain"; "Northern Europe"; "Ukraine"]);
   ("Northern Europe", ["Great Britain"; "Southern Europe"; "Western Europe"; "Scandinavia"; "Ukraine"]);
   ("Southern Europe", ["Western Europe"; "North Africa"; "Egypt"; "Middle East"; "Ukraine"]);
   ("Ukraine", ["Scandinavia"; "Northern Europe"; "Southern Europe"; "Ural"; "Afghanistan"; "Middle East"]);
   ("North Africa", ["Brazil"; "Western Europe"; "Southern Europe"; "Egypt"; "East Africa"; "Congo"]);
   ("Egypt", ["North Africa"; "Southern Europe"; "Middle East"; "East Africa"]);
   ("Congo", ["North Africa"; "East Africa"; "South Africa"]);
   ("East Africa", ["North Africa"; "Egypt"; "Middle East"; "Madagascar"; "South Africa"; "Congo"]);
   ("South Africa", ["Congo"; "East Africa"; "Madagascar"]);
   ("Madagascar", ["South Africa"; "East Africa"]);
   ("Middle East", ["East Africa"; "Egypt"; "Ukraine"; "Afghanistan"; "India"]);
   ("Afghanistan", ["Middle East"; "Ukraine"; "Ural"; "China"; "India"])
  ]

let rec first_n lst n =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h::t -> h::(first_n t (n-1))

let init_state n =
  let colors =
    first_n ["Red"; "Blue"; "Green"; "Yellow"; "Purple"; "Orange"] n in
  let players =
    List.map
      (fun name ->
         {
           id = name;
           cards = [];
           total_troops = 0;
           controls = [];
           continents = [("Asia", 0); ("Africa", 0); ("North America", 0); 
                         ("South America", 0); ("Europe", 0); ("Australia", 0)]
         })
      colors in
  {
    current_player = List.hd players;
    players = players;
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
  | AInitial_Reinforce r -> let p = st.current_player in
                              (try
                                let n = List.assoc r p.controls in
                                let c = List.remove_assoc r p.controls in
                                let p' = { p with controls = (r, n + 1)::c } in
                                (* make sure to also update players list *)
                                { st with current_player = p';
                                          log = "Successfuly reinforced " ^ r }
                              with
                              | Not_found -> st)
  | _ -> failwith "TODO"
