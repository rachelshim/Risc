
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
  | ANew_Game of int (*creates new game with n players*)
  | AInitial_Reinforce of string (*places one troop on region s*)
  | APlay_Cards of card list (*play 3 cards in list*)
  | AReinforce of (string * int) list (*reinforce region s with n troops*)
  | AAttack of string * string (*attack from region s1 to region s2*)
  | AFortify of (string * string) * int
      (*move n troops from region s1 to region s2*)
  | ANext_Turn

type curr_move =
  | CNew_Game
  | CInitial_Reinforce
  | CReinforce of int (*reinforce with n total troops*)
  | CAttack
  | CFortify
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
  }

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
   ("Northern Europe", ["Great Britain"])
  ]

let rec first_n lst n =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h::t -> h::(first_n t (n-1))

let init_state p =
  let colors =
    first_n ["Red"; "Blue"; "Green"; "Yellow"; "Purple"; "Orange"] p in
  let players =
    List.map
      (fun name ->
         {
           id = name;
           cards = [];
           total_troops = 0;
           controls = [];
           continents = []
         })
      colors in
  {
    current_player = List.hd players;
    players = players;
    turns = 0;
    continents = [];
    bonus_troops = 0;
  }
