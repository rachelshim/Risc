
(* Variant [card_type] represents the type of card *)
type card_type = Infantry | Cavalry | Artillery | Wild

(* [card] represents a card in risk, which is a tuple of territory name 
 * (as a string) and card type
 *)
type card = string * card_type

type player = 
  {
    id: string;
    cards: card list;
    total_troops: int;
    controlls: (string * int) list; (* name of the region, # of troops on it *)
  }

(* [region] represents a region in Risk, which has fields for
 * its name, the number of troops on it, and the player that controls it.
 *)
type region = 
  {
    name: string;
    troops: int;
    controller: player option;
  }

(* [continent] represents a continent in Risk, which has fields for its name,
 * the number of bonus troops its controller recieves, the list of regions
 * in the continent, as well as an option for the id of the player who controls
 * it.
 *)
type continent = 
  {
    name: string;
    controller: player option;
    bonus: int;
    regions: region list;
  }

type action = 
  | Reinforce of region * int
  | Attack of region * region
  | Fortify of (region * region) * int

type state = 
  {
    current_player: player;
    players: player list;
    turns: int;
    winner: player option;
    map: continent list;
  }

let create_player name = 
  {
    id = name;
    cards = [];
    total_troops = 0;
    controlls = [];
  }

let init_state p = 
  {
    current_player = List.head p;
    players = p;
    turns = 0;
    winner = None;
    started = false;
    map = [];
  }

let get_turns s = s.turns



