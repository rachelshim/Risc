
(* Variant [card_type] represents the type of card *)
type card_type = Infantry | Cavalry | Artillery

(* [card] represents a card in risk, which is a tuple of territory name 
 * (as a string) and card type
 *)
type card = string * card_type

(* [region] represents a region in Risk, which has fields for
 * its name, the number of troops on it, and the player's id that controls it.
 *)
type region = 
  {
    name: string;
    troops: int;
    controller: string;
  }

(* [continent] represents a continent in Risk, which has fields for its name,
 * the number of bonus troops its controller recieves, the list of regions
 * in the continent, as well as an option for the id of the player who controls
 * it.
 *)
type continent = 
  {
    name: string;
    controller: string option;
    bonus: int;
    regions: region list;
  }

type player = 
  {
    id: string;
    cards: card list;
    total_troops: int;
  }

type state = 
  {
    players: player list;
    turns: int;
    winner: player option;
    started: boolean;
  }

let create_player name = 
  {
    id = name;
    cards = [];
  }

let init_state p = 
  {
    players = [p];
    turns = 0;
    winner = None;
    started = false;
  }

let add_player p s = 
  {
    players = p::s.players;
    turns = s.turns;
    winner = s.winner;
    started = s.started;
  }

let get_turns s = s.turns

let has_started s = s.started



