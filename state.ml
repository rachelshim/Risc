
(* Variant [card_type] represents the type of card *)
type card_type = Infantry | Cavalry | Artillery

(* [card] represents a card in risk, which is a tuple of territory name 
 * (as a string) and card type
 *)
type card = string * card_type

type player = {
  id: string;
  cards: card list;
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



