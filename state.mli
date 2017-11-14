
(* [player] represents a player in the game *)
type player

(* [state] represents the current state of the game *)
type state

(* [gamelog] keeps track of every action that has been taken in the game. *)
type gamelog

(* [action] defines an action a player can do in a turn. *)
type action

(* [create_player str] creates a new player with id [str]. *)
val create_player : string -> player

(* [init_state p] creates a new game with [p] as the players. The head of [p] 
 * is initialized as the current player, i.e. the first one to play
 *)
val init_state : player list -> state

(* [get_turns s] returns the number of turns that have been played in the game
 *)
val get_turns : state -> int

(* [is_over s] returns [true] if the game has a winner, [false] otherwise
 *)
val is_over : state -> boolean

(* [update a s] updates the state in accordance with the action [a]. *)
val update : action -> state -> state