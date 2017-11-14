
(* [player] represents a player in the game *)
type player

(* [state] represents the current state of the game *)
type state

(* [create_player str] creates a new player with id [str]. *)
val create_player : string -> player

(* [init_state p] creates a new game with [p] as the inital player.
 *)
val init_state : player -> state

(* [add_player p s] adds player [p] to the game represented by [s] if the game 
 * has not started. If the game has already started, or [p] is already in the 
 * game, [s] is returned.
 *)
 val add_player : player -> state -> state

(* [get_turns s] returns the number of turns that have been played in the game
 *)
val get_turns : state -> int

(* [is_over s] returns [true] if the game has a winner, [false] otherwise
 *)
val is_over : state -> boolean

(* [has_started s] reuturns [true] if the game has started, and [false] 
 * otherwise (i.e., if a player can still be added to the game.)
 *)
val has_started : state -> boolean