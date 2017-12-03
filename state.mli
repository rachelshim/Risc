
(* [player] represents a player in the game *)
type player

(* [state] represents the current state of the game *)
type state

(* [region] represents a region in Risk, which has fields for
 * its name, the number of troops on it, and the player that controls it.
 *)
type region

(* [continent] represents a continent in Risk, which has fields for its name,
 * the number of bonus troops its controller recieves, the list of regions
 * in the continent, as well as an option for the id of the player who controls
 * it.
 *)
type continent

(* [gamelog] keeps track of every action that has been taken in the game. *)
type gamelog

(* [action] defines an action a player can do in a turn. *)
type action

(* [create_player str] creates a new player with id [str]. *)
val create_player : string -> player

(* [init_state n] creates a new game with [n] the players.
 *)
val init_state : int -> state

(* [is_over s] returns [true] if the game has a winner, [false] otherwise
 *)
val is_over : state -> boolean

(* [update a s] updates the state in accordance with the action [a]. *)
val update : action -> state -> state

(* [valid_mode a s] returns [true] if the attempted move is valid. *)
val valid_mode : action -> state -> boolean
