open Action

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
(* type continent *)

(* [gamelog] keeps track of every action that has been taken in the game. *)
(* type gamelog *)

(* [create_player str] creates a new player with id [str]. *)
(* val create_player : string -> player *)

(* [init_state n] creates a new game with [n] the players.
 *)
val init_state : int -> state

(* [is_over s] returns [true] if the game has a winner, [false] otherwise
 *)
val is_over : state -> bool

(* [update a s] updates the state in accordance with the action [a]. *)
val update : state -> action -> state

(* [valid_mode a s] returns [true] if the attempted move is valid. *)
val valid_mode : action -> state -> bool

(** [current_player st] returns the current player for a given state *)
val current_player : state -> player

(** [num_inf pl] returns the number of infantry cards pl has *)
val num_inf : player -> int

(** [num_cav pl] returns the number of cavalry cards pl has *)
val num_cav : player -> int

(** [num_art pl] returns the number of artillery cards pl has *)
val num_art : player -> int

(** [num_wild pl] returns the number of wildcards pl has *)
val num_wild : player -> int

(** [player_id pl] returns pl's id.  *)
val player_id : player -> string

(** [region_of_name r] returns the region with name r  *)
val region_of_name : state -> string -> region

(** [avail_troops st pl] returns the number of troops available for reinforce-
    ment for player pl in st. *)
val avail_troops : player -> state -> int

(** [troops_in st r] returns the number of troops currently in region r *)
val troops_in : state -> string -> int
