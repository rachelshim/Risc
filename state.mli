open Action

(* [player] represents a player in the game *)
type player

(** [state] represents the current state of the game *)
type state

(** [region] represents a region in Risk, which has fields for
 * its name, the number of troops on it, and the player that controls it.
 *)
type region

(** [init_state n] creates a new game with [n] the players.
 *)
val init_state : int -> state

(* [update a s] updates the state in accordance with the action [a]. *)
val update : state -> action -> state

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
val avail_troops : state -> int

(** [troops_in st r] returns the number of troops currently in region r *)
val troops_in : state -> string -> int

(** [num_controlled pl] returns the number of regions controlled by pl  *)
val num_controlled : player -> int

(** [controller_of_reg st r] returns the player id that controls r in st. *)
val ctrl_of_reg : state -> string -> string

(** [owner_of_cont st c] returns the player id of the player that owns c in st*)
val owner_of_cont : state -> string -> string

(** [cont_of_reg st r] returns the continent region r is located in, in st*)
val cont_of_reg : state -> string -> string

(** [get_log st] returns the log message contained in st. *)
val get_log : state -> string

(** [get_regions st] returns a list of region names in the map *)
val get_regions : state -> string list

(** [receiving_card st] returns true if the user is receiving a card, false
otherwise. *)
val receiving_card : state -> bool

(** [num_troops_deployed pl] returns the number of troops pl has on the map  *)
val num_troops_deployed : player -> int

(** [ready_next_turn st] returns whether the current player is able to move
to the next turn. *)
val ready_next_turn : state -> bool

(**
 * Checks RIs of state, including that the number of troops on the board matches
 * the stored number for each player, that each continents-storing data
 * structure in state and each player is correct, and that the players that
 * control regions matches the players left in the game.
 *
 * If any of these do not hold, the program fails with a relevant message.
 *)
val rep_ok : state -> unit
