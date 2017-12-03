open State
open Action

(** Calls respective update function in GUI, passing along pertinent info
    depending on action type, as well as message to display in console log.
    Note: check for winner. *)
val update_gui : action -> unit

(* Take in an action from the GUI and a state and return an updated state. *)
val update : action -> state -> state

(* [init_game i] returns a new game state with the specified number of players.
 *)
val init_game : int -> state

(* GUI calls controller method with an update and state ref, and controller
   calls update method in the state, and returns an updated state. *)
