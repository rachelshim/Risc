open State
open Action

(** Calls respective update function in GUI, passing along pertinent info
    depending on action type, as well as message to display in console log.
    Note: check for winner. *)
(*val update_gui : state -> action -> unit*)

(* Take in a state, set of GUI functions, and action and return an updated
   state. *)
val controller_update : state ->
  (string -> unit) * ((string * string * int) list -> unit) *
  ((string * string) list -> unit) * (string -> unit) * (int -> unit) *
  (int * int * int * int -> unit) * (int -> unit) * (int -> unit) *
  (bool -> unit) * (string -> unit) ->
  action -> state

(* [init_game i] returns a new game state with the specified number of players.
 *)
val init_game : int -> state

(* GUI calls controller method with an update and state ref, and controller
   calls update method in the state, and returns an updated state. *)

(** [get_available_reinforcement id] returns the number of troops a player with
    the specified id has available for reinforcement. *)
val get_available_reinforcement : state -> int

(** [get_troops_in_territory id t] returns the number of troops in territory t.
*)
val get_troops_in_territory : state -> string -> int
