
(*
 * This MLI exposes methods for modifying the information displayed in the GUI
 * to be accessed by Controller
 *)

(*
 * [write_log message] is a function with the side effect that any string that
 * it is called on is appended to the GUI's gameplay log.
 *)
val write_log : string -> unit

(*
 * [update_territories data] is a function with the side effect that the 
 * territory information specified in [data], which is a list of 3-tuples of the 
 * territory name, territory owner, and number of troops in the territory,
 * is used to set the widgets in the GUI.
 *)
val update_territories : (string * string * int) list -> unit

(*continent * color/player*)
(*
 * [update_continent_owners data] is a funcion with the side effect that the
 * list of continent name, continent owner tuples described by [data] are 
 * used to set the GUI continent ownership display.
 *)
val update_continent_owners : (string * string) list -> unit

(*
 * [update_current_player player] is a function with the side effect that it
 * sets the GUI's current player display to the player name given by [player].
 *)
val update_current_player : string -> unit

(*
 * [update_available_reinforcements num] is a function with the side effect that
 * it sets the displayed number of available reinforcements in the GUI to the
 * value specified by [num].
 *)
val update_available_reinforcements : int  -> unit

(*
 * [update_cards (inf, cav, art, wild)] is a function with the side effect that
 * it sets the number of available infantry, calvalry, artillery, and wildcards
 * to the values specified. 
 *)
val update_cards : (int * int * int * int) -> unit

(*
 * [update_territories_count count] is a function with the side effect that it 
 * sets the GUI's controlled territory count to the value given by [count].
 *)
val update_territories_count : int -> unit

(*
 * [update_troop_count count] is a function with the side effect that it 
 * sets the GUI's available unused troops count to the value given by [count].
 *)
val update_troop_count : int -> unit

(*
 * [set_game_over over] is a function with the side effect that it informs the 
 * GUI that the game is over.
 *)
val set_game_over : bool -> unit