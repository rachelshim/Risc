
(*
 * This MLI exposes methods for modifying the information displayed in the GUI
 * to be accessed by Controller
 *)

val write_log : string -> unit

(*territory * color/player * numtroops*)
val update_territories : (string * string * int) list -> unit

(*continent * color/player*)
val update_continent_owners : (string * string) list -> unit

val update_current_player : string -> unit

val update_available_reinforcements : int  -> unit

val update_cards : (int * int * int * int) -> unit

val update_territories_count : int -> unit

val update_troop_count : int -> unit

val set_game_over : bool -> unit