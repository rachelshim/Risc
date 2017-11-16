open Map

(*Variant representing actions the user can take*)
type client_action =
  | APlace_Troops of (region_id * int) list
  | AAttack of region_id * region_id
  | AFinish_Attacking
  | AMove_Troops of (region_id * region_id) * int

type state

(*Updates the gui to reflect new state*)
val update : state -> unit

(* selects the region that is underneath the coordinates provided *)
val select : (int * int) -> game_map -> unit

(* requests an action be pushed to the server *)
val request_action : action -> unit

(* display error message string to the user *)
val print_rejection : string -> unit

(* draws the specified string over the requested region *)
val draw_string : region_id -> string -> game_map

(*resets the specified game map to its undrawn state *)
val reset_map_image : game_map -> game_map

(* display a popup window prompting the user to provide a username and web
 address *)
val prompt_credentials : unit -> string * string