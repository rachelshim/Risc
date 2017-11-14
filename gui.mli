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

(*selects the region that is underneath the coordinates provided*)
val select : (int * int) -> game_map -> unit

(*requests an action be pushed to the server*)
val request_action : action -> unit

(*display error message string to the user*)
val print_rejection : string -> unit