type region_id = string

(*Variant representing actions the user can take*)
type client_action =
    | APlace_Troops of (region_id * int) list
    | AAttack of region_id * region_id
    | AFinish_Attacking
    | AMove_Troops of (region_id * region_id) * int

(* Polls the server and updates the gui based on the returned state data
 *)
val refresh : client_action -> unit

(* Take in an action from the GUI and send its information to the server in
  JSON format. *)
val send_action : client_action -> unit

(* [create_game req] starts a game if the one doesn't already exist on the 
 * server. Currently unused because server starts a game by default*)
(*val create_game : client_action -> unit*)

(* [join_game (url, username)] sends a request to he specified url asking to 
 * join a game in progress with the provided username.
 *)
val join_game : string * string -> unit

(** [move move_action] sends a request to the server to move units between
    territories. Server may reject this action if it is invalid.`*)
val move : client_action -> unit

(* [quit_game req] informs the server that the client is quitting and to end
    the game in progress. *)
val quit_game : client_action -> unit

(** Start up the client by prompting for a username and IP to connect to. Once
    provided, connect to the server using the provided IP and call the
    main function that runs the GUI. *)
val init_connection : client_action -> unit
