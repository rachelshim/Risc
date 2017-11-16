type region_id = string

(*Variant representing actions the user can take*)
type client_action =
    | APlace_Troops of (region_id * int) list
    | AAttack of region_id * region_id
    | AFinish_Attacking
    | AMove_Troops of (region_id * region_id) * int

(*
 * Loop that polls the server every 2 seconds, receiving a JSON object
  representing the game state. If it is the user's turn, both a) enter
  play mode by activating GUI buttons allowing the user to take actions and
  b) check for any updates in the game state and call respective handlers in
 the GUI.
 *)
val polling_repl : client_action -> unit

(* Take in an action from the GUI and send its information to the server in
  JSON format. *)
val send_action : client_action -> unit

(* [create_game req] starts up a game; it updates the game state with the
   information that the game is in progress; it returns a response containing
   the initial, empty map state. *)
val create_game : client_action -> unit

(* [join_game req] takes in a JSON request containing a player username and
   updates the game state with the new user; it returns a response containing
   the new state of user info. *)
val join_game : string * string -> unit

(** [move req] takes in a JSON request containing information about a player
    move: the type of action and the regions and number of troops involved *)
val move : client_action -> unit

(* [quit_game req] updates the game state with the information that the game is
   no longer in progress; it returns a response containing the winner. *)
val quit_game : client_action -> unit

(** Start up the client by prompting for a username and IP to connect to. Once
    provided, connect to the server using the provided IP and call the
    main function that runs the GUI. *)
val init_connection : client_action -> unit
