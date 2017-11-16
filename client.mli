(* Client will contain the polling REPL; poll every 2 seconds and update gui *)

(*
 * Loop that polls the server every 2 seconds, receiving a JSON object
  representing the game state. If it is the user's turn, both a) enter
  play mode by activating GUI buttons allowing the user to take actions and
  b) check for any updates in the game state and call respective handlers in
 the GUI.
 *)
val polling_repl : unit -> unit

(* Take in an action from the GUI and send its information to the server in
  JSON format. *)
val send_action : action -> unit

(* [create_game req] starts up a game; it updates the game state with the
   information that the game is in progress; it returns a response containing
   the initial, empty map state. *)
val create_game : request -> response

(* [join_game req] takes in a JSON request containing a player username and
   updates the game state with the new user; it returns a response containing
   the new state of user info. *)
val join_game : request -> response

(** [move req] takes in a JSON request containing information about a player
    move: the type of action and the regions and number of troops involved *)
val move : request -> response

(* [quit_game req] updates the game state with the information that the game is
   no longer in progress; it returns a response containing the winner. *)
val quit_game : request -> response


(** Start up the client by prompting for a username and IP to connect to. Once
    provided, connect to the server using the provided IP and call the
    main function that runs the GUI. *)
val init_connection : unit -> unit
