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

(** Start up the client by prompting for a username and IP to connect to. Once
    provided, connect to the server using the provided IP and call the
    main function that runs the GUI. *)
val init_connection : unit -> unit
