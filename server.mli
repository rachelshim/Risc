(** Server acts as a liaison between the client and state.  *)

(* [create_game req] starts up a game; it updates the game state with the
   information that the game is in progress; it returns a response containing
   the initial, empty map state. *)
val create_game : request -> response

(* [join_game req] takes in a JSON request containing a player username and
   updates the game state with the new user; it returns a response containing
   the new state of user info. *)
val join_game : request -> response

(** [move req] takes in a JSON request containing information about a player
    move: the type of action and the regions and number of troops involved.
    Move can be valid or invalid; it checks the state to see if the move is
    valid and includes an error in the response if so. *)
val move : request -> response

(* [quit_game req] updates the game state with the information that the game is
   no longer in progress; it returns a response containing the winner. *)
val quit_game : request -> response
