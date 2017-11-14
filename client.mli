(*client will contain the polling REPL; poll every 2 seconds and update gui*)


(*
 * Two second loop that polls for updates every 2 seconds, updates the gui
 * and enters play mode when it is the current user's turn
 *)
val polling_repl : unit -> unit

(*send an action to the server*)
val send_action : action -> unit

(*val init_connection : *)
