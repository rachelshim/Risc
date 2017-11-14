(*client will contain the polling REPL; poll every 2 seconds and update gui*)


(*hang the client until a update is pushed from the server
 * Should probably have a timeout time in case connection is lost
 Try serversent events possibly?
*)
val wait_for_update : unit -> unit

(*send an action to the server*)
val send_action : action -> unit

(*val init_connection : *)