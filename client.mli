
(*hang the client until a update is pushed from the server
 * Should probably have a timeout time in case connection is lost
*)
val wait_for_update : unit -> unit

(*send an action to the server*)
val send_action : actions -> unit

