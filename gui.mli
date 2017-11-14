
(*Updates the gui to reflect *)
val update : state -> unit

(*selects the region that is underneath the coordinates provided*)
val select : (int * int) -> game_map -> unit