type country = String

(*Variant representing actions the user can take*)
type actions =
  | APlace_Troops of (country * int) list
  | AAttack of country * country
  | AFinish_Attacking
  | AMove_Troops of (country * country) * int

type state

(*Updates the gui to reflect new state*)
val update : state -> unit

(*selects the region that is underneath the coordinates provided*)
val select : (int * int) -> game_map -> unit
