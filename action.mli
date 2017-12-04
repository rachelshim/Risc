(** Variant [card] represents the types of card that can be traded in *)
type card = Infantry | Cavalry | Artillery | Wild

(** Variant [action] represents the types of actions that can be taken as
    part of the game *)
type action =
  | ADeployment of string (* places one troop on region s *)
  | APlayCards of (card * card * card) (* trade in 3 cards of diff types  *)
  | AReinforcement of string * int (* reinforce region s with n troops *)
  | AAttack of (string * string) * int (* attack from region s1 to region s2 *)
  | AMovement of (string * string) * int
      (* move n troops from region s1 to region s2 *)
  | ANextTurn
