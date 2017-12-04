open Gui
open State
open Action

let update_gui (st : state) (act : action) =
  let pl = current_player st in
  match act with
  | ADeployment reg -> () (* places one troop on region s *)
  | APlayCards (c1, c2, c3) ->
    update_cards (num_inf pl, num_cav pl, num_art pl, num_wild pl)
(** TODO update troops available based on state  *)
  | AReinforcement (reg, num) -> () (* reinforce region s with n troops *)
  | AAttack (reg1, reg2) -> () (* attack from region s1 to region s2 *)
  | AMovement ((reg1, reg2), num) -> ()
      (* move n troops from region s1 to region s2 *)
  | ANextTurn -> update_current_player (player_id pl)
  | _ -> ()

let controller_update (st : state) (act : action) =
  let st' = update st act in
  let gui' = update_gui st' act in
  st'

let init_game num =
  init_state num
