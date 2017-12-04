(*open Gui*)
open State
open Action

let update_gui (st : state) (write_log, update_territories, update_continent_owners, update_current_player, update_available_reinforcements, update_cards, update_territories_count, update_troop_count, set_game_over) (act : action) =
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

let controller_update (st : state) funcs (act : action) =
  let st' = update st act in
  let gui' = update_gui st' funcs act in
  st'

let init_game num =
  init_state num

let get_available_reinforcement st id =
  failwith "unimplemented"

let get_troops_in_territory st t =
  failwith "unimplemented"
