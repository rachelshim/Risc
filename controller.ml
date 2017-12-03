open State
open Action

let update_gui st act =
  match act with
  | ADeployment reg -> () (* places one troop on region s *)
  | APlayCards (c1, c2, c3) ->
    update_cards (inf, cav, art, wild) (* the cards to trade in *)
  | AWaitReinforcement -> ()
  | AReinforcement reg num -> () (* reinforce region s with n troops *)
  | AAttack (reg1, reg2) -> () (* attack from region s1 to region s2 *)
  | AMovement ((reg1, reg2), num) -> ()
      (* move n troops from region s1 to region s2 *)
  | ANextTurn -> ()

let controller_update st act =
  let st' = update st in update_gui st' act

let init_game num =
  init_state num
