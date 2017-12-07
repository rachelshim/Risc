(*open Gui*)
open State
open Action

let rec update_all_regions update_territories st lst =
  update_territories (List.map (fun r -> (r, ctrl_of_reg st r, troops_in st r))
                        lst)

let rec update_all_cont_owners update_continent_owners st lst =
  update_continent_owners (List.map (fun r ->
      let c = cont_of_reg st r in (c, owner_of_cont st c)) lst)

let update_gui (st : state)
    ((write_log, update_territories, update_continent_owners,
     update_current_player, update_available_reinforcements, update_cards,
     update_territories_count, update_troop_count, set_game_over,
      run_blocking_popup):
       ((string -> unit) * ((string * string * int) list -> unit) *
        ((string * string) list -> unit) * (string -> unit) * (int -> unit) *
        (int * int * int * int -> unit) * (int -> unit) * (int -> unit) *
        (bool -> unit) * (string -> unit))) (act : action) =
  let pl = current_player st in
  write_log (get_log st);
  match act with
  | ADeployment reg -> update_territories
                         [(reg, ctrl_of_reg st reg, troops_in st reg)];
    update_available_reinforcements (avail_troops st);
    update_territories_count (num_controlled pl);
    update_current_player (player_id pl)
    (* TODO update_troop_count when implemented in state *)
  | APlayCards (c1, c2, c3) ->
    update_cards (num_inf pl, num_cav pl, num_art pl, num_wild pl);
    update_available_reinforcements (avail_troops st)
(** TODO update troops available based on state  *)
  | AReinforcement (reg, num) -> update_territories
                                   [(reg, ctrl_of_reg st reg, troops_in st reg)];
    update_available_reinforcements (avail_troops st)
(* TODO update_troop_count when implemented in state *)
  | AAttack ((r1, r2), num) ->
    update_territories [(r1, ctrl_of_reg st r1, troops_in st r1)];
    update_territories [(r2, ctrl_of_reg st r2, troops_in st r2)];
    update_available_reinforcements (avail_troops st);
    update_continent_owners [(cont_of_reg st r1, owner_of_cont st r1);
                             (cont_of_reg st r2, owner_of_cont st r2)]
  | AMovement ((r1, r2), num) -> update_territories
                                   [(r1, ctrl_of_reg st r1, troops_in st r1)];
    update_territories [(r2, ctrl_of_reg st r2, troops_in st r2)];
    update_available_reinforcements (avail_troops st);
    run_blocking_popup "You have been awarded a card." (** TODO specify which card *)
  | ANextTurn -> update_cards (num_inf pl, num_cav pl, num_art pl, num_wild pl);
    run_blocking_popup
      "Your turn is over- please pass the computer to the next player. Huzzah!";
    update_current_player (player_id pl)
  | _ -> ()

let controller_update (st : state) (funcs:((string -> unit) * ((string * string * int) list -> unit) * ((string * string) list -> unit) * (string -> unit) * (int -> unit) * (int * int * int * int -> unit) * (int -> unit) * (int -> unit) * (bool -> unit) * (string -> unit))) (act : action) =
  let st' = update st act in
  let gui' = update_gui st' funcs act in
  st'

let init_game num ((write_log, update_territories, update_continent_owners,
                    update_current_player, update_available_reinforcements, update_cards,
                    update_territories_count, update_troop_count, set_game_over,
                    run_blocking_popup):((string -> unit) * ((string * string * int) list -> unit) * ((string * string) list -> unit) * (string -> unit) * (int -> unit) * (int * int * int * int -> unit) * (int -> unit) * (int -> unit) * (bool -> unit) * (string -> unit))) =
  let st = init_state num in
  let pl = current_player st in
  update_all_regions (update_territories) st (get_regions st);
  update_all_cont_owners (update_continent_owners) st (get_regions st);
  update_current_player (player_id pl);
  update_available_reinforcements (avail_troops st);
  update_cards (0,0,0,0);
  update_territories_count (num_controlled pl);
  (* TODO update_troop_count *)
  st

let init_state_emp num=
  init_state num

let get_available_reinforcement st =
  avail_troops st

let get_troops_in_territory st reg =
  troops_in st reg
