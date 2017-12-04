(*open Gui*)
open State
open Action

(** maybe do this  *)
(* let rec update_all_regions update_territories st lst =
  match lst with
  | [] -> ()
  | r::t -> update_territories
              [(r, player_id (current_player st), troops_in st r)];
              (update_all_regions update_territories st t) *)

let update_gui (st : state)
    ((write_log, update_territories, update_continent_owners,
     update_current_player, update_available_reinforcements, update_cards,
     update_territories_count, update_troop_count, set_game_over,
<<<<<<< Updated upstream
     run_blocking_popup):((string -> unit) * ((string * string * int) list -> unit) * ((string * string) list -> unit) * (string -> unit) * (int -> unit) * (int * int * int * int -> unit) * (int -> unit) * (int -> unit) * (bool -> unit) * (string -> unit))) (act : action) =
=======
      run_blocking_popup):
       ((string -> unit) * ((string * string * int) list -> unit) *
        ((string * string) list -> unit) * (string -> unit) * (int -> unit) *
        (int * int * int * int -> unit) * (int -> unit) * (int -> unit) *
        (bool -> unit) * (unit -> unit))) (act : action) =
>>>>>>> Stashed changes
  let pl = current_player st in
  match act with
  | ADeployment reg -> update_territories
                         [(reg, player_id pl, troops_in st reg)];
    update_available_reinforcements (avail_troops pl st);
    update_territories_count (num_controlled pl);
    (* TODO update_troop_count when implemented in state *)
  | APlayCards (c1, c2, c3) ->
    update_cards (num_inf pl, num_cav pl, num_art pl, num_wild pl);
    update_available_reinforcements (avail_troops pl st)
(** TODO update troops available based on state  *)
  | AReinforcement (reg, num) -> update_territories
                                   [(reg, player_id pl, troops_in st reg)];
    update_available_reinforcements (avail_troops pl st)
(* TODO update_troop_count when implemented in state *)
  | AAttack (reg1, reg2) -> ()
  | AMovement ((r1, r2), num) -> update_territories
                                   [(r1, player_id pl, troops_in st r1)];
    update_territories [(r2, player_id pl, troops_in st r2)];
    update_available_reinforcements (avail_troops pl st)
  | ANextTurn -> update_current_player (player_id pl);
    update_cards (num_inf pl, num_cav pl, num_art pl,
                  num_wild pl)

  | _ -> ()

let controller_update (st : state) (funcs:((string -> unit) * ((string * string * int) list -> unit) * ((string * string) list -> unit) * (string -> unit) * (int -> unit) * (int * int * int * int -> unit) * (int -> unit) * (int -> unit) * (bool -> unit) * (string -> unit))) (act : action) =
  let st' = update st act in
  let gui' = update_gui st' funcs act in
  st'

let init_game num =
  init_state num

let get_available_reinforcement st id =
  failwith "unimplemented"

let get_troops_in_territory st t =
  failwith "unimplemented"
