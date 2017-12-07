open OUnit2
open State
open Action

let two_player = init_state 2
let testmap = test_map


let tests =
[
  (* Init tests on two-player game *)
  "two_player_init" >:: (fun _ -> assert_equal (player_id (current_player two_player)) "Red");
  "tp_cards1" >::  (fun _ -> assert_equal ((two_player |> current_player |> num_inf) + (two_player |> current_player |> num_cav) + (two_player |> current_player |> num_art) + (two_player |> current_player |> num_wild)) 0);
  "tp_troops" >:: (fun _ -> assert_equal (avail_troops two_player) 19);
  "tp_regions" >:: (fun _ -> assert_equal
                               (List.fold_left
                                 (fun (p1, p2) r ->
                                   if ctrl_of_reg two_player r = "Red"
                                   then (p1 + 1, p2)
                                   else (p1, p2 + 1))
                                 (0, 0)
                                 (get_regions two_player)) (21, 21))


  (* Tests with testmap *)

]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite
