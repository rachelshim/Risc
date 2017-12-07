open OUnit2
open State
open Action

let two_player = init_state 2
let three_player = init_state 3
let four_player = init_state 4


let regions = get_regions two_player (* list of all regions in game *)

let update_deploy_alaska = update test_map (ADeployment "Alaska")
let tests =
[
  (* Init tests on two-player game *)
  "tp_init" >:: (fun _ -> assert_equal (rep_ok two_player) ());
  "tp_p1" >:: (fun _ -> assert_equal (player_id (current_player two_player)) "Red");
  "tp_cards1" >::  (fun _ -> assert_equal ((two_player |> current_player |> num_inf) + (two_player |> current_player |> num_cav) + (two_player |> current_player |> num_art) + (two_player |> current_player |> num_wild)) 0);
  "tp_cards2" >::  (fun _ -> assert_equal (
                      let p = player_of_id two_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "tp_troops" >:: (fun _ -> assert_equal (avail_troops two_player) 19);
  "tp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2) r ->
                          if ctrl_of_reg two_player r = "Red"
                          then (p1 + 1, p2)
                          else (p1, p2 + 1))
                        (0, 0) regions) (21, 21));
  "tp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in two_player r = 1)
                          true regions) true);
  (* Init tests on three-player game *)
  "trp_init" >:: (fun _ -> assert_equal (rep_ok three_player) ());
  "trp_p1" >:: (fun _ -> assert_equal (player_id (current_player three_player)) "Red");
  "trp_cards1" >::  (fun _ -> assert_equal (
                       let p = player_of_id three_player "Red" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_cards2" >::  (fun _ -> assert_equal (
                       let p = player_of_id three_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_cards3" >::  (fun _ -> assert_equal (
                        let p = player_of_id three_player "Green" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_troops" >:: (fun _ -> assert_equal (avail_troops three_player) 21);
  "trp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2, p3) r ->
                          if ctrl_of_reg three_player r = "Red"
                          then (p1 + 1, p2, p3)
                          else if ctrl_of_reg three_player r = "Blue"
                          then (p1, p2 + 1, p3)
                          else (p1, p2, p3 + 1)) (0, 0, 0)
                        regions) (14, 14, 14));
  "trp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in three_player r = 1)
                          true regions) true);
  (* Init tests on four-player game *)
  "fp_init" >:: (fun _ -> assert_equal (rep_ok four_player) ());
  "fp_p1" >:: (fun _ -> assert_equal (player_id (current_player four_player)) "Green");
  "fp_cards1" >::  (fun _ -> assert_equal (
                       let p = player_of_id four_player "Red" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards2" >::  (fun _ -> assert_equal (
                       let p = player_of_id four_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards3" >::  (fun _ -> assert_equal (
                        let p = player_of_id four_player "Green" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards4" >::  (fun _ -> assert_equal (
                        let p = player_of_id four_player "Yellow" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_troops" >:: (fun _ -> assert_equal (avail_troops four_player) 20);
  "fp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2, p3, p4) r ->
                          if ctrl_of_reg four_player r = "Red"
                          then (p1 + 1, p2, p3, p4)
                          else if ctrl_of_reg four_player r = "Blue"
                          then (p1, p2 + 1, p3, p4)
                          else if ctrl_of_reg four_player r = "Green"
                          then (p1, p2, p3 + 1, p4)
                          else(p1, p2, p3, p4 + 1)) (0, 0, 0, 0)
                        regions) (11, 11, 10, 10));
  "fp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in four_player r = 1)
                          true regions) true);

  (* Tests with test_map *)
  "deploy_test" >:: (fun _ -> assert_equal ((troops_in test_map "Alaska") + 1) (troops_in update_deploy_alaska "Alaska"));
  "num_controlled_test" >:: (fun _ -> assert_equal 21 (current_player test_map |> num_controlled));




]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite
